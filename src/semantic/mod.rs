#![allow(unused)]

pub mod symbol_table;

use std::collections::HashMap;

use crate::{
    parser::ast::{Decl, ExprKind, PointerKind, Program, StmtKind, TypeExpr},
    scanner::token::{LiteralTypes, Position, TokenType},
    semantic::symbol_table::{Symbol, SymbolKind, SymbolTable, TypeId},
};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    /// Signed-Integer types
    Int8,
    Int16,
    Int32,
    Int64,
    Int128,
    IntN,

    /// UnSigned-Integer types
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    UInt128,
    UIntN,

    /// Float types
    Float32,
    Float64,

    /// Other literal types
    String,
    Bool,
    Char,
    Unit,

    /// Others types
    Array(TypeId, Option<usize>),
    Function(Vec<TypeId>, TypeId),
    Enum,
    Struct(String, HashMap<String, TypeId>),
    Pointer(TypeId, PointerKind),
    Unknown,
}

#[derive(Debug, Default)]
pub struct TypeEnv {
    pub types: Vec<Type>,
}

impl TypeEnv {
    pub fn add(&mut self, t: Type) -> TypeId {
        let id = self.types.len();
        self.types.push(t);
        id
    }

    pub fn get(&self, id: TypeId) -> &Type {
        &self.types.get(id).expect("Invalid TypeId")
    }
}

#[derive(Debug, Clone)]
pub struct SemanticError {
    pub msg: String,
    pub pos: Position,
}

pub struct SemanticAnalyzer {
    pub symbols: SymbolTable,
    pub types: TypeEnv,
    pub errors: Vec<SemanticError>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer {
            symbols: SymbolTable::new(),
            types: TypeEnv::default(),
            errors: vec![],
        }
    }

    pub fn analyze(&mut self, program: &Program) -> &Vec<SemanticError> {
        for decl in program {
            self.check_decl(&decl.value);
        }
        &self.errors
    }

    fn check_expr(&mut self, expr: &ExprKind) -> TypeId {
        match expr {
            ExprKind::Literal(lit) => self.resolve_literal(lit),
            ExprKind::Identifier(name) => {
                if let Some(sym) = self.symbols.resolve(name) {
                    match &sym.kind {
                        SymbolKind::Variable { r#type, .. } => *r#type,
                        SymbolKind::Const { ty } => *ty,
                        SymbolKind::Func { .. } => self.types.add(Type::Unknown),
                        _ => self.types.add(Type::Unknown),
                    }
                } else {
                    self.errors.push(SemanticError {
                        msg: format!("Undefined identifier '{}'", name),
                        pos: Position::default(),
                    });
                    self.types.add(Type::Unknown)
                }
            }
            ExprKind::Binary { left, right, op } => {
                let l_ty = self.check_expr(&left.value);
                let r_ty = self.check_expr(&right.value);

                if !self.type_equals(l_ty, r_ty) {
                    if let Some(promoted) = self.coerce_numeric_types(l_ty, r_ty) {
                        return promoted;
                    } else {
                        self.errors.push(SemanticError {
                            msg: format!("Type mismatch in binary operation '{:?}'", op),
                            pos: left.pos.clone(),
                        });
                        return self.types.add(Type::Unknown);
                    }
                }

                match op {
                    TokenType::EqualEqual
                    | TokenType::BangEqual
                    | TokenType::Greater
                    | TokenType::Less
                    | TokenType::GreaterEqual
                    | TokenType::LessEqual
                    | TokenType::And
                    | TokenType::Or => self.types.add(Type::Bool),

                    _ => l_ty,
                }
            }
            ExprKind::Call { callee, arguments } => {
                if let ExprKind::Identifier(name) = &callee.value {
                    if let Some(sym) = self.symbols.resolve(name) {
                        if let SymbolKind::Func { params, ret } = &sym.kind {
                            // Create function type entry
                            let func_ty = self.types.add(Type::Function(params.clone(), *ret));
                            return func_ty;
                        }
                    }
                }

                let callee_ty = self.check_expr(&callee.value);
                let callee_type = self.types.get(callee_ty).clone();

                match callee_type {
                    Type::Function(param_types, ret_type) => {
                        let param_types = param_types.clone();
                        let ret_type = ret_type.clone();

                        if arguments.len() != param_types.len() {
                            self.errors.push(SemanticError {
                                msg: "Argument count mismatch".to_string(),
                                pos: callee.pos.clone(),
                            });
                        } else {
                            for (arg, param_ty) in arguments.iter().zip(param_types.iter()) {
                                let arg_ty = self.check_expr(&arg.value);
                                if !self.type_equals(arg_ty, *param_ty) {
                                    self.errors.push(SemanticError {
                                        msg: "Argument type mismatch".to_string(),
                                        pos: arg.pos.clone(),
                                    });
                                }
                            }
                        }
                        ret_type
                    }
                    _ => {
                        self.errors.push(SemanticError {
                            msg: "Callee is not a function".to_string(),
                            pos: callee.pos.clone(),
                        });
                        self.types.add(Type::Unknown)
                    }
                }
            }
            ExprKind::Member { object, field, .. } => {
                let obj_ty = self.check_expr(&object.value);
                match self.types.get(obj_ty) {
                    Type::Struct(_, fields) => {
                        if let Some(field_ty) = fields.get(field) {
                            *field_ty
                        } else {
                            self.errors.push(SemanticError {
                                msg: format!("Unknown field '{}'", field),
                                pos: object.pos.clone(),
                            });
                            self.types.add(Type::Unknown)
                        }
                    }
                    _ => {
                        self.errors.push(SemanticError {
                            msg: "Member access on non-struct".to_string(),
                            pos: object.pos.clone(),
                        });
                        self.types.add(Type::Unknown)
                    }
                }
            }

            _ => self.types.add(Type::Unknown),
        }
    }

    fn check_stmt(&mut self, stmt: &StmtKind) {
        match stmt {
            StmtKind::ExprStmt(expr) => {
                self.check_expr(&expr.value);
            }
            StmtKind::Return(Some(expr)) => {
                self.check_expr(&expr.value);
            }
            StmtKind::Let {
                name,
                r#type,
                initializer,
                mutable,
                ..
            } => {
                let decl_ty = r#type.as_ref().map(|t| self.resolve_type_expr(t));
                let init_ty = initializer.as_ref().map(|e| self.check_expr(&e.value));

                let final_ty = match (decl_ty, init_ty) {
                    (Some(a), Some(b)) if !self.type_equals(a, b) => {
                        self.errors.push(SemanticError {
                            msg: format!("Type mismatch in 'let {}'", name),
                            pos: initializer.as_ref().unwrap().pos.clone(),
                        });
                        a
                    }
                    (Some(a), _) => a,
                    (_, Some(b)) => b,
                    _ => self.types.add(Type::Unknown),
                };

                self.symbols
                    .define(
                        name.clone(),
                        Symbol {
                            name: name.clone(),
                            kind: SymbolKind::Variable {
                                mutable: *mutable,
                                r#type: final_ty,
                            },
                        },
                    )
                    .ok();
            }
            StmtKind::Block(stmts) => {
                self.symbols.enter_scope();
                for stmt in stmts {
                    self.check_stmt(&stmt.value);
                }
                self.symbols.exit_scope();
            }
            StmtKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_ty = self.check_expr(&condition.value);
                if !self.is_boolean(cond_ty) {
                    self.errors.push(SemanticError {
                        msg: "Condition must be boolean".to_string(),
                        pos: condition.pos.clone(),
                    });
                }
                self.check_stmt(&then_branch.value);
                if let Some(else_b) = else_branch {
                    self.check_stmt(&else_b.value);
                }
            }
            StmtKind::Assign { target, value } => {
                let value_ty = self.check_expr(&value.value);
                let target_ty = self.check_expr(&target.value);

                if let ExprKind::Identifier(name) = &target.value {
                    if let Some(sym) = self.symbols.resolve(name) {
                        match sym.kind {
                            SymbolKind::Variable { mutable, .. } if !mutable => {
                                self.errors.push(SemanticError {
                                    msg: format!("Cannot assign to immutable variable '{}'", name),
                                    pos: target.pos.clone(),
                                });
                            }
                            SymbolKind::Const { .. } => {
                                self.errors.push(SemanticError {
                                    msg: format!("Cannot reassign to constant '{}'", name),
                                    pos: target.pos.clone(),
                                });
                            }
                            _ => {}
                        }
                    }
                }

                if !self.type_equals(value_ty, target_ty) {
                    self.errors.push(SemanticError {
                        msg: "Type mismatch in assignment".to_string(),
                        pos: target.pos.clone(),
                    });
                }
            }
            StmtKind::For { iterator, iterable, body } => {
                let iter_ty = self.check_expr(&iterable.value);
                match self.types.get(iter_ty) {
                    Type::Array(elem_ty, _) => {
                        self.symbols.enter_scope();
                        self.symbols.define(
                            iterator.clone(),
                            Symbol {
                                name: iterator.clone(),
                                kind: SymbolKind::Variable {
                                    mutable: false,
                                    r#type: *elem_ty,
                                },
                            },
                        ).ok();
                        self.check_stmt(&body.value);
                        self.symbols.exit_scope();
                    }
                    Type::String => {
                        let char_ty = self.types.add(Type::Char);
                        self.symbols.enter_scope();
                        self.symbols.define(
                            iterator.clone(),
                            Symbol {
                                name: iterator.clone(),
                                kind: SymbolKind::Variable {
                                    mutable: false,
                                    r#type: char_ty,
                                },
                            },
                        ).ok();
                        self.check_stmt(&body.value);
                        self.symbols.exit_scope();
                    }
                    _ => self.errors.push(SemanticError {
                        msg: "For-loop iterable must be an array or string".to_string(),
                        pos: iterable.pos.clone(),
                    }),
                }
            }
            _ => {}
        }
    }

    fn check_decl(&mut self, decl: &Decl) {
        match decl {
            Decl::Import { path } => {
                // In future: load from module registry
                println!("Importing module: {:?}", path);
            }
            Decl::Const {
                name,
                r#type,
                value,
                ..
            } => {
                let ty = self.resolve_type_expr(r#type);
                let val_ty = self.check_expr(&value.value);

                if !self.type_equals(ty, val_ty) {
                    let ty_ref = self.types.get(ty);
                    let val_ref = self.types.get(val_ty);

                    let coercible = matches!(
                        (ty_ref, val_ref),
                        (Type::Float32, Type::Float64)
                            | (Type::Float64, Type::Float32)
                            | (Type::Int32, Type::Int64)
                            | (Type::Int64, Type::Int32)
                    );

                    if !coercible {
                        self.errors.push(SemanticError {
                            msg: format!("Type mismatch in const '{}'", name),
                            pos: value.pos,
                        });
                    }
                }
                self.symbols
                    .define(
                        name.clone(),
                        Symbol {
                            name: name.clone(),
                            kind: SymbolKind::Const { ty },
                        },
                    )
                    .ok();
            }
            Decl::Func(func) => {
                // param type
                let p_types = func
                    .params
                    .iter()
                    .map(|(_, t)| self.resolve_type_expr(t))
                    .collect();

                let ret_type = func
                    .return_type
                    .as_ref()
                    .map(|t| self.resolve_type_expr(t))
                    .unwrap_or_else(|| self.types.add(Type::Unit));

                self.symbols
                    .define(
                        func.name.clone(),
                        Symbol {
                            name: func.name.clone(),
                            kind: SymbolKind::Func {
                                params: p_types,
                                ret: ret_type,
                            },
                        },
                    )
                    .unwrap_or_else(|err| {
                        self.errors.push(SemanticError {
                            msg: err,
                            pos: Position::default(),
                        })
                    });

                self.symbols.enter_scope();
                for (param_name, param_ty) in &func.params {
                    let tid = self.resolve_type_expr(param_ty);
                    self.symbols
                        .define(
                            param_name.clone(),
                            Symbol {
                                name: param_name.clone(),
                                kind: SymbolKind::Variable {
                                    mutable: false,
                                    r#type: tid,
                                },
                            },
                        )
                        .ok();
                }

                for stmt in &func.body {
                    self.check_stmt(&stmt.value);
                    self.check_return_type(ret_type, &stmt.value);
                }

                if !matches!(self.types.get(ret_type), Type::Unit) {
                    let mut has_return = false;
                    for stmt in &func.body {
                        if let StmtKind::Return(_) = stmt.value {
                            has_return = true;
                            break;
                        }
                    }

                    if !has_return {
                        self.errors.push(SemanticError {
                            msg: format!("Function '{}' missing return statement", func.name),
                            pos: Position::default(),
                        });
                    }
                }

                self.symbols.exit_scope();
            }
            Decl::Struct { name, fields, .. } => {
                let mut field_map = HashMap::new();
                for (fname, fty) in fields {
                    field_map.insert(fname.clone(), self.resolve_type_expr(fty));
                }

                self.types.add(Type::Struct(name.clone(), field_map));
                self.symbols
                    .define(
                        name.clone(),
                        Symbol {
                            name: name.clone(),
                            kind: SymbolKind::Struct {
                                fields: HashMap::new(),
                            },
                        },
                    )
                    .ok();
            }
            _ => {}
        }
    }

    fn coerce_numeric_types(&mut self, a: TypeId, b: TypeId) -> Option<TypeId> {
        use Type::*;
        match (self.types.get(a), self.types.get(b)) {
            // If both are numeric, promote to the “wider” type
            (Int32, Float64) | (Float64, Int32) => Some(self.types.add(Float64)),
            (Int32, Float32) | (Float32, Int32) => Some(self.types.add(Float32)),
            (Int64, Float32) | (Float32, Int64) => Some(self.types.add(Float64)),
            (Int64, Float64) | (Float64, Int64) => Some(self.types.add(Float64)),
            (Int32, Int64) | (Int64, Int32) => Some(self.types.add(Int64)),
            (Float32, Float64) | (Float64, Float32) => Some(self.types.add(Float64)),
            _ => None,
        }
    }

    fn resolve_type_expr(&mut self, expr: &TypeExpr) -> TypeId {
        match expr {
            TypeExpr::Named(name) => match name.as_str() {
                "int8" => self.types.add(Type::Int8),
                "int16" => self.types.add(Type::Int16),
                "int32" => self.types.add(Type::Int32),
                "int64" => self.types.add(Type::Int64),
                "int128" => self.types.add(Type::Int128),
                "int_n" => self.types.add(Type::IntN),
                "uint8" => self.types.add(Type::UInt8),
                "uint16" => self.types.add(Type::UInt16),
                "uint32" => self.types.add(Type::UInt32),
                "uint64" => self.types.add(Type::UInt64),
                "uint128" => self.types.add(Type::UInt128),
                "uint_n" => self.types.add(Type::UIntN),
                "float32" => self.types.add(Type::Float32),
                "float64" => self.types.add(Type::Float64),
                "bool" => self.types.add(Type::Bool),
                "char" => self.types.add(Type::String),
                "string" => self.types.add(Type::String),
                _ => self.types.add(Type::Unknown),
            },
            TypeExpr::Array { element, size } => {
                let elem_ty = self.resolve_type_expr(element);
                self.types.add(Type::Array(elem_ty, *size))
            }
            TypeExpr::Pointer { target, mutable } => {
                let targ_ty = self.resolve_type_expr(target);
                self.types.add(Type::Pointer(targ_ty, mutable.clone()))
            }
            _ => self.types.add(Type::Unknown),
        }
    }

    fn resolve_literal(&mut self, lit: &LiteralTypes) -> TypeId {
        match lit {
            LiteralTypes::Int(_) => self.types.add(Type::Int32), // default integer type
            LiteralTypes::Float(_) => self.types.add(Type::Float64),
            LiteralTypes::Bool(_) => self.types.add(Type::Bool),
            LiteralTypes::String(_) => self.types.add(Type::String),
            LiteralTypes::Char(_) => self.types.add(Type::Char),
        }
    }

    fn check_return_type(&mut self, expected: TypeId, stmt: &StmtKind) {
        if let StmtKind::Return(Some(expr)) = stmt {
            let actual = self.check_expr(&expr.value);
            if !self.type_equals(expected, actual)
                && self.coerce_numeric_types(expected, actual).is_none()
            {
                self.errors.push(SemanticError {
                    msg: "Return type mismatch".to_string(),
                    pos: expr.pos.clone(),
                });
            }
        }
    }

    fn type_equals(&self, a: TypeId, b: TypeId) -> bool {
        self.types.get(a) == self.types.get(b)
    }

    fn is_boolean(&self, t: TypeId) -> bool {
        matches!(self.types.get(t), Type::Bool)
    }
}
