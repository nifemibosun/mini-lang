#![allow(unused)]

use std::collections::HashMap;

pub mod symbol_table;
use crate::parser::ast;
use crate::scanner::token;

#[derive(Debug, Clone)]
struct CurrFuncBlock {
    current_func_name: String,
    curr_ret_type: symbol_table::Type,
}

#[derive(Debug, Clone)]
pub struct SemanticAnalyzer<'a> {
    ast: &'a ast::Program,
    curr_func_block: Option<CurrFuncBlock>,
    pub symbols: symbol_table::SymbolTable,
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(ast: &'a ast::Program) -> Self {
        SemanticAnalyzer {
            ast,
            curr_func_block: None,
            symbols: symbol_table::SymbolTable::new(),
        }
    }

    pub fn analyze_program(&mut self) -> symbol_table::SymbolTable {
        for decl in self.ast.clone() {
            self.analyze_decl(decl.value).unwrap();
        }

        std::mem::take(&mut self.symbols)
    }

    fn analyze_expr(&mut self, expr: ast::Expr) -> Result<symbol_table::Type, String> {
        match expr.value {
            ast::ExprKind::Literal(lit) => match lit {
                token::LiteralTypes::Int(_) => Ok(symbol_table::Type::Int32),
                token::LiteralTypes::Float(_) => Ok(symbol_table::Type::Float64),
                token::LiteralTypes::String(_) => Ok(symbol_table::Type::String),
                token::LiteralTypes::Bool(_) => Ok(symbol_table::Type::Bool),
                token::LiteralTypes::Char(_) => Ok(symbol_table::Type::Char),
                _ => Err("Unknown luteral type".to_string()),
            },
            ast::ExprKind::Identifier(name) => {
                if let Some(symbol) = self.symbols.resolve(&name) {
                    match &symbol.kind {
                        symbol_table::SymbolKind::Variable { var_type, .. } => Ok(var_type.clone()),
                        _ => Err(format!("'{}' is not a variable", &name)),
                    }
                } else {
                    Err(format!("Undefined variable: {}", &name))
                }
            }
            ast::ExprKind::Unary { op, right } => Ok(self.analyze_unary_expr(op, *right)?),
            ast::ExprKind::Binary { left, op, right } => {
                Ok(self.analyze_binary_expr(*left, op, *right)?)
            }
            ast::ExprKind::Grouping(inner) => Ok(self.analyze_expr(*inner)?),
            ast::ExprKind::Call { callee, arguments } => {
                Ok(self.analyze_call_expr(*callee, arguments)?)
            }
            _ => Err(format!("Unknown expression kind: {:#?}", expr.value)),
        }
    }

    fn analyze_unary_expr(
        &mut self,
        op: token::TokenType,
        right: ast::Expr,
    ) -> Result<symbol_table::Type, String> {
        let right_type = self.analyze_expr(right)?;

        match op {
            token::TokenType::Minus => match right_type {
                symbol_table::Type::Int8
                | symbol_table::Type::Int16
                | symbol_table::Type::Int32
                | symbol_table::Type::Int64
                | symbol_table::Type::Int128
                | symbol_table::Type::IntN
                | symbol_table::Type::UInt8
                | symbol_table::Type::UInt16
                | symbol_table::Type::UInt32
                | symbol_table::Type::UInt64
                | symbol_table::Type::UInt128
                | symbol_table::Type::UIntN
                | symbol_table::Type::Float32
                | symbol_table::Type::Float64 => Ok(right_type),
                _ => Err(format!("Cannot use '-' on type {:#?}", right_type)),
            },
            token::TokenType::Bang => {
                if right_type == symbol_table::Type::Bool {
                    Ok(symbol_table::Type::Bool)
                } else {
                    Err(format!("Cannot use '!' on type {:#?}", right_type))
                }
            }
            _ => Err(format!("Unknown unary operator {:#?}", op)),
        }
    }

    fn analyze_binary_expr(
        &mut self,
        left: ast::Expr,
        op: token::TokenType,
        right: ast::Expr,
    ) -> Result<symbol_table::Type, String> {
        let left_type = self.analyze_expr(left)?;
        let right_type = self.analyze_expr(right)?;

        if left_type != right_type {
            return Err(format!(
                "Type mismatch in binary expression. Left side is {:?}, but Right side is {:?}.",
                left_type, right_type
            ));
        }

        match op {
            token::TokenType::Plus
            | token::TokenType::Minus
            | token::TokenType::Star
            | token::TokenType::Slash
            | token::TokenType::Mod => {
                if !left_type.is_numeric() && left_type != symbol_table::Type::String {
                    return Err(format!("Cannot perform arithmetic on type {:?}", left_type));
                }
                Ok(left_type)
            }

            token::TokenType::Greater
            | token::TokenType::GreaterEqual
            | token::TokenType::Less
            | token::TokenType::LessEqual
            | token::TokenType::EqualEqual
            | token::TokenType::BangEqual => Ok(symbol_table::Type::Bool),

            token::TokenType::And | token::TokenType::Or => {
                if left_type != symbol_table::Type::Bool {
                    return Err(format!(
                        "Logical operators require Bool, found {:?}",
                        left_type
                    ));
                }
                Ok(symbol_table::Type::Bool)
            }

            _ => Err(format!("Unknown or unsupported binary operator: {:?}", op)),
        }
    }

    fn analyze_call_expr(
        &mut self,
        callee: ast::Expr,
        arguments: Vec<ast::Expr>,
    ) -> Result<symbol_table::Type, String> {
        let func_name = match callee.value {
            ast::ExprKind::Identifier(name) => name,
            _ => return Err("Call must be an identifier".to_string()),
        };

        let (params, return_type) = if let Some(symbol) = self.symbols.resolve(&func_name) {
            match &symbol.kind {
                symbol_table::SymbolKind::FuncDecl {
                    params,
                    return_type,
                } => (params.clone(), return_type.clone()),
                _ => return Err(format!("'{}' is defined but is not a function", func_name)),
            }
        } else {
            return Err(format!("Undefined function: '{}'", func_name));
        };

        if arguments.len() != params.len() {
            return Err(format!(
                "Function '{}' expects {} arguments, but got {}.",
                func_name,
                params.len(),
                arguments.len()
            ));
        }

        for (i, arg_expr) in arguments.into_iter().enumerate() {
            let arg_type = self.analyze_expr(arg_expr)?;
            let expected_type = &params[i].1;

            if &arg_type != expected_type {
                return Err(format!(
                    "Type mismatch at argument {}. Function '{}' expects {:?}, but got {:?}",
                    i + 1,
                    func_name,
                    expected_type,
                    arg_type
                ));
            }
        }

        Ok(return_type)
    }

    fn analyze_stmt(&mut self, stmt: ast::Stmt) -> Result<(), String> {
        match stmt.value {
            ast::StmtKind::ExprStmt(expr) => {
                self.analyze_expr(expr)?;
                Ok(())
            }
            ast::StmtKind::Let {
                name,
                mutable,
                r#type,
                initializer,
            } => Ok(self.analyze_let_stmt(name, r#type.unwrap(), mutable, initializer.unwrap())),
            ast::StmtKind::ConstStmt {
                name,
                r#type,
                value,
                ..
            } => Ok(self.analyze_const(name, r#type, value)),
            ast::StmtKind::Assign {
                target,
                operator,
                value,
            } => Ok(self.analyze_assign_stmt(target, value)?),
            ast::StmtKind::Return(ret_expr) => Ok(self.analyze_return_stmt(ret_expr)?),
            ast::StmtKind::Block(body) => {
                self.analyze_block_stmt(body);
                Ok(())
            }
            ast::StmtKind::If {
                condition,
                then_branch,
                else_branch,
            } => Ok(self.analyze_if_stmt(condition, then_branch, else_branch)?),
            ast::StmtKind::While { condition, body } => {
                Ok(self.analyze_while_stmt(condition, body)?)
            }
            ast::StmtKind::For {
                iterator,
                iterable,
                body,
            } => Ok(()),
            ast::StmtKind::Match { expr, arms } => Ok(()),
            _ => Err(format!("Unknown statement kind: {:?}", stmt.value)),
        }
    }

    #[inline]
    fn analyze_let_stmt(
        &mut self,
        name: String,
        ty: ast::TypeExpr,
        mutable: bool,
        value: ast::Node<ast::ExprKind>,
    ) {
        self.analyze_var(name, ty, mutable, value);
    }

    fn analyze_assign_stmt(&mut self, target: ast::Expr, value: ast::Expr) -> Result<(), String> {
        let name = match target.value {
            ast::ExprKind::Identifier(n) => n,
            _ => return Err("Assignment target must be an identifier".to_string()),
        };

        let (is_mut, target_type) = if let Some(symbol) = self.symbols.resolve(&name) {
            match &symbol.kind {
                symbol_table::SymbolKind::Variable {
                    mutable, var_type, ..
                } => (*mutable, var_type.clone()),
                _ => {
                    return Err(format!(
                        "'{}' is not a variable and cannot be assigned to",
                        name
                    ));
                }
            }
        } else {
            return Err(format!("Undefined variable '{}'", name));
        };

        if !is_mut {
            return Err(format!("Cannot assign to immutable variable '{}'", name));
        }

        let val_type = self.analyze_expr(value)?;
        if target_type != val_type {
            return Err(format!(
                "Type mismatch in assignment. Variable '{}' expects {:?}, but got {:?}",
                name, target_type, val_type
            ));
        }

        Ok(())
    }

    fn analyze_return_stmt(&mut self, ret_expr: Option<ast::Expr>) -> Result<(), String> {
        let expected = self
            .curr_func_block
            .clone()
            .expect("Unexpected return statement");

        if let Some(expr) = ret_expr {
            let ret_val_type = self
                .convert_expr_to_val_type(expr, expected.curr_ret_type.clone())?;
            let ret_type = symbol_table::Value::new(ret_val_type).val_type;

            if expected.curr_ret_type != ret_type {
                println!(
                    "Semantic Error: Expected return type {:?}, found {:?}",
                    expected.curr_ret_type, ret_type
                );
            }
        }

        let name = expected.current_func_name;

        let symbol = symbol_table::Symbol {
            name: name.clone(),
            kind: symbol_table::SymbolKind::Return {
                return_type: expected.curr_ret_type,
            },
        };

        if let Err(e) = self.symbols.define(&name, symbol) {
            return Err(format!("Semantic Error: {}", e));
        }

        Ok(())
    }

    fn analyze_block_stmt(&mut self, body: Vec<ast::Stmt>) -> Result<(), String> {
        self.symbols.enter_scope();

        for stmt in body {
            self.analyze_stmt(stmt)?;
        }

        if let Err(e) = self.symbols.exit_scope() {
            return Err(format!("Semantic Error: {}", e));
        }

        Ok(())
    }

    fn analyze_if_stmt(
        &mut self,
        condition: ast::Expr,
        then_branch: Box<ast::Stmt>,
        else_branch: Option<Box<ast::Stmt>>,
    ) -> Result<(), String> {
        let cond_type = self.analyze_expr(condition)?;

        if cond_type != symbol_table::Type::Bool {
            return Err(format!(
                "If condition must be a boolean, found {:?}",
                cond_type
            ));
        }

        self.analyze_stmt(*then_branch)?;

        if let Some(else_stmt) = else_branch {
            self.analyze_stmt(*else_stmt)?;
        }

        Ok(())
    }

    fn analyze_while_stmt(
        &mut self,
        condition: ast::Expr,
        body: Box<ast::Stmt>,
    ) -> Result<(), String> {
        let cond_type = self.analyze_expr(condition)?;

        if cond_type != symbol_table::Type::Bool {
            return Err(format!(
                "While condition must be a boolean, found {:?}",
                cond_type
            ));
        }

        self.analyze_stmt(*body)?;
        Ok(())
    }

    fn analyze_var(
        &mut self,
        name: String,
        ty: ast::TypeExpr,
        mutable: bool,
        value: ast::Node<ast::ExprKind>,
    ) -> Result<(), String> {
        let var_type = self.convert_type_expr_to_type(ty)?;
        let rhs_expr = ast::Expr {
            value: value.value.clone(),
            pos: value.pos,
        };

        let rhs_type = match self.analyze_expr(rhs_expr.clone()) {
            Ok(t) => t,
            Err(e) => {
                return Err(format!("Semantic Error: {}", e));
            }
        };

        if var_type != rhs_type {
            let err = symbol_table::SymbolTableError::TypeMismatch {
                name: name.clone(),
                expected: var_type.clone(),
                found: rhs_type,
            };
        }

        let symbol_value = match &rhs_expr.value {
            ast::ExprKind::Literal(_) => {
                match self.convert_expr_to_val_type(rhs_expr, var_type.clone()) {
                    Ok(val) => Some(symbol_table::Value::new(val)),
                    Err(e) => {
                        format!("Semantic Error: converting value {}", e);
                        None
                    }
                }
            }
            _ => None,
        };

        let symbol = symbol_table::Symbol {
            name: name.clone(),
            kind: symbol_table::SymbolKind::Variable {
                var_type: var_type.clone(),
                value: symbol_value,
                mutable,
            },
        };

        if let Err(e) = self.symbols.define(&name, symbol) {
            return Err(format!("Semantic Error: {}", e));
        }

        Ok(())
    }

    fn analyze_decl(&mut self, decl: ast::Decl) -> Result<(), String> {
        match decl {
            ast::Decl::Import { path } => Ok(()),
            ast::Decl::ConstDecl {
                name,
                r#type,
                value,
                ..
            } => Ok(self.analyze_const(name, r#type, value)),
            ast::Decl::Type { name, r#type, .. } => Ok(self.analyze_type_decl(name, r#type)),
            ast::Decl::Func(func_decl) => Ok(self.analyze_func_decl(func_decl)?),
            ast::Decl::Struct { name, fields, .. } => Ok(self.analyze_struct_decl(name, fields)?),
            ast::Decl::Enum {
                is_public,
                name,
                variants,
            } => Ok(()),
            ast::Decl::Construct { name, methods } => {
                Ok(self.analyze_construct_decl(name, methods))
            }
            _ => Err(format!("Unknown declaration kind: {:#?}", decl)),
        }
    }

    #[inline]
    fn analyze_const(&mut self, name: String, ty: ast::TypeExpr, value: ast::Node<ast::ExprKind>) {
        self.analyze_var(name, ty, false, value);
    }

    fn analyze_type_decl(&mut self, name: String, ty: ast::TypeExpr) {
        let target_type = self
            .convert_type_expr_to_type(ty)
            .expect("Invalid type in type alias");

        let symbol = symbol_table::Symbol {
            name: name.clone(),
            kind: symbol_table::SymbolKind::TypeAlias { target_type },
        };

        if let Err(e) = self.symbols.define(&name, symbol) {
            println!("Semantic Error: {}", e);
        }
    }

    fn analyze_func_decl(&mut self, func_decl: ast::FuncDecl) -> Result<(), String> {
        let name = func_decl.name;
        let mut func_params = Vec::new();

        for param in func_decl.params {
            let param_name = param.0;
            let param_type = self.convert_type_expr_to_type(param.1)?;

            func_params.push((param_name, param_type));
        }

        let ret_type: symbol_table::Type = if let Some(ty) = func_decl.return_type {
            self.convert_type_expr_to_type(ty)?
        } else {
            symbol_table::Type::Unit
        };

        let symbol = symbol_table::Symbol {
            name: name.clone(),
            kind: symbol_table::SymbolKind::FuncDecl {
                params: func_params.clone(),
                return_type: ret_type.clone(),
            },
        };

        if let Err(e) = self.symbols.define(&name, symbol) {
            return Err(format!("Semantic Error: {}", e));
        }

        let prev_func_block = self.curr_func_block.clone();
        self.curr_func_block = Some(CurrFuncBlock {
            current_func_name: name,
            curr_ret_type: ret_type.clone(),
        });

        self.symbols.enter_scope();

        for (p_name, p_type) in func_params {
            let param_symbol = symbol_table::Symbol {
                name: p_name.clone(),
                kind: symbol_table::SymbolKind::Variable {
                    var_type: p_type,
                    value: None,
                    mutable: false,
                },
            };

            if let Err(e) = self.symbols.define(&p_name, param_symbol) {
                return Err(format!("Semantic Error: {}", e));
            }
        }

        for stmt in func_decl.body {
            self.analyze_stmt(stmt)?;
        }

        if let Err(e) = self.symbols.exit_scope() {
            return Err(format!("Semantic Error: {}", e));
        }

        self.curr_func_block = prev_func_block;
        Ok(())
    }

    fn analyze_struct_decl(&mut self, name: String, fields: Vec<(String, ast::TypeExpr)>) -> Result<(), String> {
        let mut struct_fields = HashMap::new();

        for field in fields {
            let field_name = field.0;
            let field_type = self.convert_type_expr_to_type(field.1)?;

            struct_fields.insert(field_name, field_type);
        }

        let symbol = symbol_table::Symbol {
            name: name.clone(),
            kind: symbol_table::SymbolKind::StructDecl {
                fields: struct_fields,
            },
        };

        if let Err(e) = self.symbols.define(&name, symbol) {
            return Err(format!("Semantic Error: {}", e));
        }

        Ok(())
    }

    fn analyze_construct_decl(&mut self, name: String, methods: Vec<ast::FuncDecl>) {
        // Placeholder behavior
        for method in methods {
            self.analyze_func_decl(method);
        }
    }

    fn convert_type_expr_to_type(
        &mut self,
        type_expr: ast::TypeExpr,
    ) -> Result<symbol_table::Type, String> {
        match type_expr {
            ast::TypeExpr::Named(ty_expr) => {
                let sym_type = match ty_expr.as_str() {
                    "int8" => symbol_table::Type::Int8,
                    "int16" => symbol_table::Type::Int16,
                    "int32" => symbol_table::Type::Int32,
                    "int64" => symbol_table::Type::Int64,
                    "int128" => symbol_table::Type::Int128,
                    "int_n" => symbol_table::Type::IntN,

                    "uint8" => symbol_table::Type::UInt8,
                    "uint16" => symbol_table::Type::UInt16,
                    "uint32" => symbol_table::Type::UInt32,
                    "uint64" => symbol_table::Type::UInt64,
                    "uint128" => symbol_table::Type::UInt128,
                    "uint_n" => symbol_table::Type::UIntN,

                    "float32" => symbol_table::Type::Float32,
                    "float64" => symbol_table::Type::Float64,

                    "string" => symbol_table::Type::String,
                    "bool" => symbol_table::Type::Bool,
                    "char" => symbol_table::Type::Char,

                    unknown_name => {
                        if let Some(symbol) = self.symbols.resolve(unknown_name) {
                            match &symbol.kind {
                                symbol_table::SymbolKind::StructDecl { .. } => {
                                    Ok(symbol_table::Type::Struct(unknown_name.to_string()))
                                }

                                symbol_table::SymbolKind::TypeAlias { target_type } => {
                                    Ok(target_type.clone())
                                }
                                _ => Err(format!(
                                    "'{}' is defined but is not a struct/type.",
                                    unknown_name
                                )),
                            }
                        } else {
                            Err(format!("Unknown type: '{}'", unknown_name))
                        }
                    }?,
                };
                Ok(sym_type)
            }
            _ => Err("Complex types not implemented yet".to_string()),
        }
    }

    fn convert_expr_to_val_type(
        &mut self,
        expr: ast::Expr,
        s_type: symbol_table::Type,
    ) -> Result<symbol_table::ValueType, String> {
        match expr.value {
            ast::ExprKind::Literal(lit) => {
                let val_type = match lit {
                    token::LiteralTypes::Int(i_lit) => match s_type {
                        symbol_table::Type::Int8 => {
                            symbol_table::ValueType::Int8(i_lit.try_into().unwrap())
                        }
                        symbol_table::Type::Int16 => {
                            symbol_table::ValueType::Int16(i_lit.try_into().unwrap())
                        }
                        symbol_table::Type::Int32 => {
                            symbol_table::ValueType::Int32(i_lit.try_into().unwrap())
                        }
                        symbol_table::Type::Int64 => {
                            symbol_table::ValueType::Int64(i_lit.try_into().unwrap())
                        }
                        symbol_table::Type::Int128 => {
                            symbol_table::ValueType::Int128(i_lit.try_into().unwrap())
                        }
                        symbol_table::Type::IntN => symbol_table::ValueType::IntN(i_lit),

                        symbol_table::Type::UInt8 => {
                            symbol_table::ValueType::UInt8(i_lit.try_into().unwrap())
                        }
                        symbol_table::Type::UInt16 => {
                            symbol_table::ValueType::UInt16(i_lit.try_into().unwrap())
                        }
                        symbol_table::Type::UInt32 => {
                            symbol_table::ValueType::UInt32(i_lit.try_into().unwrap())
                        }
                        symbol_table::Type::UInt64 => {
                            symbol_table::ValueType::UInt64(i_lit.try_into().unwrap())
                        }
                        symbol_table::Type::UInt128 => {
                            symbol_table::ValueType::UInt128(i_lit.try_into().unwrap())
                        }
                        symbol_table::Type::UIntN => {
                            symbol_table::ValueType::UIntN(i_lit.try_into().unwrap())
                        }
                        _ => Err(format!("Unknown integer type"))?,
                    },
                    token::LiteralTypes::Float(f_lit) => match s_type {
                        symbol_table::Type::Float32 => {
                            symbol_table::ValueType::Float32(f_lit as f32)
                        }
                        symbol_table::Type::Float64 => symbol_table::ValueType::Float64(f_lit),
                        _ => Err(format!("Unknown float type"))?,
                    },

                    token::LiteralTypes::String(s_lit) => match s_type {
                        symbol_table::Type::String => symbol_table::ValueType::String(s_lit),
                        _ => Err(format!("Unknown string type"))?,
                    },

                    token::LiteralTypes::Bool(bool_lit) => match s_type {
                        symbol_table::Type::Bool => symbol_table::ValueType::Bool(bool_lit),
                        _ => Err(format!("Unknown bool type"))?,
                    },

                    token::LiteralTypes::Char(char_lit) => match s_type {
                        symbol_table::Type::Char => symbol_table::ValueType::Char(char_lit),
                        _ => Err(format!("Unknown char type"))?,
                    },
                    _ => Err(format!("Unknown literal type"))?,
                };

                return Ok(val_type);
            }
            _ => Err(format!("Unknown literal expr"))?,
        }
        Err(format!("Error: converting expr to value type"))
    }
}
