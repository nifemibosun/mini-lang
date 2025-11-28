#![allow(unused)]

use std::collections::HashMap;
use crate::{
    parser::ast, 
    scanner::token
};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Type {
    // Signed Integer types
    Int8,
    Int16,
    Int32,
    Int64,
    Int128,
    IntN,

    // UnSigned Integer types
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    UInt128,
    UIntN,

    // Float types
    Float32,
    Float64,

    String,
    Bool,
    Char,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    // Signed Integer value types
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Int128(i128),
    IntN(isize),

    // UnSigned Integer value types
    UInt8(u8),
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),
    UInt128(u128),
    UIntN(usize),

    // Float value types
    Float32(f32),
    Float64(f64),

    String(String),
    Bool(bool),
    Char(char),
}

/// Takes a TypeExpr as input and return symbol type
pub fn convert_type_expr_to_type(type_expr: ast::TypeExpr) -> Result<Type, String> {
    match type_expr {
        ast::TypeExpr::Named(ty_expr) => {
            let s_type = match ty_expr.as_str() {
                "int8" => Type::Int8,
                "int16" => Type::Int16,
                "int32" => Type::Int32,
                "int64" => Type::Int64,
                "int128" => Type::Int128,
                "int_n" => Type::IntN,

                "uint8" => Type::UInt8,
                "uint16" => Type::UInt16,
                "uint32" => Type::UInt32,
                "uint64" => Type::UInt64,
                "uint128" => Type::UInt128,
                "uint_n" => Type::UIntN,

                "float32" => Type::Float32,
                "float64" => Type::Float64,

                "string" => Type::String,
                "bool" => Type::Bool,
                "char" => Type::Char,
                _ => todo!()
            };

            Ok(s_type)
        }
        _ => Err("todo".to_string())
    }
}

pub fn convert_expr_to_val_type(expr: ast::Expr, s_type: Type) -> Result<ValueType, String> {
    match expr.value {
        ast::ExprKind::Literal(lit) => {
            let v_type = match lit {
                token::LiteralTypes::Int(i_lit) => {
                    match s_type {
                        Type::Int8 => ValueType::Int8(i_lit.try_into().unwrap()),
                        Type::Int16 => ValueType::Int16(i_lit.try_into().unwrap()),
                        Type::Int32 => ValueType::Int32(i_lit.try_into().unwrap()),
                        Type::Int64 => ValueType::Int64(i_lit.try_into().unwrap()),
                        Type::Int128 => ValueType::Int128(i_lit.try_into().unwrap()),
                        Type::IntN => ValueType::IntN(i_lit),

                        Type::UInt8 => ValueType::UInt8(i_lit.try_into().unwrap()),
                        Type::UInt16 => ValueType::UInt16(i_lit.try_into().unwrap()),
                        Type::UInt32 => ValueType::UInt32(i_lit.try_into().unwrap()),
                        Type::UInt64 => ValueType::UInt64(i_lit.try_into().unwrap()),
                        Type::UInt128 => ValueType::UInt128(i_lit.try_into().unwrap()),
                        Type::UIntN => ValueType::UIntN(i_lit.try_into().unwrap()),
                        _ => todo!()
                    }
                }
                token::LiteralTypes::Float(f_lit) => {
                    match s_type {
                        // Type::Float32 => ValueType::Float32(f_lit.try_into().unwrap()),
                        Type::Float64 => ValueType::Float64(f_lit),
                        _ => todo!("Coming soon float")
                    }
                }

                token::LiteralTypes::String(s_lit) => {
                    match s_type {
                        Type::String => ValueType::String(s_lit),
                        _ => todo!()
                    }
                }

                token::LiteralTypes::Bool(bool_lit) => {
                    match s_type {
                        Type::Bool => ValueType::Bool(bool_lit),
                        _ => todo!()
                    }
                }

                token::LiteralTypes::Char(char_lit) => {
                    match s_type {
                        Type::Char => ValueType::Char(char_lit),
                        _ => todo!()
                    }
                }
                _ => todo!()
            };

            Ok(v_type)
        }
        _ => Err("todo".to_string())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Value {
    // Value type
    pub v_type: Type,
    pub literal: ValueType,
}

impl Value {
    pub fn new(literal: ValueType) -> Self {
        let v_type = match literal {
            ValueType::Int8(_) => Type::Int8,
            ValueType::Int16(_) => Type::Int16,
            ValueType::Int32(_) => Type::Int32,
            ValueType::Int64(_) => Type::Int64,
            ValueType::Int128(_) => Type::Int128,
            ValueType::IntN(_) => Type::IntN,

            ValueType::UInt8(_) => Type::UInt8,
            ValueType::UInt16(_) => Type::UInt16,
            ValueType::UInt32(_) => Type::UInt32,
            ValueType::UInt64(_) => Type::UInt64,
            ValueType::UInt128(_) => Type::UInt128,
            ValueType::UIntN(_) => Type::UIntN,

            ValueType::Float32(_) => Type::Float32,
            ValueType::Float64(_) => Type::Float64,

            ValueType::String(_) => Type::String,
            ValueType::Bool(_) => Type::Bool,
            ValueType::Char(_) => Type::Char,
        };
        Value { v_type, literal }
    }
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    // Symbol type
    pub s_type: Type,
    pub value: Option<Value>,
    pub mutable: bool,
}

#[derive(Debug, Clone)]
pub enum SymbolTableError {
    Redefinition(String),
    ImmutableAssignment(String),
    TypeMismatch {
        name: String,
        expected: Type,
        found: Type,
    },
    UndefinedVariable(String),
    MissingTypeOrInitializer(String),
    CannotExitGlobalScope,
}

impl std::fmt::Display for SymbolTableError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use SymbolTableError::*;
        match self {
            Redefinition(n) => write!(f, "Symbol '{}' is being redefined in the same scope", n),
            ImmutableAssignment(n) => write!(f, "Cannot assign to immutable variable '{}'", n),
            TypeMismatch {
                name,
                expected,
                found,
            } => write!(
                f,
                "Type mismatch for '{}': expected {:?}, got {:?}",
                name, expected, found
            ),
            UndefinedVariable(n) => write!(f, "Undefined variable '{}'", n),
            MissingTypeOrInitializer(n) => write!(f, "Variable '{}' must be initialized or have an explicit type", n),
            CannotExitGlobalScope => write!(f, "Cannot exit global scope"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) -> Result<(), SymbolTableError> {
        if self.scopes.len() > 1 {
            self.scopes.pop();
            Ok(())
        } else {
            Err(SymbolTableError::CannotExitGlobalScope)
        }
    }

    fn current_scope(&mut self) -> &mut HashMap<String, Symbol> {
        self.scopes.last_mut().unwrap()
    }

    pub fn current_scope_level(&self) -> usize {
        self.scopes.len() - 1
    }

    pub fn define(&mut self, name: &str, symbol: Symbol) -> Result<(), SymbolTableError> {
        let scope = self.current_scope();

        if scope.contains_key(&name.to_string()) {
            return Err(SymbolTableError::Redefinition(name.to_string()));
        }

        scope.insert(name.to_string(), symbol);
        Ok(())
    }

    pub fn declare(
        &mut self,
        name: &str,
        decl_type: Option<Type>,
        init: Option<ValueType>,
        mutable: bool,
    ) -> Result<(), SymbolTableError> {
        if let Some(_) = self.current_scope().get(name) {
            return Err(SymbolTableError::Redefinition(name.to_string()));
        }

        let value = init.map(Value::new);
        let s_type = match (decl_type, &value) {
            (Some(t), Some(v)) if t != v.v_type => {
                return Err(SymbolTableError::TypeMismatch {
                    name: name.to_string(),
                    expected: t,
                    found: v.v_type.clone(),
                });
            }
            (Some(t), _) => t,
            (None, Some(v)) => v.v_type.clone(),
            (None, None) => {
                return Err(SymbolTableError::MissingTypeOrInitializer(name.to_string()));
            }
        };

        let symbol = Symbol {
            name: name.to_string(),
            s_type,
            value,
            mutable,
        };

        self.current_scope().insert(name.to_string(), symbol);
        Ok(())
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    pub fn resolve_mut(&mut self, name: &str) -> Option<&mut Symbol> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(symbol) = scope.get_mut(name) {
                return Some(symbol);
            }
        }
        None
    }

    pub fn assign(&mut self, name: &str, new_value: Value) -> Result<(), SymbolTableError> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(symbol) = scope.get_mut(name) {
                if !symbol.mutable {
                    return Err(SymbolTableError::ImmutableAssignment(name.to_string()));
                }
                if symbol.s_type != new_value.v_type {
                    return Err(SymbolTableError::TypeMismatch {
                        name: name.to_string(),
                        expected: symbol.s_type.clone(),
                        found: new_value.v_type,
                    });
                }
                symbol.value = Some(new_value);
                return Ok(());
            }
        }
        Err(SymbolTableError::UndefinedVariable(name.to_string()))
    }
}
