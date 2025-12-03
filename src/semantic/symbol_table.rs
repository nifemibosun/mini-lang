#![allow(unused)]

use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
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

    // Other types
    String,
    Bool,
    Char,
    Struct(String),
    Unit,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Value {
    pub val_type: Type,
    pub literal: ValueType,
}

impl Value {
    pub fn new(literal: ValueType) -> Self {
        let val_type = match literal {
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
            _ => Type::IntN,
        };
        Value { val_type, literal }
    }
}

#[derive(Debug, Clone)]
pub enum SymbolKind {
    Return {
        return_type: Type,
    },

    Variable {
        var_type: Type,
        value: Option<Value>,
        mutable: bool,
    },

    TypeAlias {
        target_type: Type,
    },

    StructDecl {
        fields: HashMap<String, Type>,
    },

    FuncDecl {
        params: Vec<(String, Type)>,
        return_type: Type,
    },
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
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
    NotAVariable(String),
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
            NotAVariable(n) => write!(f, "This is not a variable '{}'", n),
            UndefinedVariable(n) => write!(f, "Undefined variable '{}'", n),
            MissingTypeOrInitializer(n) => write!(
                f,
                "Variable '{}' must be initialized or have an explicit type",
                n
            ),
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
        let var_type = match (decl_type, &value) {
            (Some(t), Some(v)) if t != v.val_type => {
                return Err(SymbolTableError::TypeMismatch {
                    name: name.to_string(),
                    expected: t,
                    found: v.val_type.clone(),
                });
            }
            (Some(t), _) => t,
            (None, Some(v)) => v.val_type.clone(),
            (None, None) => {
                return Err(SymbolTableError::MissingTypeOrInitializer(name.to_string()));
            }
        };

        let symbol = Symbol {
            name: name.to_string(),
            kind: SymbolKind::Variable {
                var_type,
                value,
                mutable,
            },
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
                match &mut symbol.kind {
                    SymbolKind::Variable {
                        var_type,
                        value,
                        mutable,
                    } => {
                        if !*mutable {
                            return Err(SymbolTableError::ImmutableAssignment(name.to_string()));
                        }
                        if *var_type != new_value.val_type {
                            return Err(SymbolTableError::TypeMismatch {
                                name: name.to_string(),
                                expected: var_type.clone(),
                                found: new_value.val_type,
                            });
                        }
                        *value = Some(new_value);
                        return Ok(());
                    }
                    _ => return Err(SymbolTableError::NotAVariable(name.to_string())),
                }
            }
        }
        Err(SymbolTableError::UndefinedVariable(name.to_string()))
    }
}
