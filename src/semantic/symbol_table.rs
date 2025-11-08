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
    UInt,
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
pub enum ValueTypes {
    // Signed Integer types
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    Int128(i128),
    IntN(isize),

    // UnSigned Integer types
    UInt8(u8),
    UInt16(u16),
    UInt32(u32),
    UInt64(u64),
    UInt128(u128),
    UIntN(usize),

    // Float types
    Float32(f32),
    Float64(f64),

    String(String),
    Bool(bool),
    Char(char),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Value {
    v_type: Type,
    literal: ValueTypes, 
}


#[derive(Debug, Clone)]
pub struct Symbol {
    name: String,
    s_type: Type,
    value: Option<Value>,
    mutable: bool,
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { scopes: vec![HashMap::new()] }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        } else {
            panic!("Cannot exit global scope");
        }
    }

    fn current_scope(&mut self) -> &mut HashMap<String, Symbol> {
        self.scopes.last_mut().unwrap()
    }

    pub fn current_scope_level(&self) -> usize {
        self.scopes.len() - 1
    }

    pub fn define(&mut self, name: &str, symbol: Symbol) {
        let scope = self.current_scope();

        if scope.contains_key(&name.to_string()) {
            println!("Warning: symbol `{}` is being redefined in the same scope", name);
        }

        scope.insert(name.to_string(), symbol);
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    pub fn assign(&mut self, name: &str, new_value: Value) -> Result<(), String> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(symbol) = scope.get_mut(name) {
                if !symbol.mutable {
                    return Err(format!("Cannot assign to immutable variable `{}`", name));
                }
                if symbol.s_type != new_value.v_type {
                    return Err(format!("Type mismatch for `{}`: expected {:?}, got {:?}", name, symbol.s_type, new_value.v_type));
                }
                symbol.value = Some(new_value);
                return Ok(());
            }
        }
        Err(format!("Undefined variable `{}`", name))
    }
}
