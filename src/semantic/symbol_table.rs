#![allow(unused)]

use std::collections::HashMap;

pub type TypeId = usize;

#[derive(Debug, Clone)]
pub enum SymbolKind {
    Variable {
        mutable: bool,
        r#type: TypeId,
    },
    Const {
        ty: TypeId,
    },
    Func {
        params: Vec<TypeId>,
        ret: TypeId,
    },
    Struct {
        fields: HashMap<String, TypeId>,
    },
    Enum {
        variants: HashMap<String, Option<TypeId>>,
    },
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub kind: SymbolKind,
}

#[derive(Debug, Default)]
pub struct Scope {
    pub symbols: HashMap<String, Symbol>,
}

#[derive(Debug, Default)]
pub struct SymbolTable {
    scopes: Vec<Scope>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![Scope::default()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    pub fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn define(&mut self, name: String, symbol: Symbol) -> Result<(), String> {
        let current = self.scopes.last_mut().unwrap();

        if current.symbols.contains_key(&name) {
            Err(format!("Symbol '{}' already defined in this scope", name))
        } else {
            current.symbols.insert(name, symbol);
            Ok(())
        }
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(sym) = scope.symbols.get(name) {
                return Some(sym);
            }
        }

        None
    }
}
