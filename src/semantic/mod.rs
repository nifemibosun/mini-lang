#![allow(unused)]

pub mod symbol_table;
use crate::parser::ast;

pub struct SemanticAnalyzer {
    ast: ast::Program,
    pub symbols: symbol_table::SymbolTable,
}

impl SemanticAnalyzer {
    pub fn new(ast: ast::Program) -> Self {
        SemanticAnalyzer { 
            ast,
            symbols: symbol_table::SymbolTable::new()
        }
    }

    pub fn visit_node(&mut self) -> symbol_table::SymbolTable {
        for decl in  self.ast.clone() {
            self.visit_decl(decl.value);
        }

        self.symbols.clone()
    }

    fn visit_decl(&mut self, decl: ast::Decl) {
        match decl {
            ast::Decl::Const { name, r#type, value, .. } => {
                let s_type = symbol_table::convert_type_expr_to_type(r#type).unwrap();
                let v_type = symbol_table::convert_expr_to_val_type(value, s_type).unwrap();

                let symbol = symbol_table::Symbol {
                    name: name.clone(),
                    s_type: s_type.clone(),
                    value: Some(symbol_table::Value::new(v_type)),
                    mutable: false,
                };

                self.visit_const_decl(&name.clone(), symbol);
            }
            ast::Decl::Type { name, r#type, .. } => {
                let s_type =  symbol_table::convert_type_expr_to_type(r#type).unwrap();
                let symbol = symbol_table::Symbol {
                    name: name.clone(),
                    s_type,
                    value: None,
                    mutable: false
                };

                self.visit_type_decl(&name.clone(), symbol);
            }
            ast::Decl::Func( .. ) => {
                todo!();
            }
            _ => todo!()
        }
    }

    fn visit_const_decl(&mut self, name: &str, symbol: symbol_table::Symbol) {
        self.symbols.define(name, symbol);
    }

    fn visit_type_decl(&mut self, name: &str, symbol: symbol_table::Symbol) {
        self.symbols.define(name, symbol);
    }

    fn visit_func_decl(&mut self, name: &str, symbol: symbol_table::Symbol) {
        self.symbols.define(name, symbol);
    }
}