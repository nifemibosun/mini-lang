#![allow(unused)]

use std::collections::HashMap;

pub mod symbol_table;
use crate::parser::ast;
use crate::scanner::token;

#[derive(Debug, Clone)]
pub struct SemanticAnalyzer<'a> {
    ast: &'a ast::Program,
    pub symbols: symbol_table::SymbolTable,
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(ast: &'a ast::Program) -> Self {
        SemanticAnalyzer {
            ast,
            symbols: symbol_table::SymbolTable::new(),
        }
    }

    pub fn analyze_program(&mut self) -> symbol_table::SymbolTable {
        for decl in self.ast.clone() {
            self.analyze_decl(decl.value);
        }

        self.symbols.clone()
    }

    fn analyze_decl(&mut self, decl: ast::Decl) {
        match decl {
            ast::Decl::Const {
                name,
                r#type,
                value,
                ..
            } => self.analyze_const_decl(name, r#type, value),
            ast::Decl::Type { name, r#type, .. } => self.analyze_type_decl(name, r#type),
            ast::Decl::Func(func_decl) => self.analyze_func_decl(func_decl),
            ast::Decl::Struct { name, fields, .. } => self.analyze_struct_decl(name, fields),
            _ => todo!(),
        }
    }

    fn analyze_const_decl(
        &mut self,
        name: String,
        ty: ast::TypeExpr,
        value: ast::Node<ast::ExprKind>,
    ) {
        let var_type = self.convert_type_expr_to_type(ty).unwrap();
        let val_type = self
            .convert_expr_to_val_type(value, var_type.clone())
            .unwrap();

        let symbol = symbol_table::Symbol {
            name: name.clone(),
            kind: symbol_table::SymbolKind::Variable {
                var_type: var_type.clone(),
                value: Some(symbol_table::Value::new(val_type)),
                mutable: false,
            },
        };

        if let Err(e) = self.symbols.define(&name, symbol) {
            println!("Semantic Error: {}", e);
        }
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

    fn analyze_func_decl(&mut self, func_decl: ast::FuncDecl) {
        let name = func_decl.name;
        let mut func_params = Vec::new();

        for param in func_decl.params {
            let param_name = param.0;
            let param_type = self.convert_type_expr_to_type(param.1).unwrap();

            func_params.push((param_name, param_type));
        }

        let return_type: symbol_table::Type = if let Some(ty) = func_decl.return_type {
            self.convert_type_expr_to_type(ty).unwrap()
        } else {
            symbol_table::Type::Unit
        };

        let symbol = symbol_table::Symbol {
            name: name.clone(),
            kind: symbol_table::SymbolKind::FuncDecl {
                params: func_params,
                return_type,
            },
        };

        self.symbols.define(name.as_str(), symbol).unwrap();
    }

    fn analyze_struct_decl(&mut self, name: String, fields: Vec<(String, ast::TypeExpr)>) {
        let mut struct_fields = HashMap::new();

        for field in fields {
            let field_name = field.0;
            let field_type = self.convert_type_expr_to_type(field.1).unwrap();

            struct_fields.insert(field_name, field_type);
        }

        let symbol = symbol_table::Symbol {
            name: name.clone(),
            kind: symbol_table::SymbolKind::StructDecl {
                fields: struct_fields,
            },
        };

        if let Err(e) = self.symbols.define(&name, symbol) {
            println!("Semantic Error: {}", e);
        }
    }

    fn convert_type_expr_to_type(
        &self,
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
        &self,
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
                        _ => todo!(),
                    },
                    token::LiteralTypes::Float(f_lit) => {
                        match s_type {
                            // symbol_table::Type::Float32 => symbol_table::ValueType::Float32(f_lit.try_into().unwrap()),
                            symbol_table::Type::Float64 => symbol_table::ValueType::Float64(f_lit),
                            _ => todo!("Coming soon float"),
                        }
                    }

                    token::LiteralTypes::String(s_lit) => match s_type {
                        symbol_table::Type::String => symbol_table::ValueType::String(s_lit),
                        _ => todo!(),
                    },

                    token::LiteralTypes::Bool(bool_lit) => match s_type {
                        symbol_table::Type::Bool => symbol_table::ValueType::Bool(bool_lit),
                        _ => todo!(),
                    },

                    token::LiteralTypes::Char(char_lit) => match s_type {
                        symbol_table::Type::Char => symbol_table::ValueType::Char(char_lit),
                        _ => todo!(),
                    },
                    _ => todo!(),
                };

                return Ok(val_type);
            }
            _ => Err("todo".to_string()),
        }
        // Err("Implement existing logic".to_string())
    }
}
