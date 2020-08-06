#![feature(test)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This module exports the implementation of parser for the Enso language.

use ast::AnyAst;
use ast::Ast;

use jni::JNIEnv;
use jni::objects::*;
use jni::sys::*;



// =======================
// === Parser Rust API ===
// =======================

/// Parses a single source file.
pub fn parse(input:String) -> AnyAst {
    Ast::new(ast::txt::Text{text:input})
}



// ======================
// === Parser JNI API ===
// ======================

/// Parses a single source file.
#[no_mangle]
pub extern "system" fn Java_org_enso_parser_Parser_parse(
    env: JNIEnv,
    _class: JClass,
    input: JString,
) -> jweak {
    let txt = env.new_object(
        env.find_class("org/enso/ast/Ast$Txt$Text").unwrap(),
        "(Ljava/lang/String;)V",
        &[input.into()],
    ).unwrap();

    let non = env.get_static_field(
        env.find_class("scala/None$").unwrap(),
        "MODULE$",
        "Lscala/None$;",
    ).unwrap().l().unwrap();

    let ast = env.new_object(
        env.find_class("org/enso/ast/Ast$Ast").unwrap(),
        "(Lscala/Option;JJLjava/lang/Object;)V",
        &[non.into(), 0i64.into(), 0i64.into(), txt.into()],
    ).unwrap();

    ast.into_inner()
}
