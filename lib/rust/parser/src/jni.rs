//! This module exports JNI interface for parser methods implemented in Rust.
//!
//! The basics steps to add a new method are following:
//! 1. Add the new method in Scala (in `org.enso.parser.Parser`).
//! 2. (Optional) Run `scalac Parser.scala; javah Parser` to generate the C API in `Parser.h`.
//!    Note that you can skip this step. It is merely a guidance for you, as it generates
//!    the correct function names and type signatures of all `Parser` native methods.
//!    Generally, the method interface is going to have the following shape:
//!    ```c
//!    JNIEXPORT $returnType JNICALL Java_$package_$className_$methodName
//!      (JNIEnv* env, jobject this, $argType1 $arg1, $argType2 $arg2)
//!    ```
//!    For example if the definition is:
//!    ```scala
//!    package org.enso.parser
//!
//!    class Parser {
//!      @native def newMethod(string: String, array: Array[Int])
//!    }
//!    ```
//!    Then the JNI API is going to be:
//!    ```c
//!    JNIEXPORT jobject JNICALL Java_org_enso_parser_Parser_newMethod
//!      (JNIEnv* env, jobject this, jstring string, jintArray array)
//!    ```
//!    The list of all available types can be found in
//!    [oracle documentation](https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/types.html).
//! 3. Implement the new parser method in this file.
//!    For the above definition the implementation is going to be:
//!    ```rust
//!    use jni::JNIEnv;
//!    use jni::objects::*;
//!    use jni::sys::*;
//!
//!    #[no_mangle]
//!    pub extern "system" fn Java_org_enso_parser_Parser_newMethod(
//!        env    : JNIEnv,    // the JVM enviroment, used for calling methods and constructors
//!        this   : JClass,    // the instance of `Parser`
//!        string : JString,
//!        array  : jintArray,
//!    ) -> jweak { unimplemented!() }
//!    ```
//! 4. (Optional) Generate a shared library from the Rust definition by `cargo build`.
//!    It will be generated into `target/rust/debug/`.
//!    This step is done automatically by `sbt`.

use ast::Api;

use jni::JNIEnv;
use jni::objects::*;
use jni::sys::*;
use std::time::Instant;


// ======================
// === Parser JNI API ===
// ======================

/// Parses a content a of single source file.
#[no_mangle]
pub extern "system" fn Java_org_enso_parser_Parser_parseStr(
    env   : JNIEnv,
    _this : JClass,
    input : JString,
) -> jweak {
    let api = ast::Scala::new(&env);
    let txt = api.text(env.get_string(input).unwrap().into());
    let ast = api.ast_shape(None, 0, 0, api.shape_text(txt));

    ast.into_inner()
}

/// Parses a single source file.
#[no_mangle]
pub extern "system" fn Java_org_enso_parser_Parser_parseFile(
    env      : JNIEnv,
    this     : JClass,
    filename : JString,
) -> jweak {
    Java_org_enso_parser_Parser_parseStr(env, this, filename)
}


// === Tokens ===

/// Parses a content of a single source file into a stream of tokens.
#[no_mangle]
pub extern "system" fn Java_org_enso_parser_Parser_lexStr(
    env   : JNIEnv,
    this  : JClass,
    input : JString,
) -> jweak {
    Java_org_enso_parser_Parser_parseStr(env, this, input)
}

/// Parses a single source file into a stream of tokens.
#[no_mangle]
pub extern "system" fn Java_org_enso_parser_Parser_lexFile(
    env      : JNIEnv,
    this     : JClass,
    filename : JString,
) -> jweak {
    Java_org_enso_parser_Parser_parseStr(env, this, filename)
}


// === Benchmark ===

/// Creates a prefix tree of depth 20 and prints the it took.
#[no_mangle]
pub extern "system" fn Java_org_enso_parser_Parser_bench(
    env      : JNIEnv,
    _this    : JClass,
) -> jweak {
    let now = Instant::now();
    let ast = tree(&ast::Rust, 20);
    println!("Rust AST build time: {}ms ({})", now.elapsed().as_millis(), ast.len);

    let now = Instant::now();
    let ast = tree(&ast::Scala::new(&env), 20);
    println!("Scala AST build time: {}ms", now.elapsed().as_millis());

    ast.into_inner()
}

/// Creates a prefix tree of given depth.
pub fn tree<Api:ast::Api>(api:&Api, depth:usize) -> Api::AstShape {
    if depth == 0 {
        return api.ast_shape(None, 0, 0, api.shape_number(api.number("0".to_string())));
    }

    let fun = Box::new(tree(api, depth-1));
    let arg = Box::new(tree(api, depth-1));

    api.ast_shape(None, 0, 0, api.shape_prefix(api.prefix(fun, arg)))
}
