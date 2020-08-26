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
#[no_mangle]
pub extern "system" fn Java_org_enso_parser_Parser_bench(
    env      : JNIEnv,
    _this    : JClass,
) -> jweak {
    let non = env.get_static_field(
        env.find_class("scala/None$").unwrap(),
        "MODULE$",
        "Lscala/None$;",
    ).unwrap().l().unwrap();
    let env = &env;
    let env = Env {
        env,
        ast: JMethod::new(env, "org/enso/ast/Ast$Ast", "<init>", "(Lscala/Option;JJLjava/lang/Object;)V"),
        app: JMethod::new(env, "org/enso/ast/Ast$App$Prefix", "<init>", "(Lorg/enso/ast/Ast$Ast;Lorg/enso/ast/Ast$Ast;)V"),
        num: JMethod::new(env, "org/enso/ast/Ast$Num$Number", "<init>", "(Ljava/lang/String;)V"),
        non
    };

    let now = Instant::now();
    let ast = tree(20);
    println!("Rust AST build time: {}ms ({})", now.elapsed().as_millis(), ast.len);

    let now = Instant::now();
    let ast = env.tree(20);
    println!("Scala AST build time: {}ms", now.elapsed().as_millis());

    ast.into_inner()
}

pub fn tree(depth:usize) -> ast::AnyAst {
    if depth == 0 {
        return ast::Ast::new(ast::num::Number{number:"0".into()})
    }

    let fun = Box::new(tree(depth-1));
    let arg = Box::new(tree(depth-1));

    ast::Ast::new(ast::app::Prefix {fun, arg})
}

struct JMethod<'a> {
    env:&'a JNIEnv<'a>,
    obj:JClass<'a>,
    fun:JMethodID<'a>,
}

impl<'a> JMethod<'a> {
    pub fn new(env:&'a JNIEnv<'a>, obj:&str, fun:&str, typ:&str) -> Self {
        let obj = env.find_class(obj).unwrap();
        let fun = env.get_method_id(obj, fun, typ).unwrap();

        Self{env,obj,fun}
    }

    pub fn call(&self, args:&[JValue]) -> JObject {
        self.env.new_object_unchecked(self.obj, self.fun, args).unwrap()
    }
}

struct Env<'a> {
    env:&'a JNIEnv<'a>,
    ast:JMethod<'a>,
    app:JMethod<'a>,
    num:JMethod<'a>,
    non:JObject<'a>,
}

impl<'a> Env<'a> {
    pub fn ast(&self, val:JObject) -> JObject {
        self.ast.call(&[self.non.into(), 0.into(), 0.into(), val.into()])
    }

    pub fn tree(&self, depth:usize) -> JObject {
        if depth == 0 {
            return self.ast(self.num.call(&[self.env.new_string("0").unwrap().into()]))
        }
        let app = self.app.call(&[self.tree(depth-1).into(), self.tree(depth-1).into()]);
        self.ast(app)
    }
}