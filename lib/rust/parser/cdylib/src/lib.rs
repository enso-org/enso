//! C interface to [`enso_parser`].
//!
//! See [`Parser`] documentation for a usage example.

// === Features ===
#![feature(core_ffi_c)]
// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]

use enso_prelude::*;

use jni::JNIEnv;
use jni::objects::{JClass, JString, JByteBuffer};
use jni::sys::{jstring, jobject};

// ==============
// === Parser ===
// ==============

/// Parses Enso source code. This type is logically stateless--for any given input, any instance of
/// this type will produce the same output. However it may own resources, so that subsequent usage
/// of the same [`Parser`] can be expected to be more efficient than creating a new [`Parser`] for
/// each input.
///
/// # Usage
///
/// ```C
/// include <enso_parser.h>
///
/// void parse_input(const char *const input, const uintptr_t input_len) {
///     // Allocate a new parser. If parsing more than one input, it would be more efficient to
///     // allocate it once and reuse it.
///     Parser *const parser = parser_new();
///     // Allocate a parse result. If parsing more than one input, there are two efficient
///     // approaches, depending on use pattern:
///     // - Finish with each result before parsing the next (by using the resulting data, or
///     //   copying it to memory used elsewhere); reuse one `ParseResult` for each input parsed.
///     // - Keep `ParseResult`s to avoid needing to copy their contents; allocate new instances
///     //   to store results of parsing additional inputs.
///     ParseResult *const result = parse_result_alloc();
///     // Parse the data.
///     parser_run(input, input_len, result);
///     // Gather the results.
///     char *const sanitized_input = parse_result_get_input_data(result);
///     const uint8_t *const output = parse_result_get_output_data(result);
///     const uintptr_t output_len = parse_result_get_output_data_len(result);
///     // Do something with the results.
///     handle_parse_result(sanitized_input || input, output, output_len);
///     // Free resources.
///     parse_result_free(result);
///     parser_free(parser);
/// }
/// ```
#[derive(Debug, Default)]
#[allow(missing_copy_implementations)] // Expected to contain non-Copy data in the future.
pub struct Parser {
    // The implementation doesn't currently support reusing parser resources, but when we add that
    // optimization, the API supports it.
}


// === Implementation ===

impl Parser {
    fn run(&mut self, input: &[u8]) -> ParseResult {
        let input = String::from_utf8_lossy(input);
        let tree = enso_parser::Parser::new().run(&input);
        let tree = match enso_parser::serialization::serialize_tree(&tree) {
            Ok(tree) => tree,
            // `Tree` does not contain any types with fallible `serialize` implementations, so this
            // cannot fail.
            Err(_) => default(),
        };
        let input = match input {
            Cow::Owned(input) => Some(input),
            _ => None,
        };
        ParseResult { input, tree }
    }
}

// === Java Interface ===

// This keeps Rust from "mangling" the name and making it unique for this
// crate.
#[no_mangle]
pub extern "system" fn Java_org_enso_syntax2_LoadParser_hello(env: JNIEnv,
                                             class: JClass,
                                             buf: JByteBuffer) -> jobject {
    unsafe {
        println!("{}", "in");
        let p: *mut Parser = parser_new();
        let r: *mut ParseResult = parse_result_alloc();

        println!("{}", "in2");
        let addr = env.get_direct_buffer_address(buf).ok().map(|b| {
            println!("{}", "in3");
            env.get_direct_buffer_capacity(buf).map(|c| {

                println!("{}", "in4");
                parser_run(p, b.as_mut_ptr(), c as usize, r);
                println!("{}", "in5");
            });
        });
        let mut msg: String = "Hello ".to_owned();
        msg.push_str(" from Rust");
        println!("{}", msg);
        let length: usize = parse_result_get_output_len(r);
        let data = parse_result_get_output_data_mut(r);

        let mut s2: &mut[u8] = std::slice::from_raw_parts_mut(data, length);


        let result = env.new_direct_byte_buffer(s2);

        parse_result_free(r);
        parser_free(p);

        // Finally, extract the raw pointer to return.
        result.ok().unwrap().into_inner()
    }
}


// === C Interface ===

/// Create a new parser. This operation is infallible; the result will never be NULL.
#[no_mangle]
pub extern "C" fn parser_new() -> *mut Parser {
    Box::into_raw(default())
}

/// Parse the given source code, provided in the input parameters. The result will be written to the
/// object provided in the output parameter; any previous state of the output object will be
/// overwritten.
///
/// # Safety
///
/// The `parser` argument must be a value returned by `parser_new` that has never been passed to
/// `parser_free`.
/// The `input` argument must be a non-NULL pointer to a sequence of at least `input_len` readable
/// bytes.
/// The `output` argument must be a value returned by `parse_result_alloc` that has never been
/// passed to `parse_result_free`.
#[no_mangle]
pub unsafe extern "C" fn parser_run(
    parser: *mut Parser,
    input: *const u8,
    input_len: usize,
    output: *mut ParseResult,
) {
    let parser = &mut *parser;
    let input = slice::from_raw_parts(input, input_len);
    let output = &mut *output;
    *output = parser.run(input);
}

/// Release the parser's resources.
///
/// # Safety
///
/// The `parser` argument must be a value returned by `parser_new` that has never been passed to
/// `parser_free`.
#[no_mangle]
pub unsafe extern "C" fn parser_free(parser: *mut Parser) {
    let parser = Box::from_raw(parser);
    drop(parser);
}



// ===================
// === ParseResult ===
// ===================

/// The result of parsing. Contains the source code, if the parser modified it, and the serialized
/// AST.
#[derive(Debug, Default)]
pub struct ParseResult {
    input: Option<String>,
    tree:  Vec<u8>,
}


// === C Interface ===

/// Allocate a new parse result; its initial state is unspecified. This operation is infallible; the
/// result will never be NULL.
#[no_mangle]
pub extern "C" fn parse_result_alloc() -> *mut ParseResult {
    Box::into_raw(default())
}

/// If the input provided was valid utf-8, this will return NULL. If it was not, this will return
/// the beginning of a copy of the input that has been modified to contain only valid utf-8. If this
/// returns non-NULL, all source code in the serialized AST data will be relative to this pointer,
/// not the original input.
///
/// # Safety
///
/// The `result` argument must be a value returned by `parse_result_alloc` that has never been
/// passed to `parse_result_free`.
#[no_mangle]
pub unsafe extern "C" fn parse_result_get_input_data(result: *mut ParseResult) -> *const u8 {
    let result = &*result;
    result.input.as_ref().map(|s| s.as_ptr()).unwrap_or(0 as _)
}

/// Return the length of the data obtained by [`parse_result_get_input_data`].
///
/// # Safety
///
/// The `result` argument must be a value returned by `parse_result_alloc` that has never been
/// passed to `parse_result_free`.
#[no_mangle]
pub unsafe extern "C" fn parse_result_get_input_len(result: *mut ParseResult) -> usize {
    let result = &*result;
    result.input.as_ref().map(|s| s.len()).unwrap_or(0)
}

/// Return a pointer to the start of the serialized AST data.
///
/// # Safety
///
/// The `result` argument must be a value returned by `parse_result_alloc` that has never been
/// passed to `parse_result_free`.
#[no_mangle]
pub unsafe extern "C" fn parse_result_get_output_data(result: *mut ParseResult) -> *const u8 {
    let result = &*result;
    result.tree.as_ptr()
}

unsafe extern "C" fn parse_result_get_output_data_mut(result: *mut ParseResult) -> *mut u8 {
    let result : &mut ParseResult = &mut *result;
    result.tree.as_mut_ptr()
}

/// Return the length of the data obtained by [`parse_result_get_output_data`].
///
/// # Safety
///
/// The `result` argument must be a value returned by `parse_result_alloc` that has never been
/// passed to `parse_result_free`.
#[no_mangle]
pub unsafe extern "C" fn parse_result_get_output_len(result: *mut ParseResult) -> usize {
    let result = &*result;
    result.tree.len()
}

/// Free the memory allocated for these parse results; any pointers obtained from this instance
/// will no longer be valid.
///
/// # Safety
///
/// The `result` argument must be a value returned by `parse_result_alloc` that has never been
/// passed to `parse_result_free`.
#[no_mangle]
pub unsafe extern "C" fn parse_result_free(ast: *mut ParseResult) {
    let ast = Box::from_raw(ast);
    drop(ast);
}
