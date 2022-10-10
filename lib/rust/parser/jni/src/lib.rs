//! Java interface to [`enso_parser`].

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
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

use jni::objects::JByteBuffer;
use jni::objects::JClass;
use jni::sys::jobject;
use jni::JNIEnv;



// ======================
// === Java Interface ===
// ======================

/// Parse the input. Returns a serialized representation of the parse tree. The caller is
/// responsible for freeing the memory associated with the returned buffer.
///
/// # Safety
///
/// The state MUST be a value returned by `allocState` that has not been passed to `freeState`.
/// The input buffer contents MUST be valid UTF-8.
/// The contents of the returned buffer MUST not be accessed after another call to `parseInput`, or
/// a call to `freeState`.
#[allow(unsafe_code)]
#[no_mangle]
pub extern "system" fn Java_org_enso_syntax2_Parser_parseInput(
    env: JNIEnv,
    _class: JClass,
    state: u64,
    input: JByteBuffer,
) -> jobject {
    let state = unsafe { &mut *(state as usize as *mut State) };
    let direct_allocated = "Internal Error: ByteBuffer must be direct-allocated.";
    let input = env.get_direct_buffer_address(input).expect(direct_allocated);
    let input = if cfg!(debug_assertions) {
        std::str::from_utf8(input).unwrap()
    } else {
        unsafe { std::str::from_utf8_unchecked(input) }
    };
    let mut code = input;
    let mut meta = None;
    if let Some((meta_, code_)) = enso_parser::metadata::parse(input) {
        meta = Some(meta_);
        code = code_;
    }
    state.base = str::as_ptr(code) as usize as u64;
    let tree = enso_parser::Parser::new().run(code);
    state.output = match enso_parser::serialization::serialize_tree(&tree) {
        Ok(tree) => tree,
        // `Tree` does not contain any types with fallible `serialize` implementations, so this
        // cannot fail.
        Err(_) => {
            debug_assert!(false);
            default()
        }
    };
    state.metadata = meta.and_then(|meta| meta.ok());
    let result = env.new_direct_byte_buffer(&mut state.output);
    result.unwrap().into_inner()
}

/// Return the `base` parameter to pass to the `Message` class along with the other output of the
/// most recent call to `parseInput`.
///
/// # Safety
///
/// The input MUST have been returned by `allocState`, and MUST NOT have previously been passed to
/// `freeState`.
#[allow(unsafe_code)]
#[no_mangle]
pub extern "system" fn Java_org_enso_syntax2_Parser_getLastInputBase(
    _env: JNIEnv,
    _class: JClass,
    state: u64,
) -> u64 {
    let state = unsafe { &mut *(state as usize as *mut State) };
    state.base
}

/// Return the metadata associated with the most recent parse.
///
/// # Safety
///
/// The input MUST have been returned by `allocState`, and MUST NOT have previously been passed to
/// `freeState`.
#[allow(unsafe_code)]
#[no_mangle]
pub extern "system" fn Java_org_enso_syntax2_Parser_getMetadata(
    _env: JNIEnv,
    _class: JClass,
    state: u64,
) -> u64 {
    let state = unsafe { &mut *(state as usize as *mut State) };
    match &state.metadata {
        Some(metadata) => {
            let metadata: *const _ = metadata;
            metadata as usize as u64
        }
        None => 0,
    }
}

/// Allocate a new parser state object. The returned value should be passed to `freeState` when no
/// longer needed.
#[allow(unsafe_code)]
#[no_mangle]
pub extern "system" fn Java_org_enso_syntax2_Parser_allocState(
    _env: JNIEnv,
    _class: JClass,
) -> u64 {
    Box::into_raw(Box::new(State::default())) as _
}

/// Free the resources owned by the state object.
///
/// # Safety
///
/// The input MUST have been returned by `allocState`, and MUST NOT have previously been passed to
/// `freeState`.
#[allow(unsafe_code)]
#[no_mangle]
pub extern "system" fn Java_org_enso_syntax2_Parser_freeState(
    _env: JNIEnv,
    _class: JClass,
    state: u64,
) {
    if state != 0 {
        let state = unsafe { Box::from_raw(state as usize as *mut State) };
        drop(state);
    }
}

/// Return the high bits of the UUID associated with the specified node.
///
/// # Safety
///
/// The `metadata` pointer MUST be 0, or a value returned by `Parser.getMetadata`. If it is the
/// latter, `parser.parseInput` MUST NOT have been called since the call to `getMetadata` that
/// returned the value.
#[allow(unsafe_code)]
#[no_mangle]
pub extern "system" fn Java_org_enso_syntax2_Parser_getUuidHigh(
    _env: JNIEnv,
    _class: JClass,
    metadata: u64,
    code_offset: u64,
    code_length: u64,
) -> u64 {
    get_uuid(metadata, code_offset, code_length).0
}

/// Return the low bits of the UUID associated with the specified node.
///
/// # Safety
///
/// The `metadata` pointer MUST be 0, or a value returned by `Parser.getMetadata`. If it is the
/// latter, `parser.parseInput` MUST NOT have been called since the call to `getMetadata` that
/// returned the value.
#[allow(unsafe_code)]
#[no_mangle]
pub extern "system" fn Java_org_enso_syntax2_Parser_getUuidLow(
    _env: JNIEnv,
    _class: JClass,
    metadata: u64,
    code_offset: u64,
    code_length: u64,
) -> u64 {
    get_uuid(metadata, code_offset, code_length).1
}

#[allow(unsafe_code)]
fn get_uuid(metadata: u64, code_offset: u64, code_length: u64) -> (u64, u64) {
    if metadata == 0 {
        return (0, 0);
    }
    let metadata = unsafe { &*(metadata as usize as *const enso_parser::metadata::Metadata) };
    let data = metadata.get_uuid(code_offset as usize, code_length as usize);
    match data {
        Some(uuid) => uuid.as_u64_pair(),
        None => (0, 0),
    }
}



// ====================
// === Parser state ===
// ====================

#[derive(Default, Debug)]
struct State {
    base:     u64,
    output:   Vec<u8>,
    metadata: Option<enso_parser::metadata::Metadata>,
}
