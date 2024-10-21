//! Java interface to [`enso_parser`].

// === Non-Standard Linter Configuration ===
#![allow(clippy::option_map_unit_fn)]
#![allow(clippy::precedence)]
#![allow(dead_code)]
#![deny(unconditional_recursion)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(unused_qualifications)]

use enso_prelude::*;

use enso_parser::macros::resolver::RootContext;
use jni::objects::JByteBuffer;
use jni::objects::JClass;
use jni::sys::jobject;
use jni::sys::jstring;
use jni::JNIEnv;



// ======================
// === Java Interface ===
// ======================

static DIRECT_ALLOCATED: &str = "Internal Error: ByteBuffer must be direct-allocated.";
static FAILED_SERIALIZE_AST: &str = "Failed to serialize AST to binary format.";

/// Parse the input as a module. Returns a serialized representation of the parse tree.
///
/// # Safety
///
/// The state MUST be a value returned by `allocState` that has not been passed to `freeState`.
/// The input buffer contents MUST be valid UTF-8.
/// The contents of the returned buffer MUST not be accessed after another call to `parseInput`, or
/// a call to `freeState`.
#[allow(unsafe_code)]
#[no_mangle]
pub extern "system" fn Java_org_enso_syntax2_Parser_parseModule(
    env: JNIEnv,
    class: JClass,
    state: u64,
    input: JByteBuffer,
) -> jobject {
    parse(env, class, state, input, RootContext::Module)
}

/// Parse the input as a block. Returns a serialized representation of the parse tree.
///
/// # Safety
///
/// The state MUST be a value returned by `allocState` that has not been passed to `freeState`.
/// The input buffer contents MUST be valid UTF-8.
/// The contents of the returned buffer MUST not be accessed after another call to `parseInput`, or
/// a call to `freeState`.
#[allow(unsafe_code)]
#[no_mangle]
pub extern "system" fn Java_org_enso_syntax2_Parser_parseBlock(
    env: JNIEnv,
    class: JClass,
    state: u64,
    input: JByteBuffer,
) -> jobject {
    parse(env, class, state, input, RootContext::Block)
}

#[allow(unsafe_code)]
fn parse(
    mut env: JNIEnv,
    _class: JClass,
    state: u64,
    input: JByteBuffer,
    root_context: RootContext,
) -> jobject {
    let state = unsafe { &mut *(state as usize as *mut State) };
    let input = unsafe { decode_utf8_buffer(&env, &input) };
    let mut code = input;
    let mut meta = None;
    if let Some((meta_, code_)) = enso_parser::metadata::parse(input) {
        match meta_ {
            Ok(meta_) => meta = Some(meta_),
            Err(e) => eprintln!("Ignoring invalid metadata: {e}."),
        }
        code = code_;
    }
    state.base = str::as_ptr(code) as usize as u64;
    let parser = enso_parser::Parser::new();
    let tree = match root_context {
        RootContext::Module => parser.parse_module(code),
        RootContext::Block => parser.parse_block(code),
    };
    state.output = enso_parser::serialization::serialize_tree(&tree).unwrap_or_else(|_| {
        // `Tree` does not contain any types with fallible `serialize` implementations, so this
        // cannot fail.
        debug_assert!(false);
        default()
    });
    state.metadata = meta;
    let result =
        unsafe { env.new_direct_byte_buffer(state.output.as_mut_ptr(), state.output.len()) };
    result.unwrap().into_raw()
}

/// Parse a module. Returns a serialize format compatible with a lazy deserialization strategy.
///
/// # Safety
///
/// The state MUST be a value returned by `allocState` that has not been passed to `freeState`.
/// The input buffer contents MUST be valid UTF-8.
/// The contents of the returned buffer MUST NOT be accessed after another call to `parseInput`, or
/// a call to `freeState`.
#[allow(unsafe_code)]
#[no_mangle]
pub extern "system" fn Java_org_enso_syntax2_Parser_parseModuleLazy(
    mut env: JNIEnv,
    _class: JClass,
    state: u64,
    input: JByteBuffer,
) -> jobject {
    let state = unsafe { &mut *(state as usize as *mut State) };
    let input = unsafe { decode_utf8_buffer(&env, &input) };

    let tree = enso_parser::Parser::new().parse_module(input);
    state.output = enso_parser::format::serialize(&tree).expect(FAILED_SERIALIZE_AST);

    let result =
        unsafe { env.new_direct_byte_buffer(state.output.as_mut_ptr(), state.output.len()) };
    result.unwrap().into_raw()
}

/// Parse a block. Returns a serialize format compatible with a lazy deserialization strategy.
///
/// # Safety
///
/// The state MUST be a value returned by `allocState` that has not been passed to `freeState`.
/// The input buffer contents MUST be valid UTF-8.
/// The contents of the returned buffer MUST NOT be accessed after another call to `parseInput`, or
/// a call to `freeState`.
#[allow(unsafe_code)]
#[no_mangle]
pub extern "system" fn Java_org_enso_syntax2_Parser_parseBlockLazy(
    mut env: JNIEnv,
    _class: JClass,
    state: u64,
    input: JByteBuffer,
) -> jobject {
    let state = unsafe { &mut *(state as usize as *mut State) };
    let input = unsafe { decode_utf8_buffer(&env, &input) };

    let tree = enso_parser::Parser::new().parse_block(input);
    state.output = enso_parser::format::serialize(&tree).expect(FAILED_SERIALIZE_AST);

    let result =
        unsafe { env.new_direct_byte_buffer(state.output.as_mut_ptr(), state.output.len()) };
    result.unwrap().into_raw()
}

/// Determine the token variant of the provided input.
#[allow(unsafe_code)]
#[no_mangle]
pub extern "system" fn Java_org_enso_syntax2_Parser_isIdentOrOperator(
    env: JNIEnv,
    _class: JClass,
    input: JByteBuffer,
) -> u64 {
    let input = unsafe { decode_utf8_buffer(&env, &input) };

    let parsed = enso_parser::lexer::run(input);
    if parsed.internal_error.is_some() {
        return 0;
    }
    let token = match &parsed.value[..] {
        [token] => token,
        _ => return 0,
    };
    match &token.variant {
        enso_parser::syntax::token::Variant::Ident(_) => 1,
        enso_parser::syntax::token::Variant::Operator(_) => 2,
        _ => 0,
    }
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
#[allow(unsafe_code, clippy::box_default)]
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

/// Returns the string template corresponding to the given warning ID.
#[allow(unsafe_code)]
#[no_mangle]
pub extern "system" fn Java_org_enso_syntax2_Parser_getWarningTemplate(
    env: JNIEnv,
    _class: JClass,
    id: u32,
) -> jstring {
    let message =
        enso_parser::syntax::WARNINGS.get(id as usize).copied().unwrap_or("Unknown warning ID");
    env.new_string(message).unwrap().into_raw()
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

/// # Safety
///
/// The input MUST be valid UTF-8.
#[allow(unsafe_code)]
unsafe fn decode_utf8_unchecked(input: &[u8]) -> &str {
    if cfg!(debug_assertions) {
        std::str::from_utf8(input).unwrap()
    } else {
        std::str::from_utf8_unchecked(input)
    }
}

/// # Safety
///
/// The input buffer contents MUST be valid UTF-8.
#[allow(unsafe_code)]
unsafe fn decode_utf8_buffer<'a>(env: &JNIEnv, buffer: &'a JByteBuffer) -> &'a str {
    let ptr = env.get_direct_buffer_address(buffer).expect(DIRECT_ALLOCATED);
    let len = env.get_direct_buffer_capacity(buffer).expect(DIRECT_ALLOCATED);
    let bytes = slice::from_raw_parts(ptr, len);
    decode_utf8_unchecked(bytes)
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

mod static_trait_check {
    fn assert_send<T: Send>() {}
    fn assert_state_send() {
        // Require `State` to be `Send`-safe so that in Java it can be deallocated (or potentially
        // reused) by any thread.
        assert_send::<super::State>()
    }
}
