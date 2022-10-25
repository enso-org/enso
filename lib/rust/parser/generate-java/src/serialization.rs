//! Serialization overrides for the `enso_parser` types.

use enso_metamodel::java::*;

use enso_metamodel::java::bincode::MapperInput;
use enso_metamodel::java::bincode::MaterializerInput;



// ==============================
// === Derive Deserialization ===
// ==============================

// FIXME: After we have implemented a transformation from the raw `Reflect` output to a
//  `rust::TypeGraph`, at which time we can assign unique `FieldId`s: We should identify
//  generated fields in Java classes by starting from a `str -> rust::FieldId` query on Rust
//  type data, and mapping fields analogously to `rust_to_java` for types.
const CODE_GETTER: &str = "codeRepr";
const WHITESPACE_GETTER: &str = "getWhitespace";
const TREE_BEGIN: &str = "fieldSpanLeftOffsetCodeReprBegin";
const TREE_LEN: &str = "fieldSpanLeftOffsetCodeReprLen";

/// Derive deserialization for all types in the typegraph.
pub fn derive(graph: &mut TypeGraph, tree: ClassId, token: ClassId) {
    let source = "source";
    impl_deserialize(graph, tree, token, source);
    graph[token].methods.push(impl_getter(CODE_GETTER));
    graph[token].methods.push(impl_whitespace_getter(WHITESPACE_GETTER));
    graph[tree].methods.push(impl_getter(CODE_GETTER));
    graph[tree].methods.push(impl_whitespace_getter(WHITESPACE_GETTER));
}


// === Deserialization Methods ===

fn impl_deserialize(graph: &mut TypeGraph, tree: ClassId, token: ClassId, source: &str) {
    // Add UUIDs to types
    let uuid = Class::builtin("java.util.UUID", vec![]);
    let uuid = graph.classes.insert(uuid);
    let mut tree_id_ = Field::object("uuid", uuid, false);
    tree_id_.hide_in_tostring();
    let tree_id = tree_id_.id();
    graph[tree].fields.push(tree_id_);
    *graph[tree].child_field.as_mut().unwrap() += 1;
    graph[tree].methods.push(Method::Dynamic(Dynamic::Getter(tree_id)));

    // Add source field to parent types.
    let long = Primitive::Long { unsigned: true };
    let mut tree_start_whitespace_ = Field::primitive("startWhitespace", long);
    let mut tree_start_code_ = Field::primitive("startCode", long);
    let mut tree_end_code_ = Field::primitive("endCode", long);
    tree_start_whitespace_.hide_in_tostring();
    tree_start_code_.hide_in_tostring();
    tree_end_code_.hide_in_tostring();
    let tree_start_whitespace = tree_start_whitespace_.id();
    let tree_start_code = tree_start_code_.id();
    let tree_end_code = tree_end_code_.id();
    graph[tree].fields.push(tree_start_whitespace_);
    graph[tree].fields.push(tree_start_code_);
    *graph[tree].child_field.as_mut().unwrap() += 2;
    graph[tree].fields.push(tree_end_code_);

    let mut token_start_whitespace_ = Field::primitive("startWhitespace", long);
    let mut token_start_code_ = Field::primitive("startCode", long);
    let mut token_end_code_ = Field::primitive("endCode", long);
    token_start_whitespace_.hide_in_tostring();
    token_start_code_.hide_in_tostring();
    token_end_code_.hide_in_tostring();
    let token_start_whitespace = token_start_whitespace_.id();
    let token_start_code = token_start_code_.id();
    let token_end_code = token_end_code_.id();
    let index = graph[token].child_field.unwrap();
    graph[token].fields.insert(index, token_start_whitespace_);
    graph[token].fields.insert(index + 1, token_start_code_);
    graph[token].fields.push(token_end_code_);
    *graph[token].child_field.as_mut().unwrap() += 3;

    // Getters
    let token_getters = [
        Dynamic::getter_named(token_start_whitespace, "getStartWhitespace"),
        Dynamic::getter_named(token_start_code, "getStartCode"),
        Dynamic::getter_named(token_end_code, "getEndCode"),
    ];
    let tree_getters = [
        Dynamic::getter_named(tree_start_whitespace, "getStartWhitespace"),
        Dynamic::getter_named(tree_start_code, "getStartCode"),
        Dynamic::getter_named(tree_end_code, "getEndCode"),
    ];
    graph[token].methods.extend(token_getters.map(Method::Dynamic));
    graph[tree].methods.extend(tree_getters.map(Method::Dynamic));

    graph[tree].tostring_prefix_fields.push(
        r#""\"" + source.subSequence((int)startWhitespace, (int)startCode) + "\"""#.to_string(),
    );
    graph[tree]
        .tostring_prefix_fields
        .push(r#""\"" + source.subSequence((int)startCode, (int)endCode) + "\"""#.to_string());
    graph[token].tostring_prefix_fields.push(
        r#""\"" + source.subSequence((int)startWhitespace, (int)startCode) + "\"""#.to_string(),
    );
    graph[token]
        .tostring_prefix_fields
        .push(r#""\"" + source.subSequence((int)startCode, (int)endCode) + "\"""#.to_string());

    let buffer = Class::builtin("CharSequence", vec![]);
    let buffer = graph.classes.insert(buffer);
    let mut tree_source_ = Field::object(source, buffer, true);
    tree_source_.hide_in_tostring();
    let tree_source = tree_source_.id();
    graph[tree].fields.push(tree_source_);
    let mut token_source_ = Field::object(source, buffer, true);
    token_source_.hide_in_tostring();
    let token_source = token_source_.id();
    graph[token].fields.push(token_source_);
    *graph[token].child_field.as_mut().unwrap() += 1;

    let tree_begin = graph[tree].find_field(TREE_BEGIN).unwrap().id();
    let ids: Vec<_> = graph.classes.keys().collect();
    for id in ids {
        let class = &graph[id];
        let mut deserialization =
            bincode::DeserializerBuilder::new(id, crate::SERIALIZATION_SUPPORT, crate::EITHER_TYPE);
        if id == tree || class.parent == Some(tree) {
            deserialization.materialize(tree_source, context_materializer());
            deserialization.materialize(tree_id, uuid_materializer());
            deserialization.materialize(tree_start_whitespace, start_whitespace());
            deserialization.materialize(tree_start_code, start_code_tree());
            deserialization.materialize(tree_end_code, end_code_tree());
        }
        if id == token || class.parent == Some(token) {
            deserialization.materialize(token_source, context_materializer());
            deserialization.materialize(token_start_whitespace, start_whitespace());
            deserialization.materialize(token_start_code, start_code_token());
            deserialization.materialize(token_end_code, end_code_token());
        }
        deserialization.map(tree_begin, offset_mapper());
        let deserializer = deserialization.build(graph);
        graph[id].methods.push(deserializer);
    }
}

fn offset_mapper() -> impl for<'a, 'b> Fn(MapperInput<'a, 'b>) -> String + 'static {
    |MapperInput { message, value }| format!("{message}.offset({value})")
}

fn context_materializer() -> impl for<'a> Fn(MaterializerInput<'a>) -> String + 'static {
    |MaterializerInput { message }| format!("{message}.context()")
}
fn uuid_materializer() -> impl for<'a> Fn(MaterializerInput<'a>) -> String + 'static {
    |MaterializerInput { message }| format!("{message}.getUuid({TREE_BEGIN}, {TREE_LEN})")
}
fn start_whitespace() -> impl for<'a> Fn(MaterializerInput<'a>) -> String + 'static {
    |MaterializerInput { message }| format!("{message}.position()")
}
fn start_code_tree() -> impl for<'a> Fn(MaterializerInput<'a>) -> String + 'static {
    |MaterializerInput { message }| format!("{message}.advance(fieldSpanLeftOffsetCodeUtf16)")
}
fn end_code_tree() -> impl for<'a> Fn(MaterializerInput<'a>) -> String + 'static {
    |MaterializerInput { message }| format!("{message}.position()")
}
fn start_code_token() -> impl for<'a> Fn(MaterializerInput<'a>) -> String + 'static {
    |MaterializerInput { message }| format!("{message}.advance(fieldLeftOffsetCodeUtf16)")
}
fn end_code_token() -> impl for<'a> Fn(MaterializerInput<'a>) -> String + 'static {
    |MaterializerInput { message }| format!("{message}.advance(fieldCodeUtf16)")
}


// === Source Code Getters ===

fn impl_getter(name: &str) -> Method {
    let mut method = syntax::Method::new(name, syntax::Type::named("String"));
    method.body =
        "return source.subSequence((int)startCode, (int)endCode).toString();\n".to_string();
    Method::Raw(method)
}

fn impl_whitespace_getter(name: &str) -> Method {
    let mut method = syntax::Method::new(name, syntax::Type::named("CharSequence"));
    method.body = "return source.subSequence((int)startWhitespace, (int)startCode);\n".to_string();
    Method::Raw(method)
}
