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
const TREE_BEGIN: &str = "fieldSpanLeftOffsetCodeReprBegin";
const TREE_LEN: &str = "fieldSpanLeftOffsetCodeReprLen";
const TOKEN_BEGIN: &str = "fieldCodeReprBegin";
const TOKEN_LEN: &str = "fieldCodeReprLen";
const TOKEN_OFFSET_BEGIN: &str = "fieldLeftOffsetCodeReprBegin";
//const TOKEN_OFFSET_LEN: &str = "fieldLeftOffsetCodeReprLen";

/// Derive deserialization for all types in the typegraph.
pub fn derive(graph: &mut TypeGraph, tree: ClassId, token: ClassId, unsupported: ClassId) {
    let source = "source";
    impl_deserialize(graph, tree, token, unsupported, source);
    graph[token].methods.push(impl_getter(CODE_GETTER, source, TOKEN_BEGIN, TOKEN_LEN));
    graph[tree].methods.push(impl_getter(CODE_GETTER, source, TREE_BEGIN, TREE_LEN));
}


// === Deserialization Methods ===

fn impl_deserialize(
    graph: &mut TypeGraph,
    tree: ClassId,
    token: ClassId,
    unsupported: ClassId,
    source: &str,
) {
    // Add source field to parent types.
    let buffer = Class::builtin("java.nio.ByteBuffer", vec![]);
    let buffer = graph.classes.insert(buffer);
    let mut tree_source_ = Field::object(source, buffer, true);
    tree_source_.hide_in_tostring();
    let tree_source = tree_source_.id();
    graph[tree].fields.push(tree_source_);
    let mut token_source_ = Field::object(source, buffer, true);
    token_source_.hide_in_tostring();
    let token_source = token_source_.id();
    graph[token].fields.push(token_source_);
    // Add UUIDs to types
    let uuid = Class::builtin("java.util.UUID", vec![]);
    let uuid = graph.classes.insert(uuid);
    let tree_id_ = Field::object("uuid", uuid, false);
    let tree_id = tree_id_.id();
    graph[tree].fields.push(tree_id_);
    graph[tree].methods.push(Method::Dynamic(Dynamic::Getter(tree_id)));

    let tree_begin = graph[tree].find_field(TREE_BEGIN).unwrap().id();
    let token_begin = graph[token].find_field(TOKEN_BEGIN).unwrap().id();
    let token_offset_begin = graph[token].find_field(TOKEN_OFFSET_BEGIN).unwrap().id();
    let ids: Vec<_> = graph.classes.keys().collect();
    for id in ids {
        let class = &graph[id];
        let mut deserialization =
            bincode::DeserializerBuilder::new(id, crate::SERIALIZATION_SUPPORT, crate::EITHER_TYPE);
        if id == unsupported {
            deserialization.pre_hook(|bincode::HookInput { message }| {
                format!("{message}.markEncounteredUnsupportedSyntax();\n")
            });
        }
        if class.parent == Some(tree) {
            deserialization.materialize(tree_source, context_materializer());
            deserialization.materialize(tree_id, uuid_materializer());
        }
        if class.parent == Some(token) {
            deserialization.materialize(token_source, context_materializer());
        }
        deserialization.map(tree_begin, offset_mapper());
        deserialization.map(token_begin, offset_mapper());
        deserialization.map(token_offset_begin, offset_mapper());
        let deserializer = deserialization.build(graph);
        graph[id].methods.push(deserializer);
    }
}

fn context_materializer() -> impl for<'a> FnOnce(MaterializerInput<'a>) -> String + 'static {
    |MaterializerInput { message }| format!("{message}.context()")
}
fn offset_mapper() -> impl for<'a, 'b> FnOnce(MapperInput<'a, 'b>) -> String + 'static {
    |MapperInput { message, value }| format!("{message}.offset({value})")
}

fn uuid_materializer() -> impl for<'a> FnOnce(MaterializerInput<'a>) -> String + 'static {
    |MaterializerInput { message }| format!("{message}.getUuid({TREE_BEGIN}, {TREE_LEN})")
}


// === Source Code Getters ===

fn impl_getter(name: &str, buffer: &str, begin: &str, len: &str) -> Method {
    use std::fmt::Write;
    let mut body = String::new();
    let serialization = crate::SERIALIZATION_SUPPORT;
    let exception = format!("{serialization}.FormatException");
    writeln!(body, "byte[] dst = new byte[{len}];").unwrap();
    writeln!(body, "if ({len} != 0) {{").unwrap();
    writeln!(body, "    {buffer}.position({begin});").unwrap();
    writeln!(body, "    {buffer}.get(dst);").unwrap();
    writeln!(body, "}}").unwrap();
    writeln!(body, "try {{").unwrap();
    writeln!(body, "    return new String(dst, \"UTF-8\");").unwrap();
    writeln!(body, "}} catch (java.io.UnsupportedEncodingException e) {{").unwrap();
    writeln!(body, "    throw new {exception}(\"Expected UTF-8\", e);").unwrap();
    writeln!(body, "}}").unwrap();
    let mut method = syntax::Method::new(name, syntax::Type::named("String"));
    method.body = body;
    Method::Raw(method)
}
