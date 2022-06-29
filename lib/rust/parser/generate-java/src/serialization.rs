use enso_reflect::java::*;



// ==============================
// === Derive Deserialization ===
// ==============================

// FIXME: After we have implemented a transformation from the raw `Reflect` output to a
//  `rust::TypeGraph`, at which time we can assign unique `FieldId`s: We should identify
//  generated fields in Java classes by starting from a `str -> rust::FieldId` query on Rust
//  type data, and mapping fields analogously to `rust_to_java` for types.

pub fn derive(graph: &mut TypeGraph, tree: TypeId, token: TypeId) {
    let source = "source";
    impl_deserialize(graph, tree, token, source);
    graph[token].methods.push(impl_getter("codeRepr", source, "codeReprBegin", "codeReprLen"));
    graph[tree].methods.push(impl_getter(
        "leftOffset",
        source,
        "spanLeftOffsetCodeReprBegin",
        "spanLeftOffsetCodeReprLen",
    ));
}


// === Deserialization Methods ===

fn impl_deserialize(graph: &mut TypeGraph, tree: TypeId, token: TypeId, source: &str) {
    // Add source field to parent types.
    let buffer = Class::builtin(&graph, "java.nio.ByteBuffer", vec![]);
    let buffer = graph.insert(buffer);
    let buffer = FieldBuilder::object(source, buffer, true);
    let tree_source = graph.add_field(tree, buffer.clone());
    let token_source = graph.add_field(token, buffer);
    let ids: Vec<_> = graph.type_ids().collect();
    for id in ids {
        let class = &graph[id];
        let mut deserialization = bincode::DeserializerBuilder::new(id);
        match () {
            // Base classes: Map the code repr fields.
            _ if id == tree => {
                let code_begin = class.find_field("spanLeftOffsetCodeReprBegin").unwrap().id;
                deserialization.map(code_begin, |message, raw| format!("{message}.offset({raw})"));
            }
            _ if id == token => {
                let code_begin = class.find_field("codeReprBegin").unwrap().id;
                let offset_begin = class.find_field("leftOffsetCodeReprBegin").unwrap().id;
                deserialization.map(code_begin, |message, raw| format!("{message}.offset({raw})"));
                deserialization
                    .map(offset_begin, |message, raw| format!("{message}.offset({raw})"));
            }
            // Child classes: Pass context object from deserializer to parent.
            _ if class.parent == Some(tree) =>
                deserialization.materialize(tree_source, |message| format!("{message}.context()")),
            _ if class.parent == Some(token) =>
                deserialization.materialize(token_source, |message| format!("{message}.context()")),
            // Everything else: Standard deserialization.
            _ => (),
        }
        let deserializer = deserialization.build(&graph);
        graph[id].methods.push(deserializer);
    }
}


// === Source Code Getters ===

fn impl_getter(name: &str, buffer: &str, begin: &str, len: &str) -> Method {
    use std::fmt::Write;
    let mut body = String::new();
    let exception = "utils.IncompatibleFormatException";
    writeln!(body, "byte[] dst = new byte[{len}];").unwrap();
    writeln!(body, "{buffer}.position({begin});").unwrap();
    writeln!(body, "{buffer}.get(dst);").unwrap();
    writeln!(body, "try {{").unwrap();
    writeln!(body, "    return new String(dst, \"UTF-8\");").unwrap();
    writeln!(body, "}} catch (java.io.UnsupportedEncodingException e) {{").unwrap();
    writeln!(body, "    throw new {exception}(\"Expected UTF-8\", e);").unwrap();
    writeln!(body, "}}").unwrap();
    let mut method = syntax::Method::new(name, syntax::Type::named("String"));
    method.body = body;
    Method::Raw(method)
}
