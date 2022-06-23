use std::collections::BTreeMap;
use crate::java::*;
use crate::java::implementation::*;



// ==========================
// === Derive Deserialize ===
// ==========================

pub fn derive_deserialize(graph: &mut TypeGraph) {
    let mut impls = BTreeMap::new();
    for (i, class) in graph.types.iter().enumerate() {
        let class = match class {
            Some(class) => class,
            None => continue,
        };
        if class.builtin {
            continue;
        }
        let id = TypeId(i);
        let method = deserialize_class(graph, id);
        impls.insert(i, method);
    }
    for (i, method) in impls {
        graph.types[i].as_mut().unwrap().methods.push(Method::Raw(method));
    }
}

pub fn deserialize_class(graph: &TypeGraph, id: TypeId) -> syntax::Method {
    if graph[id].abstract_ {
        deserialize_abstract(graph, id)
    } else {
        deserialize_concrete(graph, id)
    }
}


// === Sum Types ===

fn deserialize_abstract(graph: &TypeGraph, id: TypeId) -> syntax::Method {
    let class = &graph[id];
    let message = "message";
    let mut n = 0;
    let mut get_temp = |base| {
        let suffix = "GeneratedTemp";
        let result = format!("{}{}{}", base, suffix, n);
        n += 1;
        result
    };
    let mut body = vec![];
    let discriminant = get_temp("discriminant");
    body.push(format!("int {} = {}.get32();", &discriminant, message));
    body.push(format!("switch ({}) {{", &discriminant));
    for (key, id) in &class.discriminants {
        body.push(format!("case {}:", key));
        body.push(format!("return {}.deserialize({});", quote_class_type(graph, *id), message));
    }
    // TODO
    body.push(format!("default: return null; }}"));
    let mut method = syntax::Method::new("deserialize".to_string(), quote_class_type(graph, id));
    method.static_ = true;
    method.body = syntax::Body::Verbatim(body);
    method.arguments = vec![(syntax::Type::named("utils.BincodeMessage"), message.to_owned())];
    method
}


// === Product Types ===

fn deserialize_concrete(graph: &TypeGraph, id: TypeId) -> syntax::Method {
    let class = &graph[id];
    let message = "message";
    let mut body = vec![];
    let mut n = 0;
    let mut get_temp = || {
        let prefix = "generatedTemp";
        let result = format!("{}{}", prefix, n);
        n += 1;
        result
    };
    let fields = class_fields(graph, &class);
    for field in &fields {
        let ty_name = quote_type(graph, &field.data);
        let expr = match &field.data {
            FieldData::Object { type_, nonnull } => {
                if !nonnull {
                    unimplemented!();
                }
                deserialize_object(graph, *type_, message, &field.name, &mut get_temp, &mut body);
                continue;
            }
            FieldData::Primitive(Primitive::Int { .. }) => format!("{}.get32()", message),
            FieldData::Primitive(Primitive::Long { .. }) => format!("{}.get64()", message),
            FieldData::Primitive(Primitive::Bool) => format!("{}.getBoolean()", message),
        };
        body.push(format!("{} {} = {};", ty_name, &field.name, expr));
    }
    let constructor_args: Vec<_> = fields.into_iter().map(|field| field.name.as_str()).collect();
    let constructor_args = constructor_args.join(", ");
    body.push(format!("return new {}({});", &class.name, constructor_args));
    let mut method = syntax::Method::new("deserialize".to_string(), quote_class_type(graph, id));
    method.static_ = true;
    method.body = syntax::Body::Verbatim(body);
    method.arguments = vec![(syntax::Type::named("utils.BincodeMessage"), message.to_owned())];
    method
}

fn deserialize_object<F>(graph: &TypeGraph, id: TypeId, message: &str, output: &str, get_temp: &mut F, body: &mut Vec<String>)
    where F: FnMut() -> String
{
    let ty = &graph[id];
    let ty_name = quote_class_type(graph, id);
    if !ty.builtin {
        body.push(format!("{} {} = {}.deserialize({});", &ty_name, output, &ty_name, message));
        return;
    }
    match ty.name.as_str() {
        "String" => body.push(format!("{} {} = {}.getString();", &ty_name, output, message)),
        "java.util.Optional" => {
            let base = ty.params[0];
            let present = get_temp();
            body.push(format!("{} {};", ty_name, &output));
            body.push(format!("boolean {} = {}.getBoolean();", present, message));
            body.push(format!("if ({}) {{", present));
            let value = get_temp();
            deserialize_object(graph, base, message, &value, get_temp, body);
            body.push(format!("{} = java.util.Optional.of({});", &output, &value));
            body.push(format!("}} else {} = java.util.Optional.empty();", &output));
        },
        "java.util.ArrayList" => {
            let base = ty.params[0];
            let count = get_temp();
            body.push(format!("int {} = (int){}.get64();", count, message));
            body.push(format!("{} {} = new {}({});", ty_name, &output, ty_name, count));
            body.push(format!("for (int i=0; i<{}; i++) {{", count));
            let value = get_temp();
            deserialize_object(graph, base, message, &value, get_temp, body);
            body.push(format!("{}.set(i, {});", &output, value));
            body.push(format!("}}"));
        }
        "utils.Either" => {
            body.push(format!("{} {} = null;", ty_name, &output));
            /*
            let which = get_temp();
            body.push(format!("int {} = {}.get32();", which, message));
            body.push(format!("if ({} == 0) {{", which));
            body.push(format!("}} else if ({} == 1) {{", which));
            // TODO: throw
            body.push(format!("}} else {{ }}"));
             */
        },
        _ => unimplemented!("Deserialize builtin: {}", &ty.name),
    }
}
