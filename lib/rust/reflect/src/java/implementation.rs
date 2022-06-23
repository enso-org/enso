//! Generation of Java syntax implementing datatypes.

use crate::java::*;



// ======================
// === Implementation ===
// ======================

use crate::java::TypeGraph;

/// Implement a whole system of datatypes.
pub fn implement(graph: &TypeGraph) -> Vec<syntax::Class> {
    let mut implementations = BTreeMap::new();
    for (i, class) in graph.types.iter().enumerate() {
        if let Some(class) = class {
            if !class.builtin {
                let id = TypeId(i);
                implementations.insert(id, implement_class(graph, id));
            }
        }
    }
    for (i, class) in graph.types.iter().enumerate() {
        if let Some(class) = class {
            let id = TypeId(i);
            if let Some(parent) = class.parent {
                let mut inner = implementations.remove(&id).unwrap();
                inner.static_ = true;
                implementations.get_mut(&parent).unwrap().nested.push(inner);
            }
        }
    }
    for class in implementations.values_mut() {
        class.package = Some("out".to_owned());
    }
    implementations.into_values().collect()
}

/// Get a type's qualified name, relative to the generated package.
pub fn path(graph: &TypeGraph, id: TypeId) -> String {
    let mut components = vec![];
    let mut next_id = Some(id);
    while let Some(id) = next_id {
        let ty = &graph[id];
        components.push(ty.name.as_str());
        next_id = ty.parent.clone();
    }
    components.reverse();
    components.join(".")
}

fn class_fields_<'v, 's: 'v, 'c: 'v>(graph: &'s TypeGraph, class: &'c Class, out: &mut Vec<&'v Field>) {
    out.extend(&class.fields);
    if let Some(parent) = class.parent {
        class_fields_(graph, &graph[parent], out);
    }
}

/// Get the fields owned by a class, including its own fields and the fields of its supertypes.
pub fn class_fields<'v, 's: 'v, 'c: 'v>(graph: &'s TypeGraph, class: &'c Class) -> Vec<&'v Field> {
    let mut out = vec![];
    class_fields_(graph, class, &mut out);
    out
}

/// Produce syntax referring to the given type.
pub fn quote_type(graph: &TypeGraph, data: &FieldData) -> syntax::Type {
    let params = vec![];
    let class = match data {
        FieldData::Object { type_, .. } => return quote_class_type(graph, *type_),
        FieldData::Primitive(Primitive::Int { .. }) => "int".to_owned(),
        FieldData::Primitive(Primitive::Bool) => "boolean".to_owned(),
        FieldData::Primitive(Primitive::Long { .. }) => "long".to_owned(),
    };
    syntax::Type { class, params }
}

/// Produce syntax referring to the given class.
pub fn quote_class_type(graph: &TypeGraph, id: TypeId) -> syntax::Type {
    let class = path(graph, id);
    let params = graph[id].params.iter().map(|ty| path(graph, *ty)).collect();
    syntax::Type { class, params }
}

fn quote_field(graph: &TypeGraph, field: &Field) -> syntax::Field {
    let Field { name, data, getter: _ } = field;
    let type_ = quote_type(graph, data);
    let name = name.clone();
    let final_ = true;
    syntax::Field { type_, name, final_ }
}

fn method(graph: &TypeGraph, method: &Method, class: &Class) -> syntax::Method {
    match method {
        Method::Dynamic(method) => implement_method(graph, method, class),
        Method::Raw(method) => method.clone(),
    }
}

fn implement_method(graph: &TypeGraph, method: &Dynamic, class: &Class) -> syntax::Method {
    match method {
        Dynamic::Constructor => implement_constructor(graph, class),
        Dynamic::HashCode => implement_hash_code(graph, class),
        Dynamic::Equals => implement_equals(graph, class),
        Dynamic::ToString => implement_to_string(graph, class),
    }
}

fn implement_constructor(graph: &TypeGraph, class: &Class) -> syntax::Method {
    let suffix = "__GeneratedArgument";
    let arguments = class_fields(graph, class)
        .into_iter()
        .map(|field| {
            (quote_type(graph, &field.data), format!("{}{}", &field.name, &suffix))
        })
        .collect();
    let mut body = vec![];
    if let Some(parent) = class.parent {
       let fields: Vec<_> = class_fields(graph, &graph[parent])
            .into_iter()
            .map(|field| format!("{}{}", &field.name, &suffix))
            .collect();
        body.push(format!("super({});", fields.join(", ")));
    }
    // TODO?
    //   - could do: java.util.Objects.requireNonNull, but if we only construct by deserialize,
    //     correct-by-construction probably OK
    let own_field_initializers = class
        .fields
        .iter()
        .map(|field| format!("{} = {}{};", &field.name, &field.name, &suffix));
    body.extend(own_field_initializers);
    let mut method = syntax::Method::constructor(class.name.clone());
    method.arguments = arguments;
    method.body = syntax::Body::Verbatim(body);
    method
}

fn implement_hash_code(graph: &TypeGraph, class: &Class) -> syntax::Method {
    let fields: Vec<_> =
        class_fields(graph, class).into_iter().map(|field| field.name.as_str()).collect();
    let fields = fields.join(", ");
    let body = format!("return java.util.Objects.hash({});", fields);
    let return_ = FieldData::Primitive(Primitive::Int { unsigned: false });
    let return_ = quote_type(graph, &return_);
    let mut method = syntax::Method::new("hashCode".to_string(), return_);
    method.override_ = true;
    method.body = syntax::Body::Verbatim(vec![body]);
    method
}

fn implement_equals(graph: &TypeGraph, class: &Class) -> syntax::Method {
    let object = "object";
    let that = "that";
    let field_comparisons =
        class_fields(graph, class).into_iter().map(|field| {
            field.data.fmt_equals(&field.name, &format!("{}.{}", that, &field.name))
        });
    let mut values = vec!["true".to_string()];
    values.extend(field_comparisons);
    let expr = values.join(" && ");
    let body = vec![
        format!("if ({} == this) return true;", &object),
        format!("if (!({} instanceof {})) return false;", &object, &class.name),
        format!("{} {} = ({}){};", &class.name, &that, &class.name, &object),
        format!("return {};", expr),
    ];
    let return_ = FieldData::Primitive(Primitive::Bool);
    let return_ = quote_type(graph, &return_);
    let mut method = syntax::Method::new("equals".to_string(), return_);
    method.override_ = true;
    method.arguments = vec![(syntax::Type::named("Object"), object.to_string())];
    method.body = syntax::Body::Verbatim(body);
    method
}

fn implement_to_string(graph: &TypeGraph, class: &Class) -> syntax::Method {
    let sb = "sb";
    let mut body = vec![
        format!("StringBuilder {} = new StringBuilder();", sb),
        format!("{}.append(\"{}[\");", sb, &class.name),
    ];
    let mut fields = class_fields(graph, class)
        .into_iter()
        .map(|field| format!("{}.append(String.valueOf({}));", sb, field.name.as_str()));
    if let Some(field) = fields.next() {
        body.push(field);
    }
    for field in fields {
        body.push(format!("{}.append(\", \");", sb));
        body.push(field);
    }
    body.push(format!("{}.append(\"]\");", sb));
    body.push(format!("return {}.toString();", sb));
    let return_ = syntax::Type::named("String");
    let mut method = syntax::Method::new("toString".to_string(), return_);
    method.override_ = true;
    method.body = syntax::Body::Verbatim(body);
    method
}

fn implement_getters(graph: &TypeGraph, class: &Class, methods: &mut Vec<syntax::Method>) {
    for field in &class.fields {
        if !field.getter {
            continue;
        }
        let type_ = quote_type(graph, &field.data);
        let mut method = syntax::Method::new(field.name.clone(), type_);
        method.body = syntax::Body::Verbatim(vec![format!("return {};", &field.name)]);
        methods.push(method);
    }
}

fn implement_class(graph: &TypeGraph, id: TypeId) -> syntax::Class {
    let class = &graph[id];
    let name = class.name.clone();
    let abstract_ = class.abstract_;
    let final_ = !abstract_;
    let static_ = false;
    let parent = class.parent.map(|id| quote_class_type(graph, id));
    let fields = class.fields.iter().map(|field| quote_field(graph, field)).collect();
    let nested = vec![];
    let mut methods = class.methods.iter().map(|m| method(graph, m, class)).collect();
    implement_getters(graph, class, &mut methods);
    let package = Default::default();
    let sealed = class.sealed.then(|| Default::default());
    syntax::Class {
        package,
        name,
        abstract_,
        final_,
        static_,
        parent,
        fields,
        methods,
        nested,
        sealed,
    }
}
