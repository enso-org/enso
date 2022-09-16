//! Given a [`java`] representation of a data model, produce a [`java::syntax`] tree that can be
//! rendered to Java code implementing the data model.

use crate::java::*;

use std::fmt::Write;



// ===================================
// === Implementing Java Datatypes ===
// ===================================

/// Produce Java syntax implement all the types modeled in a [`TypeGraph`].
pub fn implement(graph: &TypeGraph, package: &str) -> Vec<syntax::Class> {
    let mut implementations = BTreeMap::new();
    for (id, class) in graph.classes.iter() {
        if !class.builtin {
            implementations.insert(id, implement_class(graph, id));
        }
    }
    for (id, class) in graph.classes.iter() {
        if let Some(parent) = class.parent {
            let mut inner = implementations.remove(&id).unwrap();
            inner.static_ = true;
            implementations.get_mut(&parent).unwrap().nested.push(inner);
        }
    }
    for class in implementations.values_mut() {
        class.package = Some(package.to_owned());
    }
    implementations.into_values().collect()
}

/// For some [`Class`] (identified by ID) in a [`TypeGraph`], get its qualified name, relative to
/// its package. If it is not a nested class, this will be the same as its unqualified name; if it
/// is a nested class, this will include the hierarchy of classes containing it as part of its
/// namespace.
///
/// # Examples
///
/// For a [`Class`] equivalent to the following:
/// ```java
/// class Token {
///     static class Ident { }
/// };
/// ```
/// The `path` would be "Token.Ident".
///
/// For a non-nested [`Class`], like this:
/// ```java
/// class Error {
///     String message;
/// };
/// ```
/// The `path` would be "Error".
pub fn path(graph: &TypeGraph, id: ClassId) -> String {
    let mut components = vec![];
    let mut next_id = Some(id);
    while let Some(id) = next_id {
        let ty = &graph[id];
        components.push(ty.name.as_str());
        next_id = ty.parent;
    }
    components.reverse();
    components.join(".")
}

/// Get the fields owned by a class, including its own fields and the fields of its supertypes.
pub fn class_fields<'v, 's: 'v, 'c: 'v>(graph: &'s TypeGraph, class: &'c Class) -> Vec<&'v Field> {
    let mut out = vec![];
    class_fields_(graph, class, &mut out, None, None);
    out
}

fn class_fields_<'v, 's: 'v, 'c: 'v>(
    graph: &'s TypeGraph,
    class: &'c Class,
    out: &mut Vec<&'v Field>,
    start: Option<usize>,
    end: Option<usize>,
) {
    let mut fields = &class.fields[..];
    if let Some(end) = end {
        fields = &fields[..end];
    }
    if let Some(start) = start {
        fields = &fields[start..];
    } else if let Some(parent) = class.parent {
        let index = Some(graph[parent].child_field.unwrap());
        class_fields_(graph, &graph[parent], out, None, index);
        out.extend(fields);
        class_fields_(graph, &graph[parent], out, index, None);
        return;
    }
    out.extend(fields);
}

pub fn class_fields_split<'v, 's: 'v, 'c: 'v>(
    graph: &'s TypeGraph,
    class: &'c Class,
) -> Option<(Vec<&'v Field>, Vec<&'v Field>)> {
    let index = Some(class.child_field?);
    let mut pre = vec![];
    let mut post = vec![];
    class_fields_(graph, class, &mut pre, None, index);
    class_fields_(graph, class, &mut post, index, None);
    Some((pre, post))
}

/// Given a [`TypeGraph`] and a definition of a field's contents ([`FieldData`]), produce what is
/// referred to in the Java AST specification as an an `UnannType`[1]. This value is suitable for
/// use as the type portion of a field declaration, local variable declaration, formal parameter, or
/// return type specification.
///
/// [1]: https://docs.oracle.com/javase/specs/jls/se18/html/jls-8.html#jls-UnannType
pub fn quote_type(graph: &TypeGraph, data: &FieldData) -> syntax::Type {
    let class = match data {
        FieldData::Object { type_, .. } => return quote_class_type(graph, *type_),
        FieldData::Primitive(Primitive::Int { .. }) => "int",
        FieldData::Primitive(Primitive::Bool) => "boolean",
        FieldData::Primitive(Primitive::Long { .. }) => "long",
    };
    syntax::Type::named(class)
}

/// Given a [`TypeGraph`] and an ID identifying a [`Class`], produce what is referred to in the Java
/// AST specification as an an `UnannClassOrInterfaceType`[1]. This value is suitable for
/// use anywhere an `UnannType`[2] is expected.
///
/// [1]: https://docs.oracle.com/javase/specs/jls/se18/html/jls-8.html#jls-UnannClassOrInterfaceType
/// [2]: https://docs.oracle.com/javase/specs/jls/se18/html/jls-8.html#jls-UnannType
pub fn quote_class_type(graph: &TypeGraph, id: ClassId) -> syntax::Type {
    let class = path(graph, id);
    let params = quote_params(graph, &graph[id].params);
    syntax::Type { class, params }
}

/// Render a parameter list.
pub fn quote_params<'a>(
    graph: &TypeGraph,
    params: impl IntoIterator<Item = &'a ClassId>,
) -> Vec<String> {
    params.into_iter().map(|ty| path(graph, *ty)).collect()
}


// === Helpers ===

/// Given a model of a field ([`Field`]), create a representation of the Java syntax defining a
/// class field with name, type, and attributes as specified in the model.
fn quote_field(graph: &TypeGraph, field: &Field) -> syntax::Field {
    let Field { name, data, id: _, hide_in_tostring: _ } = field;
    let type_ = quote_type(graph, data);
    let name = name.clone();
    let final_ = true;
    syntax::Field { type_, name, final_ }
}

/// Given a model of a method ([`Method`]), create a representation of the Java syntax implementing
/// the method.
fn method(graph: &TypeGraph, method: &Method, class: &Class) -> syntax::Method {
    match method {
        Method::Dynamic(method) => implement_method(graph, method, class),
        Method::Raw(method) => method.clone(),
    }
}

/// Produce a representation of Java syntax implementing the specified [`Dynamic`] method, for the
/// specified [`Class`] within the specified [`TypeGraph`].
fn implement_method(graph: &TypeGraph, method: &Dynamic, class: &Class) -> syntax::Method {
    match method {
        Dynamic::Constructor => implement_constructor(graph, class),
        Dynamic::HashCode => implement_hash_code(graph, class),
        Dynamic::Equals => implement_equals(graph, class),
        Dynamic::ToString => implement_to_string(graph, class),
        Dynamic::Getter(id) => {
            let field = class.fields.iter().find(|field| field.id() == *id).unwrap();
            getter(graph, field, &field.name)
        }
        Dynamic::GetterNamed(field, name) => implement_getter(graph, class, *field, name),
    }
}

/// Produce a representation of Java syntax implementing a constructor for the given [`Class`].
///
/// The constructor will accept a value for each of its fields, and for all fields of any classes
/// it extends, in an order that matches the order they appear in serialized formats.
///
/// For all field that have the `non_null` property sets (see [`FieldData`]), the constructor will
/// produce `requireNonNull`[1] statements validating the corresponding inputs.
///
/// [1]: https://docs.oracle.com/javase/8/docs/api/java/util/Objects.html#requireNonNull-T-
fn implement_constructor(graph: &TypeGraph, class: &Class) -> syntax::Method {
    let suffix = "__GeneratedArgument";
    let arguments = class_fields(graph, class)
        .into_iter()
        .map(|field| (quote_type(graph, &field.data), format!("{}{}", &field.name, &suffix)))
        .collect();
    let mut body = vec![];
    if let Some(parent) = class.parent {
        let suffix = |field: &Field| format!("{}{}", &field.name, &suffix);
        let fields: Vec<_> = class_fields(graph, &graph[parent]).into_iter().map(suffix).collect();
        body.push(format!("super({});", fields.join(", ")));
    }
    for field in &class.fields {
        if let FieldData::Object { non_null: true, .. } = &field.data {
            body.push(format!("java.util.Objects.requireNonNull({}{});", &field.name, &suffix));
        }
    }
    let own_field_initializers =
        class.fields.iter().map(|field| format!("{} = {}{};", &field.name, &field.name, &suffix));
    body.extend(own_field_initializers);
    let mut method = syntax::Method::constructor(class.name.clone());
    method.arguments = arguments;
    method.body = body.join("\n");
    method
}

/// Produce a representation of Java syntax implementing a method overriding `Object.hashCode`[1]
/// for the specified [`Class`].
///
/// The implementation will pass all fields of the class, and of any superclasses, to
/// `java.util.Objects.hash`[2] and return the result.
///
/// [1]: https://docs.oracle.com/javase/7/docs/api/java/lang/Object.html#hashCode()
/// [2]: https://docs.oracle.com/javase/8/docs/api/java/util/Objects.html#hash-java.lang.Object...-
fn implement_hash_code(graph: &TypeGraph, class: &Class) -> syntax::Method {
    let fields: Vec<_> =
        class_fields(graph, class).into_iter().map(|field| field.name.as_str()).collect();
    let fields = fields.join(", ");
    let body = format!("return java.util.Objects.hash({});", fields);
    let return_ = FieldData::Primitive(Primitive::Int { unsigned: false });
    let return_ = quote_type(graph, &return_);
    let mut method = syntax::Method::new("hashCode", return_);
    method.override_ = true;
    method.body = body;
    method
}

/// Produce a representation of Java syntax implementing a method overriding `Object.equals`[1]
/// for the specified [`Class`].
///
/// The implementation:
/// - Returns `true` if the objects are identity-equal.
/// - Returns `false` if the other object is not of the same type as this object.
/// Otherwise, returns a boolean-and of a field-by-field comparison:
/// - Primitive fields are compared with `==`.
/// - Reference-type fields are compared with `Object.equals`.
///
/// [1]: https://docs.oracle.com/javase/7/docs/api/java/lang/Object.html#equals(java.lang.Object)
fn implement_equals(graph: &TypeGraph, class: &Class) -> syntax::Method {
    let object = "object";
    let that = "that";
    let compare =
        |field: &Field| field.data.fmt_equals(&field.name, &format!("{that}.{}", &field.name));
    let field_comparisons = class_fields(graph, class).into_iter().map(compare);
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
    let mut method = syntax::Method::new("equals", return_);
    method.override_ = true;
    method.arguments = vec![(syntax::Type::named("Object"), object.to_string())];
    method.body = body.join("\n");
    method
}

/// Produce a representation of Java syntax implementing a method overriding `Object.toString`[1]
/// for the specified [`Class`].
///
/// The generated `toString` formats all the object's fields in the same manner as would be done by
/// a Java `record`[2] with the same fields.
///
/// [1]: https://docs.oracle.com/javase/7/docs/api/java/lang/Object.html#toString()
/// [2]: https://openjdk.org/jeps/395
fn implement_to_string(graph: &TypeGraph, class: &Class) -> syntax::Method {
    let string_builder = "stringBuilder";
    let fields_ = class_fields(graph, class);
    let mut fields = Vec::with_capacity(fields_.len());
    for field in fields_ {
        if !field.hide_in_tostring {
            fields.push(format!("{string_builder}.append(String.valueOf({}));", field.name));
        }
    }
    let mut body = String::new();
    let ty_name = &class.name;
    writeln!(body, "StringBuilder {string_builder} = new StringBuilder();").unwrap();
    writeln!(body, "{string_builder}.append(\"{ty_name}[\");").unwrap();
    let mut class = Some(class);
    while let Some(class_) = class {
        for expr in &class_.tostring_prefix_fields {
            writeln!(body, "{string_builder}.append({expr});").unwrap();
            writeln!(body, "{string_builder}.append(\", \");").unwrap();
        }
        class = class_.parent.map(|id| &graph[id]);
    }
    writeln!(body, "{}", fields.join(&format!("\n{string_builder}.append(\", \");\n"))).unwrap();
    writeln!(body, "{string_builder}.append(\"]\");").unwrap();
    writeln!(body, "return {string_builder}.toString();").unwrap();
    let return_ = syntax::Type::named("String");
    let mut method = syntax::Method::new("toString", return_);
    method.override_ = true;
    method.body = body;
    method
}

/// Produce a representation of Java syntax implementing a method returning the value of a field
/// (identified by [`FieldId`]) of the specified [`Class`].
fn implement_getter(graph: &TypeGraph, class: &Class, id: FieldId, name: &str) -> syntax::Method {
    let field = class.fields.iter().find(|field| field.id() == id).unwrap();
    getter(graph, field, name)
}

/// Produce a representation of Java syntax implementing a method returning the value of the
/// specified [`Field`]. The method must be attached to the same [`syntax::Class`] in which the
/// [`Field`] is defined.
fn getter(graph: &TypeGraph, field: &Field, name: &str) -> syntax::Method {
    let type_ = quote_type(graph, &field.data);
    let mut method = syntax::Method::new(name, type_);
    method.body = format!("return {};", &field.name);
    method
}

/// Produce a representation of Java syntax defining a `class` as specified by the given [`Class`]
/// (identified by its [`ClassId`]).
fn implement_class(graph: &TypeGraph, id: ClassId) -> syntax::Class {
    let class = &graph[id];
    let name = class.name.clone();
    let abstract_ = class.abstract_;
    let final_ = !abstract_;
    let static_ = false;
    let parent = class.parent.map(|id| quote_class_type(graph, id));
    let fields = class.fields.iter().map(|field| quote_field(graph, field)).collect();
    let nested = vec![];
    let methods = class.methods.iter().map(|m| method(graph, m, class)).collect();
    let package = Default::default();
    let sealed = class.sealed.then(Default::default);
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
