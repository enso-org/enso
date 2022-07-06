//! Derivation of bincode[1] serialization for Java types.
//! [1]: https://github.com/bincode-org/bincode
//!
//! # Compatibility
//!
//! The generated deserialization methods support the same format as Rust's `serde-bincode` for an
//! analagous tree of types, with the following configuration:
//! ```
//! # let data = &[0u8; 0];
//! use bincode::Options;
//! let options = bincode::DefaultOptions::new().with_fixint_encoding();
//! let serialized = options.serialize(data);
//! ```
//!
//! # Nullability
//!
//! The [`crate::java]` model distinguishes between non-null fields, and fields that may be null.
//! If a field is *not* non-null, or if a type is wrapped in a `java.util.Optional`, whether it's
//! present is encoded compatibly with Rust's `Option` type (i.e. with a 1-byte discriminant).
//!
//! # Basic types
//!
//! Basic types (e.g. integer types, `boolean`, `String`) are encoded compatibly with the
//! corresponding types in Rust.
//!
//! # Sequence types
//!
//! A sequence (e.g. as encoded for a Rust `Vec<T>`) is represented idiomatically in Java:
//! internally its implementation type is `java.util.ArrayList`, but in public interfaces it is
//! exposed as a `java.util.List`.
//!
//! # `Result`
//!
//! In Java, an `Either<Left, Right>` type is used to represent a `Result` as used in Rust. `Either`
//! is similar to `Result`, with the main difference being that the `Ok` case is the `Right` value
//! of an `Either`, and the `Err` case is the `left`.
//!
//! # Overrides
//!
//! The default deserialization can be replaced or modified per-field; see the
//! [`DeserializationBuilder`] interface for details.
//!
//! # Deserialization errors
//!
//! The only runtime error possible is `FormatException`, defined in the Java `serialization`
//! support package; it is a `RuntimeException` rather than a checked exception, as deserialization
//! is extensively tested to succeed for any types that may be serialized in `Rust`.

use crate::java::implementation::*;
use crate::java::*;

use derivative::Derivative;
use std::fmt::Write;



// ==========================
// === Derive Deserialize ===
// ==========================

/// Supports configuring deserialization for a type.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct DeserializerBuilder {
    root:          ClassId,
    #[derivative(Debug = "ignore")]
    materializers: BTreeMap<FieldId, Materializer>,
    #[derivative(Debug = "ignore")]
    mappers:       BTreeMap<FieldId, Mapper>,
    support:       String,
    either_type:   String,
}

impl DeserializerBuilder {
    /// Create a deserializer builder.
    /// - `root`: The type to deserialize.
    /// - `support`: The serialization support package.
    /// - `either_type`: The fully-qualified name of the type that implements `Either`.
    pub fn new(root: ClassId, support: impl Into<String>, either_type: impl Into<String>) -> Self {
        let materializers = Default::default();
        let mappers = Default::default();
        let support = support.into();
        let either_type = either_type.into();
        Self { root, materializers, mappers, support, either_type }
    }

    /// Configure the specified field to be produced according to an expression, instead of by
    /// standard deserialization. The expression will be produced by the given function.
    pub fn materialize<F>(&mut self, field: FieldId, materializer: F)
    where F: for<'a> FnOnce(MaterializerInput<'a>) -> String + 'static {
        self.materializers.insert(field, Box::new(materializer));
    }

    /// Configure the specified field to be modified by an expression, after being deserialized.
    /// The expression will be produced by the given function.
    pub fn map<F>(&mut self, field: FieldId, mapper: F)
    where F: for<'a, 'b> FnOnce(MapperInput<'a, 'b>) -> String + 'static {
        self.mappers.insert(field, Box::new(mapper));
    }

    /// Generate the deserialization method.
    pub fn build(mut self, graph: &TypeGraph) -> Method {
        let method = match graph[self.root].abstract_ {
            true => self.deserialize_abstract(graph),
            false => self.deserialize_concrete(graph),
        };
        Method::Raw(method)
    }
}

type Materializer = Box<dyn for<'a> FnOnce(MaterializerInput<'a>) -> String>;
type Mapper = Box<dyn for<'a, 'b> FnOnce(MapperInput<'a, 'b>) -> String>;

/// Input to a function that produces an expression that deserializes a field.
#[derive(Debug)]
pub struct MaterializerInput<'a> {
    /// Identifier of the serialized message object.
    pub message: &'a str,
}

/// Input to a function that produces an expression that modifies a field after deserialization.
#[derive(Debug)]
pub struct MapperInput<'a, 'b> {
    /// Identifier of the serialized message object.
    pub message: &'a str,
    /// Identifier of the field's value, after producing with standard deserialization.
    pub value:   &'b str,
}


// === Product Types ===

impl DeserializerBuilder {
    /// Deserialize a `Class` of a fixed type (not dependant on further runtime data).
    fn deserialize_concrete(&mut self, graph: &TypeGraph) -> syntax::Method {
        let class = &graph[self.root];
        let message = "message";
        let mut body = String::new();
        let mut next_temp_variable_number = 0;
        let mut get_temp = || {
            let prefix = "generatedTemp";
            let result = format!("{}{}", prefix, next_temp_variable_number);
            next_temp_variable_number += 1;
            result
        };
        let fields = class_fields(graph, class);
        for field in &fields {
            let ty_name = quote_type(graph, &field.data);
            let expr = if let Some(materializer) = self.materializers.remove(&field.id()) {
                (materializer)(MaterializerInput { message })
            } else {
                match &field.data {
                    FieldData::Object { type_, non_null } => {
                        let value = get_temp();
                        if *non_null {
                            self.deserialize_object(
                                graph,
                                *type_,
                                message,
                                &value,
                                &mut get_temp,
                                &mut body,
                            );
                        } else {
                            self.deserialize_nullable(
                                graph,
                                *type_,
                                message,
                                &value,
                                &mut get_temp,
                                &mut body,
                            );
                        }
                        value
                    }
                    FieldData::Primitive(Primitive::Int { .. }) => format!("{}.get32()", message),
                    FieldData::Primitive(Primitive::Long { .. }) => format!("{}.get64()", message),
                    FieldData::Primitive(Primitive::Bool) => format!("{}.getBoolean()", message),
                }
            };
            let expr = match self.mappers.remove(&field.id()) {
                Some(mapper) => {
                    let value = get_temp();
                    writeln!(body, "{} {} = {};", ty_name, &value, expr).unwrap();
                    (mapper)(MapperInput { message, value: &value })
                }
                None => expr,
            };
            writeln!(body, "{} {} = {};", ty_name, &field.name, expr).unwrap();
        }
        let constructor_args: Vec<_> =
            fields.into_iter().map(|field| field.name.as_str()).collect();
        let constructor_args = constructor_args.join(", ");
        writeln!(body, "return new {}({});", &class.name, constructor_args).unwrap();
        let message_ty = syntax::Type::named(format!("{}.Message", &self.support));
        let mut method = syntax::Method::new("deserialize", quote_class_type(graph, self.root));
        method.static_ = true;
        method.body = body;
        method.arguments = vec![(message_ty, message.to_owned())];
        method
    }

    /// Deserialize an optional object; if it is not present, use the Java `null` value.
    fn deserialize_nullable<F>(
        &self,
        graph: &TypeGraph,
        id: ClassId,
        message: &str,
        output: &str,
        get_temp: &mut F,
        body: &mut String,
    ) where
        F: FnMut() -> String,
    {
        let ty_name = quote_class_type(graph, id);
        writeln!(body, "{ty_name} {output} = null;").unwrap();
        writeln!(body, "if ({message}.getBoolean()) {{").unwrap();
        let value = get_temp();
        self.deserialize_object(graph, id, message, &value, get_temp, body);
        writeln!(body, "{output} = {value};").unwrap();
        writeln!(body, "}}").unwrap();
    }

    /// Deserialize an object that is non-optional (unconditionally present in the serialized data).
    fn deserialize_object<F>(
        &self,
        graph: &TypeGraph,
        id: ClassId,
        message: &str,
        output: &str,
        get_temp: &mut F,
        body: &mut String,
    ) where
        F: FnMut() -> String,
    {
        let ty = &graph[id];
        let ty_name = quote_class_type(graph, id);
        if !ty.builtin {
            writeln!(body, "{ty_name} {output} = {ty_name}.deserialize({message});").unwrap();
            return;
        }
        match ty.name.as_str() {
            STRING => writeln!(body, "{ty_name} {output} = {message}.getString();").unwrap(),
            OPTIONAL => {
                let base = ty.params[0];
                let present = get_temp();
                writeln!(body, "{ty_name} {output};").unwrap();
                writeln!(body, "boolean {present} = {message}.getBoolean();").unwrap();
                writeln!(body, "if ({present}) {{").unwrap();
                let value = get_temp();
                self.deserialize_object(graph, base, message, &value, get_temp, body);
                writeln!(body, "{output} = {OPTIONAL}.of({value});").unwrap();
                writeln!(body, "}} else {output} = {OPTIONAL}.empty();").unwrap();
            }
            LIST => {
                let base = ty.params[0];
                let count = get_temp();
                writeln!(body, "int {count} = (int){message}.get64();").unwrap();
                let list_impl = get_temp();
                let params_ = quote_params(graph, &ty.params);
                let impl_ty = syntax::Type::generic("java.util.ArrayList", params_);
                writeln!(body, "{impl_ty} {list_impl} = new {impl_ty}({count});").unwrap();
                let unmodifiable_list = "java.util.Collections.unmodifiableList";
                writeln!(body, "for (int i=0; i<{count}; i++) {{").unwrap();
                let value = get_temp();
                self.deserialize_object(graph, base, message, &value, get_temp, body);
                writeln!(body, "{list_impl}.add({value});").unwrap();
                writeln!(body, "}}").unwrap();
                writeln!(body, "{ty_name} {output} = {unmodifiable_list}({list_impl});").unwrap();
            }
            x if x == self.either_type => {
                let t0 = ty.params[0];
                let t1 = ty.params[1];
                let t0 = quote_class_type(graph, t0);
                let t1 = quote_class_type(graph, t1);
                let name = &ty.name;
                let discriminant = get_temp();
                writeln!(body, "{ty_name} {output};").unwrap();
                writeln!(body, "int {discriminant} = {message}.get32();").unwrap();
                writeln!(body, "switch ({discriminant}) {{").unwrap();
                writeln!(
                    body,
                    "case 0: {output} = {name}.right({t1}.deserialize({message})); break;"
                )
                .unwrap();
                writeln!(
                    body,
                    "case 1: {output} = {name}.left({t0}.deserialize({message})); break;"
                )
                .unwrap();
                let err = format!("Unknown discriminant in {ty_name}.");
                let serialization = &self.support;
                writeln!(body, "default: throw new {serialization}.FormatException({err:?}); }}")
                    .unwrap();
            }
            _ => unimplemented!("Deserialize builtin: {}", &ty.name),
        }
    }
}


// === Sum Types ===

impl DeserializerBuilder {
    /// Deserialize a `Class` of known supertype, with concrete type encoded in the serialized data.
    fn deserialize_abstract(&self, graph: &TypeGraph) -> syntax::Method {
        let class = &graph[self.root];
        let message = "message";
        let mut n = 0;
        let mut get_temp = |base| {
            let suffix = "GeneratedTemp";
            let result = format!("{}{}{}", base, suffix, n);
            n += 1;
            result
        };
        let mut body = String::new();
        let discriminant = get_temp("discriminant");
        writeln!(body, "int {discriminant} = {message}.get32();").unwrap();
        writeln!(body, "switch ({discriminant}) {{").unwrap();
        for (key, id) in &class.discriminants {
            let ty = quote_class_type(graph, *id);
            writeln!(body, "case {key}: return {ty}.deserialize({message});").unwrap();
        }
        let ty_name = quote_class_type(graph, self.root);
        let err = format!("Unknown discriminant in {ty_name}.");
        let serialization = &self.support;
        writeln!(body, "default: throw new {serialization}.FormatException({:?});", err).unwrap();
        writeln!(body, "}}").unwrap();
        let message_ty = syntax::Type::named(format!("{serialization}.Message"));
        let mut method = syntax::Method::new("deserialize", ty_name);
        method.static_ = true;
        method.body = body;
        method.arguments = vec![(message_ty, message.to_owned())];
        method
    }
}
