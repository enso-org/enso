//! Derivation of bincode[1] serialization for Java types.
//! [1]: https://github.com/bincode-org/bincode

use crate::java::implementation::*;
use crate::java::*;
use std::fmt::Write;



// ==========================
// === Derive Deserialize ===
// ==========================

type Materializer = Box<dyn FnOnce(&'_ str) -> String>;
type Mapper = Box<dyn FnOnce(&'_ str, &'_ str) -> String>;

/// Supports configuring deserialization for a type.
pub struct DeserializerBuilder {
    root:          TypeId,
    materializers: BTreeMap<FieldId, Materializer>,
    mappers:       BTreeMap<FieldId, Mapper>,
    support:       String,
    either_type:   String,
}

impl DeserializerBuilder {
    #[allow(missing_docs)]
    pub fn new(root: TypeId, support: impl Into<String>, either_type: impl Into<String>) -> Self {
        let materializers = Default::default();
        let mappers = Default::default();
        let support = support.into();
        let either_type = either_type.into();
        Self { root, materializers, mappers, support, either_type }
    }

    /// Configure the specified field to be produced according to the given expression, instead of
    /// by standard deserialization.
    pub fn materialize<F>(&mut self, field: FieldId, materializer: F)
    where F: FnOnce(&'_ str) -> String + 'static {
        self.materializers.insert(field, Box::new(materializer));
    }

    /// Configure the specified field to be modified by the given expression, after being
    /// deserialized.
    pub fn map<F>(&mut self, field: FieldId, mapper: F)
    where F: FnOnce(&'_ str, &'_ str) -> String + 'static {
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


// === Product Types ===

impl DeserializerBuilder {
    fn deserialize_concrete(&mut self, graph: &TypeGraph) -> syntax::Method {
        let class = &graph[self.root];
        let message = "message";
        let mut body = String::new();
        let mut n = 0;
        let mut get_temp = || {
            let prefix = "generatedTemp";
            let result = format!("{}{}", prefix, n);
            n += 1;
            result
        };
        let fields = class_fields(graph, class);
        for field in &fields {
            let ty_name = quote_type(graph, &field.data);
            let expr = if let Some(materializer) = self.materializers.remove(&field.id()) {
                (materializer)(message)
            } else {
                match &field.data {
                    FieldData::Object { type_, nonnull } => {
                        let value = get_temp();
                        self.deserialize_object(
                            graph,
                            *type_,
                            message,
                            &value,
                            &mut get_temp,
                            &mut body,
                            *nonnull,
                        );
                        value
                    }
                    FieldData::Primitive(Primitive::Int { .. }) => format!("{}.get32()", message),
                    FieldData::Primitive(Primitive::Long { .. }) => format!("{}.get64()", message),
                    FieldData::Primitive(Primitive::Bool) => format!("{}.getBoolean()", message),
                }
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

    fn deserialize_object<F>(
        &self,
        graph: &TypeGraph,
        id: TypeId,
        message: &str,
        output: &str,
        get_temp: &mut F,
        body: &mut String,
        nonnull: bool,
    ) where
        F: FnMut() -> String,
    {
        if !nonnull {
            let ty_name = quote_class_type(graph, id);
            writeln!(body, "{ty_name} {output} = null;").unwrap();
            writeln!(body, "if ({message}.getBoolean()) {{").unwrap();
            let value = get_temp();
            self.deserialize_object(graph, id, message, &value, get_temp, body, true);
            writeln!(body, "{output} = {value};").unwrap();
            writeln!(body, "}}").unwrap();
            return;
        }
        let ty = &graph[id];
        let ty_name = quote_class_type(graph, id);
        if !ty.builtin {
            writeln!(body, "{ty_name} {output} = {ty_name}.deserialize({message});").unwrap();
            return;
        }
        match ty.name.as_str() {
            "String" => writeln!(body, "{ty_name} {output} = {message}.getString();").unwrap(),
            "java.util.Optional" => {
                let base = ty.params[0];
                let present = get_temp();
                writeln!(body, "{ty_name} {output};").unwrap();
                writeln!(body, "boolean {present} = {message}.getBoolean();").unwrap();
                writeln!(body, "if ({present}) {{").unwrap();
                let value = get_temp();
                self.deserialize_object(graph, base, message, &value, get_temp, body, true);
                writeln!(body, "{output} = java.util.Optional.of({value});").unwrap();
                writeln!(body, "}} else {output} = java.util.Optional.empty();").unwrap();
            }
            "java.util.List" => {
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
                self.deserialize_object(graph, base, message, &value, get_temp, body, true);
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
