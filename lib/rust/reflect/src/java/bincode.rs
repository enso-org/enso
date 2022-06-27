use crate::java::implementation::*;
use crate::java::*;



// ==========================
// === Derive Deserialize ===
// ==========================

type Materializer = Box<dyn FnOnce(&'_ str) -> String>;
type Mapper = Box<dyn FnOnce(&'_ str, &'_ str) -> String>;

pub struct DeserializerBuilder {
    id:            TypeId,
    materializers: BTreeMap<FieldId, Materializer>,
    mappers:       BTreeMap<FieldId, Mapper>,
}

impl DeserializerBuilder {
    pub fn new(id: TypeId) -> Self {
        let materializers = Default::default();
        let mappers = Default::default();
        Self { id, materializers, mappers }
    }

    pub fn materialize<F>(&mut self, field: FieldId, materializer: F)
    where F: FnOnce(&'_ str) -> String + 'static {
        self.materializers.insert(field, Box::new(materializer));
    }

    pub fn map<F>(&mut self, field: FieldId, mapper: F)
    where F: FnOnce(&'_ str, &'_ str) -> String + 'static {
        self.mappers.insert(field, Box::new(mapper));
    }

    pub fn build(mut self, graph: &TypeGraph) -> Method {
        let method = match graph[self.id].abstract_ {
            true => deserialize_abstract(graph, self.id),
            false => self.deserialize_concrete(graph),
        };
        Method::Raw(method)
    }
}


// === Product Types ===

impl DeserializerBuilder {
    fn deserialize_concrete(&mut self, graph: &TypeGraph) -> syntax::Method {
        let class = &graph[self.id];
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
            let expr = if let Some(materializer) = self.materializers.remove(&field.id) {
                (materializer)(message)
            } else {
                match &field.data {
                    FieldData::Object { type_, nonnull } => {
                        let value = get_temp();
                        deserialize_object(
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
            body.push(format!("{} {} = {};", ty_name, &field.name, expr));
        }
        let constructor_args: Vec<_> = fields.into_iter().map(|field| field.name.as_str()).collect();
        let constructor_args = constructor_args.join(", ");
        body.push(format!("return new {}({});", &class.name, constructor_args));
        let mut method = syntax::Method::new("deserialize", quote_class_type(graph, self.id));
        method.static_ = true;
        method.body = body.join("\n");
        method.arguments = vec![(syntax::Type::named("utils.BincodeMessage"), message.to_owned())];
        method
    }
}

fn deserialize_object<F>(
    graph: &TypeGraph,
    id: TypeId,
    message: &str,
    output: &str,
    get_temp: &mut F,
    body: &mut Vec<String>,
    nonnull: bool,
) where
    F: FnMut() -> String,
{
    if !nonnull {
        let ty_name = quote_class_type(graph, id);
        body.push(format!("{} {} = null;", ty_name, &output));
        body.push(format!("if ({}.getBoolean()) {{", message));
        let value = get_temp();
        deserialize_object(graph, id, message, &value, get_temp, body, true);
        body.push(format!("{} = {};", &output, &value));
        body.push(format!("}}"));
        return;
    }
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
            deserialize_object(graph, base, message, &value, get_temp, body, true);
            body.push(format!("{} = java.util.Optional.of({});", &output, &value));
            body.push(format!("}} else {} = java.util.Optional.empty();", &output));
        }
        "java.util.ArrayList" => {
            let base = ty.params[0];
            let count = get_temp();
            body.push(format!("int {} = (int){}.get64();", count, message));
            body.push(format!("{} {} = new {}({});", ty_name, &output, ty_name, count));
            body.push(format!("for (int i=0; i<{}; i++) {{", count));
            let value = get_temp();
            deserialize_object(graph, base, message, &value, get_temp, body, true);
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
        }
        _ => unimplemented!("Deserialize builtin: {}", &ty.name),
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
    let mut method = syntax::Method::new("deserialize", quote_class_type(graph, id));
    method.static_ = true;
    method.body = body.join("\n");
    method.arguments = vec![(syntax::Type::named("utils.BincodeMessage"), message.to_owned())];
    method
}
