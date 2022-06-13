mod syntax;

use crate::generic;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeId(usize);

#[derive(Debug, Default)]
pub struct TypeGraph {
    types: Vec<Option<Class>>,
}

impl std::ops::Index<TypeId> for TypeGraph {
    type Output = Class;
    fn index(&self, TypeId(i): TypeId) -> &Self::Output {
        self.types.get(i).unwrap().as_ref().unwrap()
    }
}

impl std::ops::Index<&TypeId> for TypeGraph {
    type Output = Class;
    fn index(&self, id: &TypeId) -> &Self::Output {
        &self[*id]
    }
}

impl TypeGraph {
    fn reserve_type_id(&mut self) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(None);
        id
    }

    fn set(&mut self, id: TypeId, value: Class) {
        self.types[id.0] = Some(value);
    }

    fn insert(&mut self, value: Class) -> TypeId {
        let id = TypeId(self.types.len());
        self.types.push(Some(value));
        id
    }

    // FIXME: Some uses of this function actually need relative paths.
    //  For the `enso-parser` datamodel, the difference won't matter.
    pub fn path(&self, id: TypeId) -> String {
        let mut components = vec![];
        let mut next_id = Some(id);
        while let Some(id) = next_id {
            let ty = &self[id];
            components.push(ty.name.clone());
            next_id = ty.parent.clone();
        }
        components.reverse();
        components.join(".")
    }

    pub fn implement(&self) -> Vec<syntax::Class> {
        let mut implement = Implement {
            inner:    Default::default(),
            toplevel: Default::default(),
            graph:    self,
        };
        for (i, class) in self.types.iter().enumerate() {
            let class = match class {
                Some(class) => class,
                None => continue,
            };
            if class.builtin {
                continue;
            }
            // XXX: standard methods to add:
            // - constructor
            //   - (could do: java.util.Objects.requireNonNull, but if we only construct by
            //      deserialize, correct-by-construction probably OK)
            // - equals / hashCode / toString(==Debug)
            // - getters? (probably: yes by default, support disabling)
            // - (deserialization in its own pass)
            let id = TypeId(i);
            implement.add(id, class);
        }
        implement.into_toplevel_classes()
    }
}

struct Implement<'a> {
    inner:    Vec<(TypeId, syntax::Class)>,
    toplevel: std::collections::BTreeMap<TypeId, syntax::Class>,
    graph:    &'a TypeGraph,
}

impl<'a> Implement<'a> {
    fn add(&mut self, id: TypeId, class: &Class) {
        let name = class.name.clone();
        let abstract_ = class.abstract_;
        let final_ = !abstract_;
        let static_ = false;
        let parent = class.parent.map(|id| self.graph[id].name.clone());
        assert_eq!(parent, None, "Unimplemented.");
        let mut fields = vec![];
        for Field { name, data } in &class.fields {
            let mut params = vec![];
            let class = match data {
                FieldData::Object { type_, .. } => {
                    params =
                        self.graph[type_].params.iter().map(|ty| self.graph.path(*ty)).collect();
                    self.graph.path(*type_)
                }
                FieldData::Primitive(Primitive::Int { .. }) => "int".to_owned(),
                FieldData::Primitive(Primitive::Bool) => "boolean".to_owned(),
                FieldData::Primitive(Primitive::Long { .. }) => "long".to_owned(),
                FieldData::Primitive(Primitive::String) => "String".to_owned(),
            };
            let name = name.clone();
            let final_ = true;
            fields.push(syntax::Field { class, params, name, final_ });
        }
        let nested = vec![];
        let methods = vec![];
        let syntax =
            syntax::Class { name, abstract_, final_, static_, parent, fields, methods, nested };
        match class.parent.as_ref() {
            Some(parent) => self.inner.push((*parent, syntax)),
            None => {
                self.toplevel.insert(id, syntax);
            }
        }
    }

    fn into_toplevel_classes(mut self) -> Vec<syntax::Class> {
        for (id, mut class) in self.inner {
            class.static_ = true;
            self.toplevel.get_mut(&id).unwrap().nested.push(class);
        }
        self.toplevel.into_values().collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Class {
    name:      String,
    params:    Vec<TypeId>,
    parent:    Option<TypeId>,
    abstract_: bool,
    builtin:   bool,
    fields:    Vec<Field>,
}

impl Class {
    fn builtin(name: &str, fields: impl IntoIterator<Item = TypeId>) -> Self {
        let params: Vec<_> = fields.into_iter().collect();
        let name = name.to_owned();
        let parent = None;
        let abstract_ = false;
        let builtin = true;
        let fields = params
            .iter()
            .map(|&type_| Field {
                name: "data".to_owned(),
                data: FieldData::Object { type_, nonnull: true },
            })
            .collect();
        Class { name, params, parent, abstract_, builtin, fields }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    name: String,
    data: FieldData,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FieldData {
    Object { type_: TypeId, nonnull: bool },
    Primitive(Primitive),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Primitive {
    Bool,
    Int { unsigned: bool },
    Long { unsigned: bool },
    // FIXME. Shouldn't be a primitive.
    String,
}



///////////////////


pub fn from_generic(graph: &generic::TypeGraph) -> TypeGraph {
    let mut java_of_generic = std::collections::HashMap::new();
    let mut primitives = std::collections::HashMap::new();
    let mut java = TypeGraph::default();
    for (id, _) in graph.types.iter().enumerate() {
        let id = generic::TypeId(id);
        let id_ = java.reserve_type_id();
        java_of_generic.insert(id, id_);
    }
    for (id, ty) in graph.types.iter().enumerate() {
        let id = generic::TypeId(id);
        let id_ = java_of_generic[&id];
        let ty = match ty.as_ref() {
            Some(generic::Type { data: generic::Data::Primitive(ty), .. }) => ty,
            _ => continue,
        };
        let primitive = match ty {
            generic::Primitive::Bool => Primitive::Bool,
            // FIXME the right way to handle this varies; needs to be configurable
            generic::Primitive::Usize => Primitive::Long { unsigned: true },
            generic::Primitive::U32 => Primitive::Int { unsigned: true },
            generic::Primitive::String => {
                java.set(id_, Class::builtin("String", vec![]));
                continue;
            }
            generic::Primitive::Option(t0) => {
                java.set(id_, Class::builtin("java.util.Optional", vec![java_of_generic[t0]]));
                continue;
            }
            generic::Primitive::Sequence(t0) => {
                java.set(id_, Class::builtin("java.util.ArrayList", vec![java_of_generic[t0]]));
                continue;
            }
            generic::Primitive::Result(t0, t1) => {
                let t0_ = java_of_generic[t0];
                let t1_ = java_of_generic[t1];
                java.set(id_, Class::builtin("Either", vec![t0_, t1_]));
                continue;
            }
        };
        primitives.insert(id, primitive);
    }
    for (id, ty) in graph.types.iter().enumerate() {
        let ty = match ty.as_ref() {
            Some(ty) => ty,
            None => continue,
        };
        let id = generic::TypeId(id);
        let id_ = java_of_generic[&id];
        let name = ty.name.to_pascal_case();
        let abstract_ = ty.abstract_;
        let parent = ty.parent.as_ref().map(|id| java_of_generic[id]);
        let builtin = false;
        let fields;
        match &ty.data {
            generic::Data::Primitive(_) => continue,
            generic::Data::Struct(generic::Struct::Unit) => fields = vec![],
            generic::Data::Struct(generic::Struct::Named(fields_)) => {
                fields = fields_
                    .iter()
                    .map(|generic::Named { name, value: generic::Field { type_ } }| {
                        let name = name.clone();
                        let data = if let Some(primitive) = primitives.get(type_) {
                            FieldData::Primitive(primitive.clone())
                        } else {
                            let type_ = java_of_generic[type_];
                            FieldData::Object { type_, nonnull: true }
                        };
                        let name = name.to_camel_case();
                        Field { name, data }
                    })
                    .collect();
            }
            generic::Data::Struct(generic::Struct::Unnamed(fields)) => todo!(),
        }
        let params = vec![];
        let class = Class { name, params, parent, abstract_, builtin, fields };
        java.set(id_, class);
    }
    java
}



// ==============
// === Graphs ===
// ==============

pub mod graphviz {
    use super::*;
    use crate::graphviz::EdgeType;
    use crate::graphviz::Graph;
    use crate::graphviz::Node;
    use crate::graphviz::NodeType;

    pub fn graph(typegraph: &TypeGraph) {
        let mut graph = Graph::default();
        let types = &typegraph.types;
        for (i, ty) in types.iter().enumerate() {
            let ty = match ty.as_ref() {
                Some(ty) => ty,
                None => continue,
            };
            let sname = format!("{}{}", ty.name, i);
            let node_type = match &ty.abstract_ {
                true => NodeType::AbstractStruct,
                false => NodeType::Struct,
            };
            let label = ty.name.clone();
            let primitive = ty.builtin;
            graph.nodes.insert(sname.clone(), Node { primitive, node_type, label });
            if let Some(&parent) = ty.parent.as_ref() {
                let id = parent.0;
                let sparent = format!("{}{}", types[id].as_ref().unwrap().name, id);
                graph.edges.push((sparent.clone(), sname.clone(), EdgeType::Subtype));
            }
            for field in &ty.fields {
                match &field.data {
                    FieldData::Object { type_, nonnull } => {
                        let id = type_.0;
                        let sname2 = format!("{}{}", types[id].as_ref().unwrap().name, id);
                        let edgetype = match nonnull {
                            false => EdgeType::OptionalField,
                            true => EdgeType::Field,
                        };
                        graph.edges.push((sname.clone(), sname2, edgetype));
                    }
                    // TODO
                    FieldData::Primitive(_) => {}
                }
            }
        }
        println!("{}", graph);
    }
}
