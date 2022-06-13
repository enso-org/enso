use super::*;
use crate::generic;
use std::collections::BTreeMap;
use std::collections::HashMap;


// =======================
// === Rust to Generic ===
// =======================

pub fn to_generic(ty: TypeData) -> (generic::TypeId, generic::TypeGraph) {
    let mut genericize = Genericize::new();
    let root_generic_id = genericize.run(ty);
    let roots = vec![root_generic_id];
    genericize.generic_graph.gc(roots);
    (root_generic_id, genericize.generic_graph)
}

#[derive(Debug)]
pub struct Genericize {
    // Outputs
    generic_of_rust: HashMap<TypeId, generic::TypeId>,
    generic_graph:   generic::TypeGraph,
    // Intermediate state
    interfaces:      Vec<(generic::TypeId, generic::TypeId)>,
    parent_types:    HashMap<GenericTypeId, (generic::TypeName, generic::Data, usize)>,
    subtypings:      Vec<(GenericTypeId, TypeId, generic::TypeId)>,
}

impl Genericize {
    fn new() -> Self {
        let generic_of_rust = Default::default();
        let generic_graph = Default::default();
        let interfaces = Default::default();
        let parent_types = Default::default();
        let subtypings = Default::default();
        Genericize { generic_of_rust, generic_graph, interfaces, parent_types, subtypings }
    }
}

impl Genericize {
    fn field_name(&self, s: &str) -> generic::FieldName {
        generic::FieldName::from_snake_case(s)
    }

    fn type_name(&self, s: &str) -> generic::TypeName {
        generic::TypeName::from_pascal_case(s)
    }

    fn named_struct(
        &mut self,
        id_: generic::TypeId,
        name: &str,
        fields: &[NamedField],
        erased: Option<GenericTypeId>,
    ) {
        let mut body = vec![];
        let mut child_field = None;
        for (i, field) in fields.iter().enumerate() {
            if field.subtype {
                assert_eq!(child_field, None);
                child_field = Some((i, field.type_.id));
                continue;
            }
            let name = self.field_name(&field.name);
            let value = generic::Field { type_: self.generic_of_rust[&field.type_.id] };
            body.push(generic::Named { name, value });
        }
        let data = generic::Data::Struct(generic::Struct::Named(body));
        let name = self.type_name(name);
        if let Some((index, field)) = child_field {
            let erased = erased.unwrap();
            self.parent_types.insert(erased, (name.to_owned(), data, index));
            self.subtypings.push((erased, field, id_));
            return;
        }
        let mut ty = generic::Type::new(name, data);
        self.generic_graph.set(id_, ty);
    }

    fn unnamed_struct(&mut self, id_: generic::TypeId, name: &str, fields: &[UnnamedField]) {
        let data = generic::Data::Struct(generic::Struct::Unnamed(
            fields
                .into_iter()
                .map(|field| generic::Field { type_: self.generic_of_rust[&field.type_.id] })
                .collect(),
        ));
        let name = self.type_name(name);
        let ty = generic::Type::new(name, data);
        self.generic_graph.set(id_, ty);
    }

    fn struct_(
        &mut self,
        id_: generic::TypeId,
        name: &str,
        fields: &Fields,
        erased: Option<GenericTypeId>,
    ) {
        match fields {
            Fields::Named(fields) => self.named_struct(id_, name, fields, erased),
            Fields::Unnamed(fields) => self.unnamed_struct(id_, name, fields),
            Fields::Unit => self.unit_struct(id_, name),
        }
    }

    fn unit_struct(&mut self, id_: generic::TypeId, name: &str) {
        let data = generic::Data::Struct(generic::Struct::Unit);
        let name = self.type_name(name);
        let ty = generic::Type::new(name, data);
        self.generic_graph.set(id_, ty);
    }

    fn enum_(&mut self, generic_id: generic::TypeId, name: &str, variants: &[Variant]) {
        let name = self.type_name(name);
        let children = variants.into_iter().map(|Variant { ident, fields, transparent }| {
            if *transparent {
                let id = &fields.as_wrapped_type().unwrap().id;
                let id_ = self.generic_of_rust[id];
                self.interfaces.push((generic_id, id_));
                id_
            } else {
                let id_ = self.generic_graph.reserve_type_id();
                self.struct_(id_, ident, fields, None);
                self.generic_graph[id_].parent = Some(generic_id);
                id_
            }
        });
        let data = generic::Data::Struct(generic::Struct::Unit);
        let mut ty = generic::Type::new(name, data);
        ty.abstract_ = true;
        ty.closed = true;
        ty.child_attrs.get_or_insert_default().discriminants = children.enumerate().collect();
        self.generic_graph.set(generic_id, ty);
    }

    fn primitive(&mut self, id_: generic::TypeId, name: &str, primitive: &Primitive) {
        let primitive = match primitive {
            Primitive::U32 => generic::Primitive::U32,
            Primitive::Bool => generic::Primitive::Bool,
            Primitive::Usize => generic::Primitive::Usize,
            Primitive::String => generic::Primitive::String,
            Primitive::Vec(t0) => generic::Primitive::Sequence(self.generic_of_rust[&t0.id]),
            Primitive::Option(t0) => generic::Primitive::Option(self.generic_of_rust[&t0.id]),
            Primitive::Result(t0, t1) => generic::Primitive::Result(
                self.generic_of_rust[&t0.id],
                self.generic_of_rust[&t1.id],
            ),
        };
        let data = generic::Data::Primitive(primitive);
        let name = self.type_name(name);
        let ty = generic::Type::new(name, data);
        self.generic_graph.set(id_, ty);
    }
}

impl Genericize {
    fn remove_transparent(
        &mut self,
        types: &mut HashMap<TypeId, TypeData>,
    ) -> Vec<(TypeId, TypeId)> {
        let mut alias = vec![];
        types.retain(|id, TypeData { transparent, data, .. }| {
            if *transparent {
                let target_id = match data {
                    Data::Struct(Struct { fields: Fields::Named(fields) }) if fields.len() == 1 =>
                        fields[0].type_.id,
                    Data::Struct(Struct { fields: Fields::Unnamed(fields) })
                        if fields.len() == 1 =>
                        fields[0].type_.id,
                    _ => panic!(),
                };
                alias.push((*id, target_id));
            }
            !*transparent
        });
        alias
    }

    pub fn run(&mut self, ty: TypeData) -> generic::TypeId {
        let root_rust_id = ty.id;
        let mut rust_types = collect_types(ty);
        let aliases = self.remove_transparent(&mut rust_types);
        for id in rust_types.keys() {
            let generic_id = self.generic_graph.reserve_type_id();
            self.generic_of_rust.insert(*id, generic_id);
        }
        // FIXME: 1-pass alias resolution here will panic if the target of an alias is an alias.
        for (id, target) in aliases {
            let generic_target = self.generic_of_rust[&target];
            self.generic_of_rust.insert(id, generic_target);
        }
        for (&rust_id, rust) in &rust_types {
            let name = &rust.name;
            let generic_id = self.generic_of_rust[&rust_id];
            let erased = Some(rust.subtype_erased);
            match &rust.data {
                Data::Struct(Struct { fields }) => self.struct_(generic_id, name, fields, erased),
                Data::Enum(Enum { variants }) => self.enum_(generic_id, name, variants),
                Data::Primitive(primitive) => self.primitive(generic_id, name, &primitive),
            };
        }
        for (parent_, child_) in self.interfaces.drain(..) {
            let old_parent = self.generic_graph[child_].parent.replace(parent_);
            assert_eq!(None, old_parent);
        }
        let mut aliases = vec![];
        // parent_types:    HashMap<GenericTypeId, generic::Type>,
        // subtypings:      Vec<(GenericTypeId, TypeId, generic::TypeId)>,
        // child_types:     HashMap<(GenericTypeId, TypeId), generic::TypeId>,
        let mut child_types = HashMap::new();
        let mut parent_ids = HashMap::new();
        let subtypings = std::mem::take(&mut self.subtypings);
        // Handle the variant first; that way we can generate its type family with `enum_`, which
        // doesn't handle checking for already-created child types.
        for (erased, field, id_) in &subtypings {
            let field_ty = &rust_types[field];
            match &field_ty.data {
                Data::Enum(_) => {
                    let field_ = self.generic_of_rust[field];
                    let (name, wrapper_data, index) = self.parent_types.remove(erased).unwrap();
                    // Move the Variant: We're merging the wrapper data into it, so any reference
                    // to it that wasn't through the wrapper must be an error.
                    // Note: This approach won't allow types that are subsetted by multiple enums.
                    let mut enum_ty_ = self.generic_graph.take(field_);
                    enum_ty_.name = name;
                    enum_ty_.data = wrapper_data;
                    enum_ty_.child_attrs.as_mut().unwrap().child_field = Some(index);
                    let children_: Vec<_> = enum_ty_
                        .child_attrs
                        .as_ref()
                        .unwrap()
                        .discriminants
                        .values()
                        .copied()
                        .collect();
                    self.generic_graph.set(*id_, enum_ty_);
                    for child_ in children_ {
                        let old_parent = self.generic_graph[child_].parent.replace(*id_);
                        assert_eq!(old_parent, Some(field_));
                        child_types.insert((*erased, *field), child_);
                    }
                    parent_ids.insert(erased, *id_);
                }
                Data::Struct(Struct { .. }) => continue,
                Data::Primitive(_) => panic!("Cannot transform a builtin to a subtype."),
            };
        }
        for (erased, field, id_) in &subtypings {
            let field_ty = &rust_types[field];
            if let Data::Struct(Struct { fields }) = &field_ty.data {
                // FIXME: Would be simple to handle, but doesn't occur in our data. Needs testing.
                let instantiated_with_enum =
                    "Unhandled: #[reflect(subtype)] that is never instantiated with an enum.";
                let parent_ = parent_ids.get(erased).expect(instantiated_with_enum);
                let child_key = (*erased, *field);
                if let Some(existing) = child_types.get(&child_key) {
                    aliases.push((*id_, *existing));
                } else {
                    self.struct_(*id_, &field_ty.name, fields, None);
                    self.generic_graph[*id_].parent = Some(*parent_);
                    child_types.insert(child_key, *id_);
                }
            }
        }
        self.generic_graph.apply_aliases(&aliases);
        self.generic_of_rust[&root_rust_id]
    }
}

/// Gather the Rust type IDs and definitions for the given type and its closure in the type
/// graph.
fn collect_types(root: TypeData) -> HashMap<TypeId, TypeData> {
    let mut to_visit = BTreeMap::new();
    let mut new_types = HashMap::new();
    for lazy in root.referenced_types() {
        to_visit.insert(lazy.id, lazy.evaluate);
    }
    let root_id = root.id;
    new_types.insert(root_id, root);
    while let Some((id, evaluate)) = to_visit.pop_last() {
        new_types.entry(id).or_insert_with(|| {
            let type_ = (evaluate)();
            debug_assert_eq!(id, type_.id);
            let refs = type_
                .referenced_types()
                .into_iter()
                .map(|LazyType { id, evaluate }| (id, evaluate));
            to_visit.extend(refs);
            type_
        });
    }
    new_types
}
