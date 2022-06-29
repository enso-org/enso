use crate::abstracted;
use crate::rust::*;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::mem::take;



// ==========================
// === Rust to Abstracted ===
// ==========================

pub fn to_abstracted(
    ty: TypeData,
) -> (abstracted::TypeGraph, BTreeMap<TypeId, abstracted::TypeId>) {
    let mut to_abstracted = ToAbstracted::new();
    let root_ = to_abstracted.run(ty);
    to_abstracted.graph.gc(vec![root_]);
    (to_abstracted.graph, to_abstracted.rust_to_abstracted)
}

#[derive(Debug, Default)]
pub struct ToAbstracted {
    // Outputs
    rust_to_abstracted: BTreeMap<TypeId, abstracted::TypeId>,
    graph:              abstracted::TypeGraph,
    // Intermediate state
    interfaces:         Vec<(abstracted::TypeId, abstracted::TypeId)>,
    parent_types:       BTreeMap<GenericTypeId, (abstracted::TypeName, abstracted::Data, usize)>,
    subtypings:         Vec<(GenericTypeId, TypeId, abstracted::TypeId)>,
    flatten:            BTreeSet<abstracted::FieldId>,
}

impl ToAbstracted {
    fn new() -> Self {
        Default::default()
    }
}

impl ToAbstracted {
    fn named_struct<'f>(
        &mut self,
        id_: abstracted::TypeId,
        name: &str,
        fields: impl IntoIterator<Item = &'f NamedField>,
        erased: Option<GenericTypeId>,
    ) {
        let mut body = vec![];
        let mut child_field = None;
        for (i, field) in fields.into_iter().enumerate() {
            assert!(!(field.flatten && field.subtype));
            if field.subtype {
                assert_eq!(child_field, None);
                child_field = Some((i, field.type_.id));
                continue;
            }
            let type_ = self.rust_to_abstracted[&field.type_.id];
            let name = field_name(&field.name);
            let mut field_ = abstracted::Field::named(name, type_);
            if field.flatten {
                self.flatten.insert(field_.id());
            }
            field_.hide = field.hide;
            body.push(field_);
        }
        let data = abstracted::Data::Struct(body);
        let name = type_name(name);
        if let Some((index, field)) = child_field {
            let erased = erased.unwrap();
            self.parent_types.insert(erased, (name, data, index));
            self.subtypings.push((erased, field, id_));
            return;
        }
        let ty = abstracted::Type::new(name, data);
        self.graph.set(id_, ty);
    }

    fn unnamed_struct(&mut self, id_: abstracted::TypeId, name: &str, fields: &[UnnamedField]) {
        let abstract_field = |field: &UnnamedField| {
            abstracted::Field::unnamed(self.rust_to_abstracted[&field.type_.id])
        };
        let data = fields.iter().map(abstract_field).collect();
        let data = abstracted::Data::Struct(data);
        let name = type_name(name);
        let ty = abstracted::Type::new(name, data);
        self.graph.set(id_, ty);
    }

    fn struct_(
        &mut self,
        id_: abstracted::TypeId,
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

    fn unit_struct(&mut self, id_: abstracted::TypeId, name: &str) {
        let data = abstracted::Data::Struct(vec![]);
        let name = type_name(name);
        let ty = abstracted::Type::new(name, data);
        self.graph.set(id_, ty);
    }

    fn enum_(&mut self, id_: abstracted::TypeId, name: &str, variants: &[Variant]) {
        let name = type_name(name);
        let children = variants.iter().map(|Variant { ident, fields, transparent }| {
            if *transparent {
                let field = &fields.as_wrapped_type().unwrap().id;
                let field_ = self.rust_to_abstracted[field];
                self.interfaces.push((id_, field_));
                field_
            } else {
                let new_ = self.graph.reserve_type_id();
                self.struct_(new_, ident, fields, None);
                self.graph[new_].parent = Some(id_);
                new_
            }
        });
        let data = abstracted::Data::Struct(vec![]);
        let mut ty = abstracted::Type::new(name, data);
        ty.abstract_ = true;
        ty.closed = true;
        ty.discriminants = children.enumerate().collect();
        self.graph.set(id_, ty);
    }

    fn primitive(&mut self, id_: abstracted::TypeId, name: &str, primitive: &Primitive) {
        let primitive = match primitive {
            Primitive::Bool => abstracted::Primitive::Bool,
            Primitive::U32 => abstracted::Primitive::U32,
            // In platform-independent formats, a `usize` is serialized as 64 bits.
            Primitive::Usize => abstracted::Primitive::U64,
            Primitive::String => abstracted::Primitive::String,
            Primitive::Vec(t0) => abstracted::Primitive::Sequence(self.rust_to_abstracted[&t0.id]),
            Primitive::Option(t0) => abstracted::Primitive::Option(self.rust_to_abstracted[&t0.id]),
            Primitive::Result(t0, t1) => abstracted::Primitive::Result(
                self.rust_to_abstracted[&t0.id],
                self.rust_to_abstracted[&t1.id],
            ),
        };
        let data = abstracted::Data::Primitive(primitive);
        let name = type_name(name);
        let ty = abstracted::Type::new(name, data);
        self.graph.set(id_, ty);
    }
}

impl ToAbstracted {
    fn remove_transparent(
        &mut self,
        types: &mut BTreeMap<TypeId, TypeData>,
    ) -> BTreeMap<TypeId, TypeId> {
        let mut alias = BTreeMap::new();
        types.retain(|id, TypeData { data, .. }| {
            let target = match data {
                Data::Struct(Struct { fields: Fields::Named(fields), transparent })
                    if *transparent =>
                {
                    assert_eq!(fields.len(), 1);
                    fields[0].type_.id
                }
                Data::Struct(Struct { fields: Fields::Unnamed(fields), transparent })
                    if *transparent =>
                {
                    assert_eq!(fields.len(), 1);
                    fields[0].type_.id
                }
                _ => return true,
            };
            alias.insert(*id, target);
            false
        });
        let entries: Vec<_> = alias.iter().map(|(k, v)| (*k, *v)).collect();
        for (key, mut value) in entries {
            while let Some(value_) = alias.get(&value).copied() {
                alias.insert(key, value_);
                value = value_;
            }
        }
        alias
    }

    pub fn run(&mut self, ty: TypeData) -> abstracted::TypeId {
        let root_rust_id = ty.id;
        let mut rust_types = collect_types(ty);
        let aliases = self.remove_transparent(&mut rust_types);
        for id in rust_types.keys() {
            let id_ = self.graph.reserve_type_id();
            self.rust_to_abstracted.insert(*id, id_);
        }
        for (id, target) in aliases {
            let target_ = self.rust_to_abstracted[&target];
            self.rust_to_abstracted.insert(id, target_);
        }
        for (&id, rust) in &rust_types {
            let name = &rust.name;
            let id_ = self.rust_to_abstracted[&id];
            let erased = Some(rust.subtype_erased);
            match &rust.data {
                Data::Struct(Struct { fields, transparent: _ }) =>
                    self.struct_(id_, name, fields, erased),
                Data::Enum(Enum { variants }) => self.enum_(id_, name, variants),
                Data::Primitive(primitive) => self.primitive(id_, name, primitive),
            };
        }
        for (parent_, child_) in self.interfaces.drain(..) {
            let old_parent = self.graph[child_].parent.replace(parent_);
            assert_eq!(None, old_parent);
        }
        self.generate_subtypes(&rust_types);
        abstracted::transform::flatten(&mut self.graph, &mut self.flatten);
        self.rust_to_abstracted[&root_rust_id]
    }

    fn generate_subtypes(&mut self, rust_types: &BTreeMap<TypeId, TypeData>) {
        let mut parent_ids = BTreeMap::new();
        let mut aliases = vec![];
        let subtypings = take(&mut self.subtypings);
        for (erased, field, id_) in &subtypings {
            let field_ty = &rust_types[field];
            match &field_ty.data {
                Data::Enum(_) => {
                    let field_ = self.rust_to_abstracted[field];
                    let (name, wrapper_data, index) = self.parent_types.remove(erased).unwrap();
                    // Move the Enum: We're merging the wrapper data into it, so any reference
                    // to it that wasn't through the wrapper must be an error.
                    // Note: This approach won't allow types that are subsetted by multiple enums.
                    let mut enum_ty_ = self.graph.take(field_);
                    enum_ty_.name = name;
                    enum_ty_.data = wrapper_data;
                    enum_ty_.child_field = Some(index);
                    let children_: Vec<_> = enum_ty_.discriminants.values().copied().collect();
                    self.graph.set(*id_, enum_ty_);
                    for child_ in children_ {
                        let old_parent = self.graph[child_].parent.replace(*id_);
                        assert_eq!(old_parent, Some(field_));
                    }
                    parent_ids.insert(erased, *id_);
                }
                Data::Struct(Struct { .. }) => continue,
                Data::Primitive(_) => panic!("Cannot transform a builtin to a subtype."),
            };
        }
        for (_erased, field, id_) in &subtypings {
            let field_ty = &rust_types[field];
            if let Data::Struct(_) = &field_ty.data {
                // FIXME: Would be simple to handle, but doesn't occur in our data. Needs testing.
                //let instantiated_with_enum =
                //    "Unhandled: #[reflect(subtype)] that is never instantiated with an enum.";
                //let parent_ = parent_ids.get(erased).expect(instantiated_with_enum);
                //let id = *self.transparented_types.get(field).expect("Unimplemented.");
                let id = *self.rust_to_abstracted.get(field).expect("Unimplemented.");
                aliases.push((*id_, id));
            }
        }
        self.graph.apply_aliases(&aliases);
    }
}

/// Gather the Rust type IDs and definitions for the given type and its closure in the type
/// graph.
fn collect_types(root: TypeData) -> BTreeMap<TypeId, TypeData> {
    let mut to_visit = BTreeMap::new();
    let mut new_types = BTreeMap::new();
    for lazy in root.referenced_types() {
        to_visit.insert(lazy.id, lazy);
    }
    let root_id = root.id;
    new_types.insert(root_id, root);
    while let Some((id, lazy)) = to_visit.pop_last() {
        new_types.entry(id).or_insert_with(|| {
            let type_ = lazy.evaluate();
            debug_assert_eq!(id, type_.id);
            let refs = type_.referenced_types().into_iter().map(|lazy: LazyType| (lazy.id, lazy));
            to_visit.extend(refs);
            type_
        });
    }
    new_types
}

fn field_name(s: &str) -> abstracted::FieldName {
    abstracted::FieldName::from_snake_case(s)
}

fn type_name(s: &str) -> abstracted::TypeName {
    abstracted::TypeName::from_pascal_case(s)
}
