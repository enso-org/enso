//! Abstracting Rust data models to the `meta` representation.

use crate::rust::*;

use crate::meta;

use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::mem::take;



// ====================
// === Rust to Meta ===
// ====================

/// Abstract the data model to the meta represenation.
pub fn to_meta(ty: TypeData) -> (meta::TypeGraph, BTreeMap<TypeId, meta::TypeId>) {
    let mut to_meta = ToMeta::new();
    let root_ = to_meta.run(ty);
    let (mut graph, rust_to_meta) = to_meta.finish();
    graph.gc(vec![root_]);
    (graph, rust_to_meta)
}

#[derive(Debug, Default)]
struct ToMeta {
    // Outputs
    rust_to_meta: BTreeMap<TypeId, meta::TypeId>,
    graph:        meta::TypeGraph,
    // Intermediate state
    interfaces:   Vec<(meta::TypeId, meta::TypeId)>,
    parent_types: BTreeMap<GenericTypeId, (meta::TypeName, meta::Data, usize)>,
    subtypings:   Vec<(GenericTypeId, TypeId, meta::UnboundTypeId)>,
    flatten:      BTreeSet<meta::FieldId>,
}

impl ToMeta {
    fn new() -> Self {
        Default::default()
    }
}

impl ToMeta {
    fn named_struct<'f>(
        &mut self,
        id_: meta::UnboundTypeId,
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
            let type_ = self.rust_to_meta[&field.type_.id];
            let name = field.rename.as_deref().unwrap_or(&field.name);
            let name = field_name(name);
            let mut field_ = meta::Field::named(name, type_);
            if field.flatten {
                self.flatten.insert(field_.id());
            }
            field_.hide = field.hide;
            body.push(field_);
        }
        let data = meta::Data::Struct(body);
        let name = type_name(name);
        if let Some((index, field)) = child_field {
            let erased = erased.unwrap();
            self.parent_types.insert(erased, (name, data, index));
            self.subtypings.push((erased, field, id_));
            return;
        }
        let ty = meta::Type::new(name, data);
        self.graph.types.bind(id_, ty);
    }

    fn unnamed_struct(&mut self, id_: meta::UnboundTypeId, name: &str, fields: &[UnnamedField]) {
        let abstract_field =
            |field: &UnnamedField| meta::Field::unnamed(self.rust_to_meta[&field.type_.id]);
        let data = fields.iter().map(abstract_field).collect();
        let data = meta::Data::Struct(data);
        let name = type_name(name);
        let ty = meta::Type::new(name, data);
        self.graph.types.bind(id_, ty);
    }

    fn struct_(
        &mut self,
        id_: meta::UnboundTypeId,
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

    fn unit_struct(&mut self, id_: meta::UnboundTypeId, name: &str) {
        let data = meta::Data::Struct(vec![]);
        let name = type_name(name);
        let ty = meta::Type::new(name, data);
        self.graph.types.bind(id_, ty);
    }

    fn enum_(&mut self, id_: meta::UnboundTypeId, name: &str, variants: &[Variant]) {
        let name = type_name(name);
        let children = variants.iter().map(|Variant { ident, fields, inline: transparent }| {
            if *transparent {
                let field = &fields.as_wrapped_type().unwrap().id;
                let field_ = self.rust_to_meta[field];
                self.interfaces.push(((&id_).into(), field_));
                field_
            } else {
                let promise = self.graph.types.unbound_key();
                let new_ = meta::TypeId::from(&promise);
                self.struct_(promise, ident, fields, None);
                self.graph[new_].parent = Some((&id_).into());
                new_
            }
        });
        let data = meta::Data::Struct(vec![]);
        let mut ty = meta::Type::new(name, data);
        ty.abstract_ = true;
        ty.closed = true;
        ty.discriminants = children.enumerate().collect();
        self.graph.types.bind(id_, ty);
    }

    fn primitive(&mut self, id_: meta::UnboundTypeId, name: &str, primitive: &Primitive) {
        let primitive = match primitive {
            Primitive::Bool => meta::Primitive::Bool,
            Primitive::U32 => meta::Primitive::U32,
            // In platform-independent formats, a `usize` is serialized as 64 bits.
            Primitive::Usize => meta::Primitive::U64,
            Primitive::String => meta::Primitive::String,
            Primitive::Vec(t0) => meta::Primitive::Sequence(self.rust_to_meta[&t0.id]),
            Primitive::Option(t0) => meta::Primitive::Option(self.rust_to_meta[&t0.id]),
            Primitive::Result(t0, t1) =>
                meta::Primitive::Result(self.rust_to_meta[&t0.id], self.rust_to_meta[&t1.id]),
        };
        let data = meta::Data::Primitive(primitive);
        let name = type_name(name);
        let ty = meta::Type::new(name, data);
        self.graph.types.bind(id_, ty);
    }
}

impl ToMeta {
    fn remove_transparent(
        &mut self,
        types: &mut BTreeMap<TypeId, TypeData>,
    ) -> BTreeMap<TypeId, TypeId> {
        let mut alias = BTreeMap::new();
        types.retain(|id, TypeData { data, .. }| {
            let sole_field =
                "`#[reflect(transparent)]` can only be applied to types with exactly one field.";
            let target = match data {
                Data::Struct(Struct { fields, transparent }) if *transparent =>
                    fields.as_wrapped_type().expect(sole_field).id,
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

    /// Perform the transformation for the reference-closure of the given type.
    pub fn run(&mut self, ty: TypeData) -> meta::TypeId {
        let root_rust_id = ty.id;
        let mut rust_types = collect_types(ty);
        let aliases = self.remove_transparent(&mut rust_types);
        let mut meta_promises: BTreeMap<_, _> =
            rust_types.keys().map(|id| (*id, self.graph.types.unbound_key())).collect();
        self.rust_to_meta =
            meta_promises.iter().map(|(k, v)| (*k, meta::TypeId::from(v))).collect();
        for (id, target) in aliases {
            let target_ = self.rust_to_meta[&target];
            self.rust_to_meta.insert(id, target_);
        }
        for (&id, rust) in &rust_types {
            let name = &rust.name;
            let id_ = meta_promises.remove(&id).unwrap();
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
        meta::transform::flatten(&mut self.graph, &mut self.flatten);
        self.rust_to_meta[&root_rust_id]
    }

    /// Return results.
    pub fn finish(self) -> (meta::TypeGraph, BTreeMap<TypeId, meta::TypeId>) {
        (self.graph, self.rust_to_meta)
    }

    fn generate_subtypes(&mut self, rust_types: &BTreeMap<TypeId, TypeData>) {
        let mut parent_ids = BTreeMap::new();
        let mut aliases = vec![];
        let subtypings = take(&mut self.subtypings);
        let mut concrete_subtypes = vec![];
        for (erased, field, promise) in subtypings {
            let id_ = meta::TypeId::from(&promise);
            let field_ty = &rust_types[&field];
            match &field_ty.data {
                Data::Enum(_) => {
                    let field_ = self.rust_to_meta[&field];
                    let (name, wrapper_data, index) = self.parent_types.remove(&erased).unwrap();
                    // Move the Enum: We're merging the wrapper data into it, so any reference
                    // to it that wasn't through the wrapper must be an error.
                    // Note: This approach won't allow types that are subsetted by multiple enums.
                    let mut enum_ty_ = self.graph.types.remove(field_);
                    enum_ty_.name = name;
                    enum_ty_.data = wrapper_data;
                    enum_ty_.child_field = Some(index);
                    let children_: Vec<_> = enum_ty_.discriminants.values().copied().collect();
                    self.graph.types.bind(promise, enum_ty_);
                    for child_ in children_ {
                        let old_parent = self.graph[child_].parent.replace(id_);
                        assert_eq!(old_parent, Some(field_));
                    }
                    parent_ids.insert(erased, id_);
                }
                Data::Struct(_) => {
                    concrete_subtypes.push((erased, field, id_));
                    continue;
                }
                Data::Primitive(_) => panic!("Cannot transform a builtin to a subtype."),
            };
        }
        for (_erased, field, id_) in concrete_subtypes {
            let variants_only = "Applying `#[reflect(subtype)]` to a field that does not occur \
                in a variant of an enum used to instantiate the field is not supported.";
            let id = *self.rust_to_meta.get(&field).expect(variants_only);
            aliases.push((id_, id));
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

fn field_name(s: &str) -> meta::FieldName {
    meta::FieldName::from_snake_case(s)
}

fn type_name(s: &str) -> meta::TypeName {
    meta::TypeName::from_pascal_case(s)
}
