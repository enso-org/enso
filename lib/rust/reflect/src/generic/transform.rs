use crate::generic::*;

/// `flatten` the specified fields into their containing structs, in an order that ensures
/// flattening is transitive.
///
/// Each inserted field will have its name prepended with the name of its eliminated container.
/// If the `hide` property is set for the container, it will be inherited by its child fields.
pub fn flatten(graph: &mut TypeGraph, ids: &mut BTreeSet<FieldId>) {
    let mut unchecked: BTreeSet<_> = graph.type_ids().collect();
    while let Some(id) = unchecked.pop_last() {
        flatten_(graph, ids, &mut unchecked, id);
    }
}

fn flatten_(
    graph: &mut TypeGraph,
    to_flatten: &mut BTreeSet<FieldId>,
    unchecked: &mut BTreeSet<TypeId>,
    outer: TypeId,
) {
    let mut dependencies = vec![];
    match &graph[outer].data {
        Data::Struct(fields) => {
            let flattened_field_types = fields
                .iter()
                .filter_map(|field| to_flatten.contains(&field.id).then(|| field.type_));
            dependencies.extend(flattened_field_types);
        }
        _ => (),
    };
    if dependencies.is_empty() {
        return;
    }
    for id in dependencies {
        if unchecked.remove(&id) {
            flatten_(graph, to_flatten, unchecked, id);
        }
    }
    let outer_fields = match &graph[outer].data {
        Data::Struct(fields) => fields,
        _ => unreachable!(),
    };
    let mut flattened = Vec::with_capacity(outer_fields.len());
    for field in outer_fields {
        let inner = field.type_;
        if to_flatten.remove(&field.id) {
            let inner_ty = &graph[inner];
            let inner_fields = match &inner_ty.data {
                Data::Struct(fields) => fields,
                _ => panic!(),
            };
            flattened.extend(inner_fields.iter().map(|inner_| {
                let mut name = field.name.clone();
                name.append(inner_.name.clone());
                let mut flat = graph.field(inner_.type_);
                flat.name = name;
                flat.hide = field.hide || inner_.hide;
                flat
            }));
        } else {
            flattened.push(field.clone());
        }
    }
    match &mut graph[outer].data {
        Data::Struct(fields) => *fields = flattened,
        _ => unreachable!(),
    };
}
