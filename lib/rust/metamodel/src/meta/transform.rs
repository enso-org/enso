//! Transformations on the meta representation.

use crate::meta::*;

use derivative::Derivative;



// ===============
// === Flatten ===
// ===============

/// `flatten` the specified fields into their containing structs, transitively.
///
/// Each inserted field will have its name prepended with the name of its eliminated container.
/// If the `hide` property is set for the container, it will be inherited by its child fields.
///
/// This implements the [`reflect(flatten)`](../enso_reflect_macros/#reflectflatten-field-attribute)
/// attribute of the `#[derive(Reflect)]` macro; see the `enso_reflect_macros` documentation for an
/// example of the usage and results of the transformation.
pub fn flatten(graph: &mut TypeGraph, ids: &mut BTreeSet<FieldId>) {
    let order = toposort(graph.types.keys(), TypeGraphDependencyVisitor { graph, ids });
    for id in order {
        flatten_(graph, ids, id);
    }
}

/// `flatten` the fields specified in `to_flatten` into the type identified by `outer`.
///
/// For design notes, see [`flatten`].
fn flatten_(graph: &mut TypeGraph, to_flatten: &mut BTreeSet<FieldId>, outer: TypeId) {
    let outer_fields = match &mut graph[outer].data {
        Data::Struct(ref mut fields) => std::mem::take(fields),
        _ => return,
    };
    let mut child_field = graph[outer].child_field;
    let mut flattened = Vec::with_capacity(outer_fields.len());
    for (i, field) in outer_fields.into_iter().enumerate() {
        let inner = field.type_;
        if to_flatten.remove(&field.id) {
            let inner_ty = &graph[inner];
            let inner_fields = match &inner_ty.data {
                Data::Struct(fields) => fields,
                Data::Primitive(_) => panic!("Cannot flatten a primitive field."),
            };
            let flatten_field = |inner_: &Field| {
                let mut name = field.name.clone();
                name.append(inner_.name.clone());
                let mut flat = Field::named(name, inner_.type_);
                flat.hide = field.hide || inner_.hide;
                flat
            };
            flattened.extend(inner_fields.iter().map(flatten_field));
        } else {
            flattened.push(field);
        }
        if child_field == Some(i + 1) {
            child_field = Some(flattened.len());
        }
    }
    graph[outer].child_field = child_field;
    match &mut graph[outer].data {
        Data::Struct(fields) => *fields = flattened,
        _ => unreachable!(),
    };
}


// === Topologic Sort ===

fn toposort<T, V>(iter: impl IntoIterator<Item = T>, dependencies: V) -> Vec<T>
where
    T: Copy + Ord,
    V: DependencyVisitor<T>, {
    let mut sort = TopoSort::default();
    for id in iter {
        sort.visit(id, &dependencies);
    }
    sort.order
}

#[derive(Derivative)]
#[derivative(Default(bound = ""))]
struct TopoSort<T> {
    visited: BTreeSet<T>,
    order:   Vec<T>,
}

impl<T> TopoSort<T> {
    fn visit(&mut self, t: T, visitor: &impl DependencyVisitor<T>)
    where T: Copy + Ord {
        if self.visited.insert(t) {
            visitor.visit(self, t);
            self.order.push(t);
        }
    }
}

trait DependencyVisitor<T> {
    fn visit(&self, sort: &mut TopoSort<T>, t: T);
}

struct TypeGraphDependencyVisitor<'g, 'i> {
    graph: &'g TypeGraph,
    ids:   &'i BTreeSet<FieldId>,
}

impl DependencyVisitor<TypeId> for TypeGraphDependencyVisitor<'_, '_> {
    fn visit(&self, sort: &mut TopoSort<TypeId>, id: TypeId) {
        if let Data::Struct(fields) = &self.graph[id].data {
            for field in fields {
                if self.ids.contains(&field.id) {
                    sort.visit(field.type_, self);
                }
            }
        }
    }
}
