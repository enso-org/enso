//! This module defines a cascading style sheet registry and related style management utilities.

use crate::prelude::*;

use crate::control::callback;
use crate::data::HashMapTree;
use crate::data::Index;
use crate::data::OptVec;

pub use super::data::data;
pub use super::data::Data;
pub use super::path::Path;
pub use super::path::StaticPath;



// =============
// === Query ===
// =============

/// Pointer to a style sheet node. Always bound to the most specific style matching the query (if
/// such style exists). For example, the query 'panel.button.size' will be bound to
/// 'panel.button.size', 'button.size', or 'size' in that order.
///
/// # Implementation Details
/// Each query keeps a list of all style sheet nodes which match this query (`matches`). For
/// example, the query 'panel.button.size' contains three matches - 'panel.button.size',
/// 'button.size', and 'size'. The longest match with a defined value is considered the best match
/// and is remembered (`binding`). Moreover, each query keeps list of all sheet nodes which use this
/// query in their expressions (`users`). A query is considered unused and can be safely removed
/// from the style sheet graph if no sheet nodes use it in their expressions and it is not
/// referred by an external, user code (`external_count`).
#[derive(Debug)]
pub struct Query {
    path:           Path,
    index:          Index<Query>,
    matches:        Vec<Index<SheetNode>>,
    binding:        Option<Index<SheetNode>>,
    users:          HashSet<Index<SheetNode>>,
    external_count: usize,
}

impl Query {
    /// Constructor.
    pub fn new(path: Path, index: Index<Query>) -> Self {
        let matches = default();
        let binding = default();
        let users = default();
        let external_count = default();
        Self { path, index, matches, binding, users, external_count }
    }

    /// Checks whether the variable is being used. Please note that all external variables are
    /// considered to be used.
    pub fn is_unused(&self) -> bool {
        self.external_count == 0 && self.users.is_empty()
    }

    /// Checks whether this query is used by one or more external variables.
    pub fn is_external(&self) -> bool {
        self.external_count > 0
    }

    /// Increments the counter of external users of this query.
    fn inc_external_count(&mut self) {
        self.external_count += 1;
    }

    /// Decrements the counter of external users of this query.
    fn dec_external_count(&mut self) {
        self.external_count -= 1;
    }
}



// =================
// === SheetNode ===
// =================

/// A node in the style sheet tree. Each node is associated with a path like 'panel.button.size' and
/// contains a `Data` value. The value can either be set explicitly, or computed automatically if
/// the node is assigned with `Expression`.
///
/// # Implementation Details
/// Each sheet node keeps list of all queries which match this node (`matches`). It also keeps list
/// of all queries which were bound to this node as their best match (`bindings`). To learn more
/// about matches and bindings see the `Query` docs.
#[derive(Debug)]
pub struct SheetNode {
    path:     Path,
    index:    Index<SheetNode>,
    value:    Option<Data>,
    expr:     Option<BoundExpression>,
    matches:  HashSet<Index<Query>>,
    bindings: HashSet<Index<Query>>,
}

impl SheetNode {
    /// Constructor.
    pub fn new(path: Path, index: Index<SheetNode>) -> Self {
        let value = default();
        let expr = default();
        let matches = default();
        let bindings = default();
        Self { path, index, value, expr, matches, bindings }
    }

    /// Checks whether the style sheet node exist. Nodes without value are considered templates and
    /// are kept in the graph for optimization purposes only.
    pub fn exists(&self) -> bool {
        self.value.is_some()
    }

    /// Checks whether the sheet node is being used.
    pub fn is_unused(&self) -> bool {
        self.matches.is_empty() && self.value.is_none()
    }
}



// ==================
// === Expression ===
// ==================

/// Style sheet expression declaration.
#[derive(Clone)]
#[allow(missing_docs)]
pub struct Expression {
    pub args:     Vec<Path>,
    pub function: Rc<dyn Fn(&[&Data]) -> Data>,
}

impl Expression {
    /// Constructor.
    pub fn new<A, I, F>(args: A, function: F) -> Self
    where
        A: IntoIterator<Item = I>,
        I: Into<Path>,
        F: 'static + Fn(&[&Data]) -> Data, {
        let args = args.into_iter().map(|t| t.into()).collect_vec();
        let function = Rc::new(function);
        Self { args, function }
    }

    /// Simple reference (identity) expression constructor.
    pub fn reference(path: impl Into<Path>) -> Self {
        Self::new(&[path.into()], |t| t[0].clone())
    }
}

impl Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Expression")
    }
}

impl PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        let same_args = self.args == other.args;
        // Rc stores a fat pointer, and fat pointers can refer to different vtables even if they
        // point to the same value. Here we want only to compare the data part, while ignoring
        // the vtable. The *const u8 cast discards the vtable.
        // See: https://github.com/rust-lang/rust/issues/46139
        let lhs_ptr = Rc::as_ptr(&self.function) as *const u8;
        let rhs_ptr = Rc::as_ptr(&other.function) as *const u8;
        let same_function = lhs_ptr == rhs_ptr;
        same_args && same_function
    }
}

impls! { From<Path>        for Expression { |t| Self::reference(t) }}
impls! { From<&Path>       for Expression { |t| Self::reference(t) }}
impls! { From<StaticPath>  for Expression { |t| Self::reference(t) }}
impls! { From<&StaticPath> for Expression { |t| Self::reference(t) }}



// =======================
// === BoundExpression ===
// =======================

/// Style sheet expression with arguments bound to specific queries.
#[derive(Clone)]
pub struct BoundExpression {
    args:     Vec<Index<Query>>,
    function: Rc<dyn Fn(&[&Data]) -> Data>,
}

impl BoundExpression {
    /// Constructor.
    pub fn new(args: Vec<Index<Query>>, function: Rc<dyn Fn(&[&Data]) -> Data>) -> Self {
        Self { args, function }
    }
}

impl Debug for BoundExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BoundExpression")
    }
}



// =============
// === Value ===
// =============

/// Used to set value of style nodes. Also, used internally by `Change`.
#[derive(Clone, Debug, PartialEq)]
#[allow(missing_docs)]
pub enum Value {
    Data(Data),
    Expression(Expression),
}

impl From<Expression> for Value {
    fn from(t: Expression) -> Self {
        Self::Expression(t)
    }
}

impls! { From<Path>        for Value { |t| Self::Expression(t.into()) }}
impls! { From<&Path>       for Value { |t| Self::Expression(t.into()) }}
impls! { From<StaticPath>  for Value { |t| Self::Expression(t.into()) }}
impls! { From<&StaticPath> for Value { |t| Self::Expression(t.into()) }}

impl<T> From<T> for Value
where T: Into<Data>
{
    default fn from(t: T) -> Self {
        Self::Data(t.into())
    }
}

impl TryFrom<String> for Value {
    type Error = <Data as TryFrom<String>>::Error;
    fn try_from(s: String) -> Result<Self, Self::Error> {
        s.try_into().map(Self::Data)
    }
}

impl PartialSemigroup<&Value> for Value {
    fn concat_mut(&mut self, other: &Self) {
        *self = other.clone()
    }
}

impl PartialSemigroup<Value> for Value {
    fn concat_mut(&mut self, other: Self) {
        *self = other
    }
}



// ==============
// === Change ===
// ==============

/// Defines a change to a style sheet. Style sheets allow bulk-application of changes in order to
/// optimize the amount of necessary computations.
#[derive(Debug)]
#[allow(missing_docs)]
pub struct Change {
    pub path:  Path,
    pub value: Option<Value>,
}

impl Change {
    /// Constructor.
    pub fn new<P>(path: P, value: Option<Value>) -> Self
    where P: Into<Path> {
        let path = path.into();
        Self { path, value }
    }
}



// =================
// === SheetData ===
// =================

/// Internal data of `Sheet`.
#[derive(Debug)]
pub struct SheetData {
    queries:   QueryVec,
    nodes:     NodeVec,
    query_map: QueryMap,
    node_map:  NodeMap,
}


// === Types ===

#[allow(missing_docs)]
mod cascading_sheets_types {
    use super::*;
    pub type QueryVec = OptVec<Query, Index<Query>>;
    pub type NodeVec = OptVec<SheetNode, Index<SheetNode>>;
    pub type QueryMap = HashMapTree<String, Option<Index<Query>>>;
    pub type NodeMap = HashMapTree<String, Index<SheetNode>>;
}
use cascading_sheets_types::*;


// === Constructors ===

impl SheetData {
    /// Constructor.
    pub fn new() -> Self {
        let queries = default();
        let mut nodes = NodeVec::new();
        let query_map = default();
        let root_sheet_id = nodes.insert_with_ix_(|ix| SheetNode::new(Path::empty(), ix));
        let node_map = NodeMap::from_value(root_sheet_id);
        Self { queries, nodes, query_map, node_map }
    }

    /// Access variable by the given path or create new one if missing.
    ///
    /// # Implementation Notes
    /// Under the hood, a `SheetNode` for each sub-path will be created. For example, when creating
    /// "panel.button.size" query, the intermediate nodes will be created as well: "button.size" and
    /// "size". The intermediate nodes will not contain a value, so they will behave transparently
    /// to the engine. However, they will be used to keep track of all possible query matches which
    /// is used to implement high-performance query rebinding mechanism.
    pub fn unmanaged_query<P: Into<Path>>(&mut self, path: P) -> Index<Query> {
        let path = path.into();
        let queries = &mut self.queries;
        let nodes = &mut self.nodes;
        let query_map_node = self.query_map.get_or_create_node(&path.rev_segments);
        let mut query_matches = Vec::new();
        self.node_map.get_or_create_node_traversing_path_with(
            &path.rev_segments,
            |p| nodes.insert_with_ix_(|ix| SheetNode::new(Path::from_rev_segments(p), ix)),
            |t| query_matches.push(t.value),
        );
        query_matches.reverse();

        let query_index = *query_map_node
            .value_or_set_with(|| queries.insert_with_ix_(move |ix| Query::new(path, ix)));

        for sheet_node_index in &query_matches {
            self.nodes[*sheet_node_index].matches.insert(query_index);
        }

        self.queries[query_index].matches = query_matches;
        self.rebind_query(query_index);
        query_index
    }

    /// Access style sheet node by the given path or create new one if missing.
    fn sheet_node<P: Into<Path>>(&mut self, path: P) -> Index<SheetNode> {
        let path = path.into();
        let nodes = &mut self.nodes;
        let node = self.node_map.get_or_create_node_path_with(&path.rev_segments, |p| {
            nodes.insert_with_ix_(|ix| SheetNode::new(Path::from_rev_segments(p), ix))
        });
        node.value
    }
}


// === Getters ===

impl SheetData {
    /// Reads the value of the query.
    pub fn query_value(&self, query_index: Index<Query>) -> Option<&Data> {
        self.queries.safe_index(query_index).as_ref().and_then(|query| {
            query.binding.and_then(|sheet_node_index| self.nodes[sheet_node_index].value.as_ref())
        })
    }

    /// Reads the value of a path like it was a query. For example, querying "button.size" will
    /// return the value of "size" if no exact match was found.
    pub fn query<P>(&self, path: P) -> Option<&Data>
    where P: Into<Path> {
        let path = path.into();
        while path.rev_segments.is_empty() {
            let value = self.value(&path);
            if value.is_some() {
                return value;
            }
        }
        None
    }

    /// Reads the value of the style sheet by the exact path provided. If you want to read a value
    /// of a variable binding, use `query` instead.
    pub fn value<P>(&self, path: P) -> Option<&Data>
    where P: Into<Path> {
        let path = path.into();
        let segs = &path.rev_segments;
        self.node_map.get_node(segs).and_then(|t| self.nodes[t.value].value.as_ref())
    }

    /// Returns the amount of queries used.
    pub fn queries_count(&self) -> usize {
        self.queries.len()
    }

    /// Returns the amount of sheet nodes used not including the root sheet node.
    pub fn sheet_nodes_count(&self) -> usize {
        let root_sheet_node_count = 1;
        self.nodes.len() - root_sheet_node_count
    }
}


// === Setters ===

impl SheetData {
    /// Sets the value by the given path. Returns indexes of all affected queries.
    pub fn set<P, V>(&mut self, path: P, value: V) -> HashSet<Index<Query>>
    where
        P: Into<Path>,
        V: Into<Value>, {
        let value = value.into();
        self.apply_change(Change::new(path, Some(value)))
    }

    /// Removes the value by the given path. Returns indexes of all affected queries.
    pub fn unset<P>(&mut self, path: P) -> HashSet<Index<Query>>
    where P: Into<Path> {
        self.apply_change(Change::new(path, None))
    }

    /// Changes the value by the given path. Providing `None` as the value means that the value
    /// will be removed. Returns indexes of all affected queries.
    pub fn change<P>(&mut self, path: P, value: Option<Value>) -> HashSet<Index<Query>>
    where P: Into<Path> {
        self.apply_change(Change::new(path, value))
    }

    /// Apply a `Change`. Returns indexes of all affected queries.
    pub fn apply_change(&mut self, change: Change) -> HashSet<Index<Query>> {
        self.apply_changes(iter::once(change))
    }

    /// Apply a set of `Change`s. Returns indexes of all affected queries.
    pub fn apply_changes<I>(&mut self, changes: I) -> HashSet<Index<Query>>
    where I: IntoIterator<Item = Change> {
        let mut changed_queries = HashSet::<Index<Query>>::new();
        let mut possible_orphans = Vec::<Index<SheetNode>>::new();
        let sheet_nodes_iter = changes.into_iter().map(|change| {
            let sheet_node_index = self.sheet_node(change.path);
            let sheet_node = &mut self.nodes[sheet_node_index];

            // Remove expression bindings.
            let opt_expr = mem::take(&mut sheet_node.expr);
            if let Some(expr) = opt_expr {
                for query_index in expr.args {
                    self.queries[query_index].users.remove(&sheet_node_index);
                    self.drop_query_if_unused(query_index);
                }
            }

            // Set new value and rebind variables.
            let sheet_node = &mut self.nodes[sheet_node_index];
            match change.value {
                None => {
                    let needs_rebind = sheet_node.value.is_some();
                    if needs_rebind {
                        sheet_node.value = None;
                        for query_index in sheet_node.bindings.clone() {
                            if self.rebind_query(query_index) {
                                changed_queries.insert(query_index);
                            }
                        }
                        possible_orphans.push(sheet_node_index);
                    }
                }
                Some(value) => {
                    let needs_rebind = sheet_node.value.is_none();
                    match value {
                        Value::Data(data) => sheet_node.value = Some(data),
                        Value::Expression(expr) => {
                            let queries = expr.args.iter().map(|path| self.unmanaged_query(path));
                            let queries = queries.collect_vec();
                            for query_index in &queries {
                                self.queries[*query_index].users.insert(sheet_node_index);
                            }
                            let bound_expr = BoundExpression::new(queries, expr.function);
                            let sheet_node = &mut self.nodes[sheet_node_index];
                            sheet_node.expr = Some(bound_expr);
                            self.recompute(sheet_node_index);
                        }
                    }
                    if needs_rebind {
                        let sheet_node = &self.nodes[sheet_node_index];
                        for query_index in sheet_node.matches.clone() {
                            if self.rebind_query(query_index) {
                                changed_queries.insert(query_index);
                            }
                        }
                    }
                }
            };
            sheet_node_index
        });

        // Recompute values in the whole graph.
        let nodes = sheet_nodes_iter.collect_vec();
        for sheet_node_index in self.sheet_nodes_topo_sort(nodes) {
            let sheet_node = &self.nodes[sheet_node_index];
            changed_queries.extend(&sheet_node.bindings);
            self.recompute(sheet_node_index);
        }

        for sheet_node_index in possible_orphans {
            self.drop_sheet_if_unused(sheet_node_index);
        }

        changed_queries
    }
}


// === Utils ===

impl SheetData {
    /// Check all potential candidates (sheet nodes) this query matches with and choose the most
    /// specific one. Returns true if the var was rebound.
    fn rebind_query(&mut self, query_index: Index<Query>) -> bool {
        let mut rebound = false;
        let mut found = false;
        let query = &self.queries[query_index];
        for sheet_node_index in query.matches.clone() {
            let sheet_node = &self.nodes[sheet_node_index];
            if sheet_node.exists() {
                if let Some(sheet_node_index) = query.binding {
                    self.nodes[sheet_node_index].bindings.remove(&query_index);
                }
                let query = &mut self.queries[query_index];
                let sheet_node = &mut self.nodes[sheet_node_index];
                let new_binding = Some(sheet_node_index);
                rebound = query.binding != new_binding;
                query.binding = new_binding;
                sheet_node.bindings.insert(query_index);
                found = true;
                break;
            }
        }
        if found {
            rebound
        } else {
            self.unbind_query(query_index)
        }
    }

    fn drop_query_if_unused(&mut self, query_index: Index<Query>) {
        let query_ref = &self.queries[query_index];
        if query_ref.is_unused() {
            if let Some(query) = self.queries.remove(query_index) {
                let node = self.query_map.get_or_create_node(&query.path.rev_segments);
                node.value = None;
                for sheet_node_index in query.matches {
                    let sheet_node = &mut self.nodes[sheet_node_index];
                    sheet_node.matches.remove(&query_index);
                    sheet_node.bindings.remove(&query_index);
                    self.drop_sheet_if_unused(sheet_node_index);
                }
            }
        }
    }

    fn drop_sheet_if_unused(&mut self, sheet_node_index: Index<SheetNode>) {
        let mut segments = self.nodes[sheet_node_index].path.rev_segments.clone();
        loop {
            if segments.is_empty() {
                break;
            }
            if let Some(node) = self.node_map.get_node(&segments) {
                let no_children = node.branches.is_empty();
                let sheet_node_index = node.value;
                let unused = self.nodes[sheet_node_index].is_unused();
                if no_children && unused {
                    self.nodes.remove(sheet_node_index);
                    self.node_map.remove(&segments);
                    segments.pop();
                } else {
                    break;
                }
            }
        }
    }

    /// Removes all binding information from var and related style nodes. Returns true if var
    /// needed rebound.
    fn unbind_query(&mut self, query_index: Index<Query>) -> bool {
        let query = &mut self.queries[query_index];
        match query.binding {
            None => false,
            Some(sheet_node_index) => {
                self.nodes[sheet_node_index].bindings.remove(&query_index);
                query.binding = None;
                true
            }
        }
    }

    /// Recomputes the value of the provided sheet node if it was assigned with an expression.
    fn recompute(&mut self, sheet_node_index: Index<SheetNode>) {
        let sheet_node = &self.nodes[sheet_node_index];
        let value = sheet_node.expr.as_ref().and_then(|expr| {
            let mut opt_args: Vec<Option<&Data>> = Vec::new();
            for query_index in &expr.args {
                opt_args.push(self.query_value(*query_index));
            }
            let args: Option<Vec<&Data>> = opt_args.into_iter().collect();
            args.map(|v| (expr.function)(&v))
        });
        let sheet_mut = &mut self.nodes[sheet_node_index];
        value.for_each(|v| sheet_mut.value = Some(v));
    }

    /// Traverse all sheet nodes whose value depend on the value of the provided sheet node and sort
    /// them in a topological order. This is used mainly for efficient implementation of sheet
    /// recomputation mechanism.
    fn sheet_nodes_topo_sort<T>(&self, changed_sheet_nodes: T) -> Vec<Index<SheetNode>>
    where T: Into<Vec<Index<SheetNode>>> {
        let changed_sheet_nodes = changed_sheet_nodes.into();
        let mut sheet_ref_count = HashMap::<Index<SheetNode>, usize>::new();
        let mut sorted_sheet_nodes = changed_sheet_nodes.clone();
        self.with_all_sheet_node_deps(&changed_sheet_nodes[..], |sheet_node_index| {
            *sheet_ref_count.entry(sheet_node_index).or_default() += 1;
        });
        self.with_all_sheet_node_deps(changed_sheet_nodes, |sheet_node_index| {
            let ref_count = sheet_ref_count.entry(sheet_node_index).or_default();
            *ref_count -= 1;
            if *ref_count == 0 {
                sorted_sheet_nodes.push(sheet_node_index);
            }
        });
        sorted_sheet_nodes
    }

    /// Runs the provided callback with all sheet node indexes whose value depend on the values of
    /// the provided nodes.
    fn with_all_sheet_node_deps<T, F>(&self, targets: T, mut callback: F)
    where
        T: Into<Vec<Index<SheetNode>>>,
        F: FnMut(Index<SheetNode>), {
        let mut sheet_nodes_to_visit = targets.into();
        while !sheet_nodes_to_visit.is_empty() {
            if let Some(current_sheet_id) = sheet_nodes_to_visit.pop() {
                let sheet_node = &self.nodes[current_sheet_id];
                for query_index in &sheet_node.bindings {
                    let query = &self.queries[*query_index];
                    for sheet_node_index in &query.users {
                        callback(*sheet_node_index);
                        sheet_nodes_to_visit.push(*sheet_node_index);
                    }
                }
            }
        }
    }
}


// === Debug ===

impl SheetData {
    /// Visualizes the network in the GraphViz Dot language. Use `visualize` to automatically
    /// display it in a new browser tab.
    pub fn to_graphviz(&self) -> String {
        let mut dot = String::new();
        self.sheet_node_map_to_graphviz(&mut dot, &self.node_map);
        self.query_map_to_graphviz(&mut dot, &mut vec![], &self.query_map);
        let s = &mut dot;
        for q in &self.queries {
            for node in &q.matches {
                Self::query_sheet_node_link(s, q.index, *node, "[style=dashed]")
            }
            for node in &q.binding {
                Self::query_sheet_node_link(s, q.index, *node, "[color=red]")
            }
            for node in &q.users {
                Self::query_sheet_node_link(s, q.index, *node, "[color=blue]")
            }
        }
        for node in &self.nodes {
            for q in &node.matches {
                Self::sheet_node_query_link(s, node.index, *q, "[style=dashed]")
            }
            for q in &node.bindings {
                Self::sheet_node_query_link(s, node.index, *q, "[color=red]")
            }
            for expr in &node.expr {
                for q in &expr.args {
                    Self::sheet_node_query_link(s, node.index, *q, "[color=blue]")
                }
            }
        }
        format!("digraph G {{\nnode [shape=box style=rounded]\n{}\n}}", dot)
    }

    fn sheet_node_map_to_graphviz(&self, dot: &mut String, node_map: &NodeMap) {
        let sheet_node_index = node_map.value;
        let sheet_node = &self.nodes[sheet_node_index];
        let value = format!("{:?}", sheet_node.value);
        let label = iformat!("sheet_{sheet_node_index}({value})");
        dot.push_str(&iformat!("sheet_{sheet_node_index} [label=\"{label}\"]\n"));
        for (path, child) in &node_map.branches {
            let attrs = iformat!("[label=\"{path}\"]");
            Self::sheet_node_sheet_node_link(dot, sheet_node_index, child.value, attrs);
            self.sheet_node_map_to_graphviz(dot, child);
        }
    }

    fn query_map_to_graphviz(
        &self,
        dot: &mut String,
        path: &mut Vec<String>,
        query_map: &QueryMap,
    ) {
        query_map.value.for_each(|query_index| {
            let query = &self.queries[query_index];
            let scope = if query.is_external() { "External" } else { "Internal" };
            let real_path = path.iter().rev().join(".");
            dot.push_str(&iformat!("query_{query_index} [label=\"{scope} Query({real_path})\"]\n"));
        });
        for (segment, child) in &query_map.branches {
            path.push(segment.into());
            self.query_map_to_graphviz(dot, path, child);
            path.pop();
        }
    }

    fn query_sheet_node_link<S>(
        dot: &mut String,
        query_index: Index<Query>,
        sheet_node_index: Index<SheetNode>,
        s: S,
    ) where
        S: Into<String>,
    {
        Self::link(dot, "query", "sheet", query_index, sheet_node_index, s)
    }

    fn sheet_node_query_link<S>(
        dot: &mut String,
        sheet_node_index: Index<SheetNode>,
        query_index: Index<Query>,
        s: S,
    ) where
        S: Into<String>,
    {
        Self::link(dot, "sheet", "query", sheet_node_index, query_index, s)
    }

    fn sheet_node_sheet_node_link<S>(
        dot: &mut String,
        sheet_node_index_1: Index<SheetNode>,
        sheet_node_index_2: Index<SheetNode>,
        s: S,
    ) where
        S: Into<String>,
    {
        Self::link(dot, "sheet", "sheet", sheet_node_index_1, sheet_node_index_2, s)
    }

    fn link<Src, Tgt, S>(dot: &mut String, src_pfx: &str, tgt_pfx: &str, src: Src, tgt: Tgt, s: S)
    where
        Src: Display,
        Tgt: Display,
        S: Into<String>, {
        dot.push_str(&format!("{}_{} -> {}_{} {}\n", src_pfx, src, tgt_pfx, tgt, s.into()));
    }
}

// === Impls ===

impl Default for SheetData {
    fn default() -> Self {
        Self::new()
    }
}



// ===========
// === Var ===
// ===========

/// `Var` is a `Query` with automatically managed lifetime. Read `Query` docs to learn more.
#[derive(Clone, CloneRef, Debug, Deref)]
pub struct Var {
    rc: Rc<VarData>,
}

/// Internal state of `Var`.
#[derive(Debug)]
pub struct VarData {
    sheet:       Sheet,
    query_index: Index<Query>,
    callbacks:   CallbackRegistry,
}

impl VarData {
    /// Constructor.
    pub fn new<R>(sheet: R, query_index: Index<Query>, callbacks: CallbackRegistry) -> Self
    where R: Into<Sheet> {
        let sheet = sheet.into();
        sheet.rc.borrow_mut().queries[query_index].inc_external_count();
        Self { sheet, query_index, callbacks }
    }

    /// Adds a new callback used when value changes. Returns handle to the callback. As soon as the
    /// handle is dropped, the callback is removed.
    pub fn on_change<F>(&self, callback: F) -> callback::Handle
    where F: 'static + FnMut(&Option<Data>) {
        self.callbacks.add(callback)
    }

    /// Queries the style sheet for the current value of the var.
    pub fn value(&self) -> Option<Data> {
        self.sheet.rc.borrow().query_value(self.query_index).cloned()
    }
}

impl Drop for VarData {
    fn drop(&mut self) {
        self.sheet.callbacks.borrow_mut().remove(&self.query_index);
        let sheet_data = &mut *self.sheet.rc.borrow_mut();
        sheet_data.queries[self.query_index].dec_external_count();
        sheet_data.drop_query_if_unused(self.query_index);
    }
}

impl Var {
    /// Constructor.
    pub fn new<R>(sheet: R, query_index: Index<Query>, callbacks: CallbackRegistry) -> Self
    where R: Into<Sheet> {
        let rc = Rc::new(VarData::new(sheet, query_index, callbacks));
        Self { rc }
    }

    /// Return weak reference to this var.
    pub fn downgrade(&self) -> WeakVar {
        WeakVar { weak: Rc::downgrade(&self.rc) }
    }
}



// ===============
// === WeakVar ===
// ===============

/// Weak reference to [`Var`].
#[derive(Clone, CloneRef, Debug, Deref)]
pub struct WeakVar {
    weak: Weak<VarData>,
}

impl WeakVar {
    /// Upgrade this weak reference to strong var reference.
    pub fn upgrade(&self) -> Option<Var> {
        self.weak.upgrade().map(|rc| Var { rc })
    }
}



// =============
// === Sheet ===
// =============

/// Cascading style sheet. Implemented as a tree of `SheetNode`s. Each style sheet node can be
/// assigned with a value of type `Data` or an expression to compute one. It also allows creating
/// variables which are automatically bound to the most specific style sheet. See `Var`, `Query` ,
/// and `Sheet` to learn more.
#[derive(Clone, CloneRef, Debug, Default)]
pub struct Sheet {
    rc:        Rc<RefCell<SheetData>>,
    /// Keeps already used variables. When dropping the last variable for a given path, it's
    /// associated [`Query`] is dropped as well, so it's necessary to provide the same variable
    /// for the given [`Path`]. See the [`var`] method to learn more.
    cache:     Rc<RefCell<HashMap<String, WeakVar>>>,
    callbacks: Rc<RefCell<HashMap<Index<Query>, CallbackRegistry>>>,
}

/// Type of callback registry used in `Sheet`.
pub type CallbackRegistry = callback::SharedRegistryMut1<Option<Data>>;

impl Sheet {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Creates a new style sheet `Var`. It creates a clone-ref of an existing variable (in case not
    /// all references were dropped so far).
    pub fn var<P>(&self, path: P) -> Var
    where P: Into<Path> {
        let path = path.into();
        let path_str = path.to_string();
        let opt_var = self.cache.borrow().get(&path_str).and_then(|t| t.upgrade());
        opt_var.unwrap_or_else(|| {
            let query_ix = self.rc.borrow_mut().unmanaged_query(path);
            let callback_reg = self.callbacks.borrow_mut().entry(query_ix).or_default().clone_ref();
            let var = Var::new(self, query_ix, callback_reg);
            self.cache.borrow_mut().insert(path_str, var.downgrade());
            var
        })
    }

    /// Sets the value by the given path.
    pub fn set<P, V>(&self, path: P, value: V)
    where
        P: Into<Path>,
        V: Into<Value>, {
        let value = value.into();
        self.apply_change(Change::new(path, Some(value)))
    }

    /// Removes the value by the given path.
    pub fn unset<P>(&self, path: P)
    where P: Into<Path> {
        self.apply_change(Change::new(path, None))
    }

    /// Changes the value by the given path. Providing `None` as the value means that the value
    /// will be removed.
    pub fn change<P>(&self, path: P, value: Option<Value>)
    where P: Into<Path> {
        self.apply_change(Change::new(path, value))
    }

    /// Apply a `Change`.
    pub fn apply_change(&self, change: Change) {
        self.apply_changes(iter::once(change))
    }

    /// Apply a set of `Change`s.
    pub fn apply_changes<I>(&self, changes: I)
    where I: IntoIterator<Item = Change> {
        let changed = self.rc.borrow_mut().apply_changes(changes);
        for query_index in changed {
            self.run_callbacks_for(query_index);
        }
    }

    /// Queries the style sheet for a value of a path like it was a variable. For example,
    /// querying "button.size" will return the value of "size" if no exact match was found.
    pub fn query<P>(&self, path: P) -> Option<Data>
    where P: Into<Path> {
        self.rc.borrow().query(path).cloned()
    }

    /// Reads the value of the style sheet by the exact path provided. If you want to read a value
    /// of a variable binding, use `query` instead.
    pub fn value<P>(&self, path: P) -> Option<Data>
    where P: Into<Path> {
        self.rc.borrow().value(path).cloned()
    }

    /// Visualizes the network in the GraphViz Dot language. Use `visualize` to automatically
    /// display it in a new browser tab.
    pub fn to_graphviz(&self) -> String {
        self.rc.borrow().to_graphviz()
    }
}


// === Private ===

impl Sheet {
    /// Runs callbacks registered for the given variable id.
    fn run_callbacks_for(&self, query_index: Index<Query>) {
        let callbacks_opt = self.callbacks.borrow().get(&query_index).map(|t| t.clone_ref());
        if let Some(callbacks) = callbacks_opt {
            // FIXME: The value should not be cloned here.
            let value = self.rc.borrow().query_value(query_index).cloned();
            callbacks.run_all(&value)
        }
    }
}


// === Debug ===

impl Sheet {
    /// Returns number of `Query` instances used by the engine.
    pub fn debug_queries_count(&self) -> usize {
        self.rc.borrow().queries_count()
    }

    /// Returns number of `Sheet` instances used by the engine.
    pub fn debug_sheet_nodes_count(&self) -> usize {
        self.rc.borrow().sheet_nodes_count()
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_query_sheet_count(style: &Sheet, queries_count: usize, sheet_nodes_count: usize) {
        assert_eq!(style.debug_queries_count(), queries_count);
        assert_eq!(style.debug_sheet_nodes_count(), sheet_nodes_count);
    }

    #[test]
    pub fn memory_management_for_single_value() {
        let sheet = Sheet::new();
        sheet.set("size", data(1.0));
        assert_query_sheet_count(&sheet, 0, 1);
        sheet.unset("size");
        assert_query_sheet_count(&sheet, 0, 0);
    }

    #[test]
    pub fn memory_management_for_multiple_values() {
        let sheet = Sheet::new();
        sheet.set("size", data(1.0));
        sheet.set("button.size", data(2.0));
        sheet.set("circle.radius", data(3.0));
        assert_query_sheet_count(&sheet, 0, 4);
        sheet.unset("size");
        assert_query_sheet_count(&sheet, 0, 4);
        sheet.unset("button.size");
        assert_query_sheet_count(&sheet, 0, 2);
        sheet.unset("circle.radius");
        assert_query_sheet_count(&sheet, 0, 0);
    }

    #[test]
    pub fn memory_management_for_single_expression() {
        let sheet = Sheet::new();
        sheet.set("button.size", data(1.0));
        assert_query_sheet_count(&sheet, 0, 2);
        sheet.set("circle.radius", Expression::new(&["button.size"], |args| args[0] + &data(10.0)));
        assert_query_sheet_count(&sheet, 1, 4);
        assert_eq!(sheet.value("circle.radius"), Some(data(11.0)));
        sheet.unset("button.size");
        assert_query_sheet_count(&sheet, 1, 4);
        assert_eq!(sheet.value("circle.radius"), Some(data(11.0))); // Impossible to update.
        sheet.set("button.size", data(2.0));
        assert_query_sheet_count(&sheet, 1, 4);
        assert_eq!(sheet.value("circle.radius"), Some(data(12.0)));
        sheet.set("circle.radius", data(3.0));
        assert_query_sheet_count(&sheet, 0, 4);
        sheet.unset("button.size");
        assert_query_sheet_count(&sheet, 0, 2);
        sheet.unset("circle.radius");
        assert_query_sheet_count(&sheet, 0, 0);
    }

    #[test]
    pub fn single_variable() {
        let sheet = Sheet::new();
        let val = Rc::new(RefCell::new(None));
        let var = sheet.var("button_size");
        let handle = var.on_change(f!([val](v: &Option<Data>) * val.borrow_mut() = v.clone()));
        assert_query_sheet_count(&sheet, 1, 1);
        assert_eq!(var.value(), None);
        sheet.set("size", data(1.0));
        assert_query_sheet_count(&sheet, 1, 2);
        assert_eq!(*val.borrow(), None);
        assert_eq!(var.value(), None);
        sheet.set("button_size", data(2.0));
        assert_query_sheet_count(&sheet, 1, 2);
        assert_eq!(*val.borrow(), Some(data(2.0)));
        assert_eq!(var.value(), Some(data(2.0)));
        drop(handle);
        sheet.set("button_size", data(3.0));
        assert_query_sheet_count(&sheet, 1, 2);
        assert_eq!(*val.borrow(), Some(data(2.0)));
        assert_eq!(var.value(), Some(data(3.0)));
        sheet.unset("button_size");
        assert_query_sheet_count(&sheet, 1, 2);
        assert_eq!(*val.borrow(), Some(data(2.0)));
        assert_eq!(var.value(), None);
        drop(var);
        assert_query_sheet_count(&sheet, 0, 1);
        sheet.unset("size");
        assert_query_sheet_count(&sheet, 0, 0);
    }

    #[test]
    pub fn single_nested_variable() {
        let sheet = Sheet::new();
        let val = Rc::new(RefCell::new(None));
        let var = sheet.var("button.size");
        let handle = var.on_change(f!([val](v: &Option<Data>) * val.borrow_mut() = v.clone()));
        assert_query_sheet_count(&sheet, 1, 2);
        {
            let _var2 = sheet.var("button.size");
            assert_query_sheet_count(&sheet, 1, 2);
        }
        assert_query_sheet_count(&sheet, 1, 2);
        assert_eq!(var.value(), None);
        sheet.set("size", data(1.0));
        assert_query_sheet_count(&sheet, 1, 2);
        assert_eq!(*val.borrow(), Some(data(1.0)));
        assert_eq!(var.value(), Some(data(1.0)));
        sheet.set("button.size", data(2.0));
        assert_query_sheet_count(&sheet, 1, 2);
        assert_eq!(*val.borrow(), Some(data(2.0)));
        assert_eq!(var.value(), Some(data(2.0)));
        drop(handle);
        sheet.set("button.size", data(3.0));
        assert_query_sheet_count(&sheet, 1, 2);
        assert_eq!(*val.borrow(), Some(data(2.0)));
        assert_eq!(var.value(), Some(data(3.0)));
        sheet.unset("button.size");
        assert_query_sheet_count(&sheet, 1, 2);
        assert_eq!(*val.borrow(), Some(data(2.0)));
        assert_eq!(var.value(), Some(data(1.0)));
        drop(var);
        assert_query_sheet_count(&sheet, 0, 1);
        sheet.unset("size");
        assert_query_sheet_count(&sheet, 0, 0);
    }

    #[test]
    pub fn variable_unbind() {
        let sheet = Sheet::new();
        let val = Rc::new(RefCell::new(None));
        let var = sheet.var("button.size");
        let _handle = var.on_change(f!([val](v: &Option<Data>) * val.borrow_mut() = v.clone()));
        assert_query_sheet_count(&sheet, 1, 2);
        assert_eq!(var.value(), None);
        sheet.set("size", data(1.0));
        assert_query_sheet_count(&sheet, 1, 2);
        assert_eq!(*val.borrow(), Some(data(1.0)));
        assert_eq!(var.value(), Some(data(1.0)));
        sheet.unset("size");
        assert_query_sheet_count(&sheet, 1, 2);
        assert_eq!(*val.borrow(), None);
        assert_eq!(var.value(), None);
        drop(var);
        assert_query_sheet_count(&sheet, 0, 0);
    }

    #[test]
    pub fn simple_query_binding_1() {
        let mut style = SheetData::new();
        let query1 = style.unmanaged_query("size");
        assert!(style.query_value(query1).is_none());
        style.set("size", data(1.0));
        assert_eq!(style.query_value(query1), Some(&data(1.0)));
    }

    #[test]
    pub fn simple_query_binding_2() {
        let mut style = SheetData::new();
        style.set("size", data(1.0));
        let query1 = style.unmanaged_query("size");
        assert_eq!(style.query_value(query1), Some(&data(1.0)));
    }

    #[test]
    pub fn hierarchical_query_binding() {
        let mut style = SheetData::new();
        let query1 = style.unmanaged_query("graph.button.size");
        assert!(style.query_value(query1).is_none());
        style.set("size", data(1.0));
        assert_eq!(style.query_value(query1), Some(&data(1.0)));
        style.set("button.size", data(2.0));
        assert_eq!(style.query_value(query1), Some(&data(2.0)));
        style.set("graph.button.size", data(3.0));
        assert_eq!(style.query_value(query1), Some(&data(3.0)));
        style.unset("graph.button.size");
        assert_eq!(style.query_value(query1), Some(&data(2.0)));
        style.unset("button.size");
        assert_eq!(style.query_value(query1), Some(&data(1.0)));
        style.unset("size");
        assert_eq!(style.query_value(query1), None);
    }

    #[test]
    pub fn expr_bindings_1() {
        let mut style = SheetData::new();

        let _query_size = style.unmanaged_query("size");
        let _query_button_size = style.unmanaged_query("button.size");
        let query_graph_button_size = style.unmanaged_query("graph.button.size");

        assert!(style.query_value(query_graph_button_size).is_none());
        style.set("size", data(1.0));
        assert_eq!(style.query_value(query_graph_button_size), Some(&data(1.0)));
        style.set(
            "graph.button.size",
            Expression::new(&["button.size"], |args| args[0] + &data(10.0)),
        );
        assert_eq!(style.query_value(query_graph_button_size), Some(&data(11.0)));
        style.set("button.size", Expression::new(&["size"], |args| args[0] + &data(100.0)));
        assert_eq!(style.query_value(query_graph_button_size), Some(&data(111.0)));
        style.set("size", data(2.0));
        assert_eq!(style.query_value(query_graph_button_size), Some(&data(112.0)));
        style.set("button.size", data(3.0));
        assert_eq!(style.query_value(query_graph_button_size), Some(&data(13.0)));
        style.set("button.size", data(4.0));
        assert_eq!(style.query_value(query_graph_button_size), Some(&data(14.0)));
    }

    #[test]
    pub fn expr_circular() {
        let mut style = SheetData::new();
        style.set("a", Expression::new(&["b"], |args| args[0].clone()));
        style.set("b", Expression::new(&["a"], |args| args[0].clone()));
        assert!(style.value("a").is_none());
        assert!(style.value("b").is_none());
    }
}
