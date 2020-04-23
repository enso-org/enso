//! This module defines a cascading style sheet registry and related style management utilities.

pub mod data;
pub mod path;

use crate::prelude::*;
use crate::data::HashMapTree;
use crate::data::Index;
use crate::data::OptVec;

pub use self::data::Data;
pub use self::data::data;
pub use self::path::Path;



// ===========
// === Var ===
// ===========

/// Data of a style variable. Variables are associated with a style path like 'panel.button.size'
/// and are automatically bound to the most specific style sheet as soon as it gets defined. By
/// most specific, we mean the one with the longest path. For example, the 'panel.button.size' var
/// will be bound to one of 'panel.button.size', 'button.size', or 'size' if defined, in that order.
#[derive(Debug)]
pub struct Var {
    /// Index of the var in the style var map.
    index : Index<Var>,
    /// Set of all `Sheet` indexes which are potential matches of this var. For example, for a var
    /// 'panel.button.size', all of the following sheets will be included here: 'panel.button.size',
    /// 'button.size', and 'size'.
    matches : Vec<Index<Sheet>>,
    /// Index of the most specific `Sheet` from `matches` which has a defined value if any.
    binding : Option<Index<Sheet>>,
    /// List of all `Sheet`s which use this var in their expressions.
    usages : HashSet<Index<Sheet>>,
}

impl Var {
    /// Constructor.
    pub fn new(index:Index<Var>) -> Self {
        let matches = default();
        let binding = default();
        let usages  = default();
        Self {index,matches,binding,usages}
    }
}



// =============
// === Sheet ===
// =============

/// A node in the style sheet tree. Style sheets are associated with a style path like
/// 'panel.button.size' and each node keeps a `Data` value. The value can either be set explicitly,
/// or computed automatically if the style sheet is defined with en `Expression`. Please note that
/// although `Sheet` contains a single value, it is in fact a node in a tree defined in `Registry`,
/// so it can be interpreted as a set of hierarchical values instead.
#[derive(Debug)]
pub struct Sheet {
    /// Index of the style sheet in the style sheet map.
    index : Index<Sheet>,
    /// Value of this style sheet node. Style sheets without value behave like if they do not exist.
    value : Option<Data>,
    /// Expression used to update the value.
    expr : Option<Expression>,
    /// Indexes of all `Var`s that are potential matches with this style sheet.
    matches : HashSet<Index<Var>>,
    /// Indexes of all `Var`s that are bound (best matches) with this style sheet.
    bindings : HashSet<Index<Var>>,
}

impl Sheet {
    /// Constructor.
    pub fn new(index:Index<Sheet>) -> Self {
        let value    = default();
        let expr     = default();
        let matches  = default();
        let bindings = default();
        Self {index,value,expr,matches,bindings}
    }

    /// Checks whether the style sheet exist. Style sheets without value are considered templates
    /// and are kept in the graph for optimization purposes only.
    pub fn exists(&self) -> bool {
        self.value.is_some()
    }
}


// ==================
// === Expression ===
// ==================

/// Expression of a style sheet.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct Expression {
    /// Indexes of all vars which are used as sources to the function of this expression.
    sources : Vec<Index<Var>>,
    /// Function used to compute the new value of the style sheet.
    #[derivative(Debug="ignore")]
    function : Box<dyn Fn(&[&Data])->Data>
}



// =============
// === Types ===
// =============

// === Types ===

#[allow(missing_docs)]
mod types {
    use super::*;
    pub type VarVec   = OptVec<Var,Index<Var>>;
    pub type SheetVec = OptVec<Sheet,Index<Sheet>>;
    pub type VarMap   = HashMapTree<String,Option<Index<Var>>>;
    pub type SheetMap = HashMapTree<String,Index<Sheet>>;
}
use types::*;

trait NewInstance<K> {
    fn new_instance(&mut self) -> K;
}

impl NewInstance<Index<Var>> for VarVec {
    fn new_instance(&mut self) -> Index<Var> {
        self.insert_with_ix(Var::new)
    }
}

impl NewInstance<Index<Sheet>> for SheetVec {
    fn new_instance(&mut self) -> Index<Sheet> {
        self.insert_with_ix(Sheet::new)
    }
}



// ================
// === Registry ===
// ================

/// Style sheet registry. Could be named "Cascading Style Sheets" but then the name will be
/// confusing with CSS used in web development. Defines a set of cascading style sheets. Each
/// style sheet can be assigned with a value of type `Data` or an expression to compute one. It
/// also allows creating variables which are automatically bound to the most specific style sheet.
/// See `Var` and `Sheet` to learn more.
#[derive(Debug)]
pub struct Registry {
    /// Set of all variables.
    pub vars : VarVec,
    /// Set of all style sheets.
    pub sheets : SheetVec,
    /// Association of a path like 'button' -> 'size' to a variable.
    pub var_map : VarMap,
    /// Association of a path like 'button' -> 'size' to a style sheet.
    pub sheet_map : SheetMap,
}


// === Constructors ===

impl Registry {
    /// Constructor.
    pub fn new() -> Self {
        let vars          = default();
        let mut sheets    = OptVec::<Sheet,Index<Sheet>>::new();
        let var_map       = default();
        let root_sheet_id = sheets.new_instance();
        let sheet_map     = SheetMap::from_value(root_sheet_id);
        Self {vars,sheets,var_map,sheet_map}
    }

    /// Access variable by the given path or create new one if missing.
    ///
    /// Implementation note: under the hood, a `Sheet` for each sub-path will be created. For
    /// example, when creating "panel.button.size" variable, three sheets will be created as well:
    /// "panel.button.size", "button.size", and "size". This way we keep track of all possible
    /// matches and we can create high-performance value binding algorithms.
    pub fn var<P:Into<Path>>(&mut self, path:P) -> Index<Var> {
        let path         = path.into();
        let vars         = &mut self.vars;
        let sheets       = &mut self.sheets;
        let var_map_node = self.var_map.get_node(&path.rev_segments);
        let var_id       = *var_map_node.value_or_set_with(||vars.new_instance());

        let mut var_matches = Vec::new();
        self.sheet_map.get_node_traversing_with(&path.rev_segments,||{sheets.new_instance()}, |t| {
            var_matches.push(t.value)
        });
        var_matches.reverse();

        for sheet_id in &var_matches {
            self.sheets[*sheet_id].matches.insert(var_id);
        }

        self.vars[var_id].matches = var_matches;
        self.rebind_var(var_id);
        var_id
    }

    /// Access style sheet by the given path or create new one if missing.
    fn sheet<P:Into<Path>>(&mut self, path:P) -> Index<Sheet> {
        let path   = path.into();
        let sheets = &mut self.sheets;
        let node   = self.sheet_map.get_node_with(&path.rev_segments,|| sheets.new_instance());
        node.value
    }
}


// === Getters ===

impl Registry {
    /// Reads the value of the variable.
    pub fn value(&self, var_id:Index<Var>) -> Option<&Data> {
        self.vars.safe_index(var_id).as_ref().and_then(|var| {
            var.binding.and_then(|sheet_id| {
                self.sheets[sheet_id].value.as_ref()
            })
        })
    }
}


// === Setters ===

impl Registry {
    /// Set a style sheet value. Please note that it will remove expression assigned to the target
    /// style sheet if any.
    pub fn set_value<P:Into<Path>>(&mut self, path:P, data:Data) {
        self.set_value_to(path,Some(data))
    }

    /// Removes a style sheet value. Please note that it will remove expression assigned to the
    /// target style sheet if any.
    pub fn remove_value<P:Into<Path>>(&mut self, path:P) {
        self.set_value_to(path,None)
    }

    /// Set or unset a style sheet value. Please note that it will remove expression assigned to the
    /// target style sheet if any.
    pub fn set_value_to<P:Into<Path>>(&mut self, path:P, data:Option<Data>) {
        let path = path.into();
        self.remove_expression(&path);
        let sheet_id = self.sheet(&path);
        let sheet    = &mut self.sheets[sheet_id];
        sheet.value  = data;
        for var_id in sheet.matches.clone() {
            self.rebind_var(var_id)
        }
        for sheet_id in self.sheet_topo_sort(sheet_id) {
            self.recompute(sheet_id);
        }
    }

    /// Set a style sheet expression which will be used to automatically compute values whenever any
    /// of the provided dependencies will change.
    pub fn set_expression<P,F>(&mut self, path:P, args:&[Index<Var>], function:F)
    where P:Into<Path>, F:'static+Fn(&[&Data])->Data {
        let sheet_id = self.sheet(path);
        let sheet    = &mut self.sheets[sheet_id];
        let sources  = args.to_vec();
        let function = Box::new(function);
        sheet.expr   = Some(Expression {sources,function});
        for var_id in args {
            self.vars[*var_id].usages.insert(sheet_id);
        }
        self.rebind_and_recompute(sheet_id);
    }
}


// === Utils ===

impl Registry {
    /// Check all potential candidates (sheets) this variable matches to and choose the most
    /// specific one from those which exist (have a value).
    fn rebind_var(&mut self, var_id:Index<Var>) {
        let mut found = false;
        let var       = &self.vars[var_id];
        for sheet_id in var.matches.clone() {
            let sheet = &self.sheets[sheet_id];
            if sheet.exists() {
                if let Some(sheet_id) = var.binding {
                    self.sheets[sheet_id].bindings.remove(&var_id);
                }
                let var   = &mut self.vars[var_id];
                let sheet = &mut self.sheets[sheet_id];
                var.binding = Some(sheet_id);
                sheet.bindings.insert(var_id);
                found = true;
                break
            }
        }
        if !found { self.unbind_var(var_id) }
    }

    /// Removes all binding information from var and related style sheets.
    fn unbind_var(&mut self, var_id:Index<Var>) {
        let var = &self.vars[var_id];
        var.binding.for_each(|sheet_id| {
            self.sheets[sheet_id].bindings.remove(&var_id);
        });
        let var = &mut self.vars[var_id];
        var.binding = None;
    }

    /// Internal utility for removing style sheet expression.
    fn remove_expression<P>(&mut self, path:P)
        where P:Into<Path> {
        let sheet_id = self.sheet(path);
        let sheet    = &mut self.sheets[sheet_id];
        if sheet.expr.is_some() {
            sheet.value = None;
            let opt_expr = mem::take(&mut sheet.expr);
            if let Some(expr) = opt_expr {
                for var_id in expr.sources {
                    self.vars[var_id].usages.remove(&sheet_id);
                }
            }
            self.rebind_and_recompute(sheet_id);
        }
    }

    /// Internal utility which recomputes the provided style sheet, rebinds related variables, and
    /// recomputes all dependent style sheets.
    fn rebind_and_recompute(&mut self, sheet_id:Index<Sheet>) {
        self.recompute(sheet_id);
        let sheet = &mut self.sheets[sheet_id];
        for var_id in sheet.matches.clone() {
            self.rebind_var(var_id)
        }
        for sheet_id in self.sheet_topo_sort(sheet_id) {
            self.recompute(sheet_id);
        }
    }

    /// Recomputes the value of the provided sheet if the sheet was assigned with an expression.
    fn recompute(&mut self, sheet_id:Index<Sheet>) {
        let sheet = &self.sheets[sheet_id];
        let value = sheet.expr.as_ref().and_then(|expr| {
            let mut opt_values : Vec<Option<&Data>> = Vec::new();
            for var_id in &expr.sources {
                opt_values.push(self.value(*var_id));
            }
            let values : Option<Vec<&Data>> = opt_values.into_iter().collect();
            values.map(|v| (expr.function)(&v) )
        });
        let sheet_mut = &mut self.sheets[sheet_id];
        value.for_each(|v| sheet_mut.value = Some(v));
    }

    /// Traverses all sheets whose value depend on the value of the provided sheet and sorts them
    /// in a topological order. This is used mainly for efficient implementation of sheet
    /// recomputation mechanism.
    fn sheet_topo_sort(&self, changed_sheet_id:Index<Sheet>) -> Vec<Index<Sheet>> {
        let mut sheet_ref_count = HashMap::<Index<Sheet>,usize>::new();
        let mut sorted_sheets   = vec![changed_sheet_id];
        self.with_all_sheet_deps(changed_sheet_id, |sheet_id| {
            *sheet_ref_count.entry(sheet_id).or_default() += 1;
        });
        self.with_all_sheet_deps(changed_sheet_id, |sheet_id| {
            let ref_count = sheet_ref_count.entry(sheet_id).or_default();
            *ref_count -= 1;
            if *ref_count == 0 {
                sorted_sheets.push(sheet_id);
            }
        });
        sorted_sheets
    }

    /// Runs the provided callback with all sheet indexes whose value depend on the value of the
    /// provided sheet.
    fn with_all_sheet_deps<F>(&self, target:Index<Sheet>, mut callback:F)
    where F:FnMut(Index<Sheet>) {
        let mut sheets_to_visit = vec![target];
        while !sheets_to_visit.is_empty() {
            if let Some(current_sheet_id) = sheets_to_visit.pop() {
                let sheet = &self.sheets[current_sheet_id];
                for var_id in &sheet.bindings {
                    let var = &self.vars[*var_id];
                    for sheet_id in &var.usages {
                        callback(*sheet_id);
                        sheets_to_visit.push(*sheet_id);
                    }
                }
            }
        }
    }
}


// === Debug ===

impl Registry {
    /// Visualizes the network in the GraphViz Dot language. Use `visualize` to automatically
    /// display it in a new browser tab.
    pub fn to_graphviz(&self) -> String {
        let mut dot = String::new();
        Self::sheet_map_to_graphviz(&mut dot,&self.sheet_map);
        Self::var_map_to_graphviz(&mut dot,&mut vec![],&self.var_map);
        let s = &mut dot;
        for var in &self.vars {
            for sheet in &var.matches {Self::var_sheet_link(s,var.index,*sheet,"[style=dashed]")}
            for sheet in &var.binding {Self::var_sheet_link(s,var.index,*sheet,"[color=red]")}
            for sheet in &var.usages  {Self::var_sheet_link(s,var.index,*sheet,"[color=blue]")}
        }
        for sheet in &self.sheets {
            for var  in &sheet.matches  {Self::sheet_var_link(s,sheet.index,*var,"[style=dashed]")}
            for var  in &sheet.bindings {Self::sheet_var_link(s,sheet.index,*var,"[color=red]")}
            for expr in &sheet.expr {
                for var in &expr.sources {Self::sheet_var_link(s,sheet.index,*var,"[color=blue]")}
            }
        }
        format!("digraph G {{\nnode [shape=box style=rounded]\n{}\n}}",dot)
    }

    fn sheet_map_to_graphviz(dot:&mut String, sheet_map:&SheetMap) {
        let sheet_id = sheet_map.value;
        dot.push_str(&iformat!("sheet_{sheet_id}\n"));
        for (path,child) in sheet_map {
            Self::sheet_sheet_link(dot,sheet_id,child.value,iformat!("[label=\"{path}\"]"));
            Self::sheet_map_to_graphviz(dot,child);
        }
    }

    fn var_map_to_graphviz(dot:&mut String, path:&mut Vec<String>, var_map:&VarMap) {
        var_map.value.for_each(|var_id| {
            let real_path = path.iter().rev().join(".");
            dot.push_str(&iformat!("var_{var_id} [label=\"Var({real_path})\"]\n"));
        });
        for (segment,child) in var_map {
            path.push(segment.into());
            Self::var_map_to_graphviz(dot,path,child);
            path.pop();
        }
    }

    fn var_sheet_link<S>(dot:&mut String, var_id:Index<Var>, sheet_id:Index<Sheet>, s:S)
    where S:Into<String> {
        Self::link(dot,"var","sheet",var_id,sheet_id,s)
    }

    fn sheet_var_link<S>(dot:&mut String, sheet_id:Index<Sheet>, var_id:Index<Var>, s:S)
    where S:Into<String> {
        Self::link(dot,"sheet","var",sheet_id,var_id,s)
    }

    fn sheet_sheet_link<S>(dot:&mut String, sheet_id_1:Index<Sheet>, sheet_id_2:Index<Sheet>, s:S)
    where S:Into<String> {
        Self::link(dot,"sheet","sheet",sheet_id_1,sheet_id_2,s)
    }

    fn link<Src,Tgt,S>(dot:&mut String, src_pfx:&str, tgt_pfx:&str, src:Src, tgt:Tgt, s:S)
    where Src:Display, Tgt:Display, S:Into<String> {
        dot.push_str(&format!("{}_{} -> {}_{} {}\n",src_pfx,src,tgt_pfx,tgt,s.into()));
    }
}


// === Impls ===

impl Default for Registry {
    fn default() -> Self {
        Self::new()
    }
}



// =============
// === Tests ===
// =============

/// Interactive testing utility. To be removed in the future.
pub fn test() {
    let mut style = Registry::new();

    let var_size              = style.var("size");
    let var_button_size       = style.var("button.size");
    let var_graph_button_size = style.var("graph.button.size");
    let _var = style.var("scene.background.color");
    let _var = style.var("application.text.color");
    let _var = style.var("application.text.size");
    let _var = style.var("button.text.size");
    let _var = style.var("node.text.color");
    let _var = style.var("node.text.size");
    let _var = style.var("application.background.color");
    let _var = style.var("node.background.color");

    assert!(style.value(var_graph_button_size).is_none());
    style.set_value("size",data(1.0));
    style.set_expression("graph.button.size",&[var_button_size],|args| args[0] + &data(100.0));
    style.set_expression("button.size",&[var_size],|args| args[0] + &data(10.0));
    style.set_value("button.size",data(3.0));

    println!("{}",style.to_graphviz());
    println!("{:?}", style.value(var_graph_button_size));
    println!("{:?}", style.value(var_button_size));
    println!("{:?}", style.vars[var_graph_button_size]);
    println!("{:?}", style.sheets[style.vars[var_graph_button_size].binding.unwrap()]);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn simple_var_binding_1() {
        let mut style = Registry::new();
        let var1      = style.var(&["size"]);
        assert!(style.value(var1).is_none());
        style.set_value(&["size"],data(1.0));
        assert_eq!(style.value(var1),Some(&data(1.0)));
    }

    #[test]
    pub fn simple_var_binding_2() {
        let mut style = Registry::new();
        style.set_value(&["size"],data(1.0));
        let var1 = style.var(&["size"]);
        assert_eq!(style.value(var1),Some(&data(1.0)));
    }

    #[test]
    pub fn hierarchical_var_binding() {
        let mut style = Registry::new();
        let var1      = style.var("graph.button.size");
        assert!(style.value(var1).is_none());
        style.set_value("size",data(1.0));
        assert_eq!(style.value(var1),Some(&data(1.0)));
        style.set_value("button.size",data(2.0));
        assert_eq!(style.value(var1),Some(&data(2.0)));
        style.set_value("graph.button.size",data(3.0));
        assert_eq!(style.value(var1),Some(&data(3.0)));
        style.remove_value("graph.button.size");
        assert_eq!(style.value(var1),Some(&data(2.0)));
        style.remove_value("button.size");
        assert_eq!(style.value(var1),Some(&data(1.0)));
        style.remove_value("size");
        assert_eq!(style.value(var1),None);
    }

    #[test]
    pub fn expr_bindings_1() {
        let mut style = Registry::new();

        let var_size              = style.var("size");
        let var_button_size       = style.var("button.size");
        let var_graph_button_size = style.var("graph.button.size");

        assert!(style.value(var_graph_button_size).is_none());
        style.set_value("size",data(1.0));
        assert_eq!(style.value(var_graph_button_size),Some(&data(1.0)));
        style.set_expression("graph.button.size",&[var_button_size],|args| args[0] + &data(10.0));
        assert_eq!(style.value(var_graph_button_size),Some(&data(11.0)));
        style.set_expression("button.size",&[var_size],|args| args[0] + &data(100.0));
        assert_eq!(style.value(var_graph_button_size),Some(&data(111.0)));
        style.set_value("size",data(2.0));
        assert_eq!(style.value(var_graph_button_size),Some(&data(112.0)));
        style.set_value("button.size",data(3.0));
        assert_eq!(style.value(var_graph_button_size),Some(&data(13.0)));
        style.set_value("button.size",data(4.0));
        assert_eq!(style.value(var_graph_button_size),Some(&data(14.0)));
    }

    #[test]
    pub fn expr_circular() {
        let mut style = Registry::new();

        let var_a = style.var("a");
        let var_b = style.var("b");

        style.set_expression("a",&[var_b],|args| args[0].clone());
        style.set_expression("b",&[var_a],|args| args[0].clone());
        assert!(style.value(var_a).is_none());
    }
}
