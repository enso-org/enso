//! A module with utilities for generating SpanTree from macros (Match and Ambiguous).

// TODO[ao] Duplicated with `pattern_subcrumbs` function in `ast::crumbs`, but adds information
// about spacing. All the 'crumblike' utilities should be merged to one solution

use crate::prelude::*;

use ast::crumbs::PatternMatchCrumb;
use ast::Ast;
use ast::MacroPatternMatch;
use ast::Shifted;



// ======================
// === LocatedPattern ===
// ======================

/// A fragment of MacroPatternMatch localized by PatternMatchCrumbs.
#[allow(missing_docs)]
#[derive(Debug)]
pub struct LocatedPattern<'a> {
    pub pattern: &'a MacroPatternMatch<Shifted<Ast>>,
    pub crumbs:  Vec<PatternMatchCrumb>,
}



// ==================
// === PatternDfs ===
// ==================

/// A iterator over all nodes in MacroPatternMatch tree, traversing it with DFS algorithm.
struct PatternDfs<'a> {
    /// The FILO queue of nodes to visit.
    to_visit: Vec<LocatedPattern<'a>>,
}

impl<'a> Iterator for PatternDfs<'a> {
    type Item = LocatedPattern<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let to_return = self.to_visit.pop();
        if let Some(pattern) = &to_return {
            self.push_children_to_visit(pattern);
        }
        to_return
    }
}

impl<'a> PatternDfs<'a> {
    /// Create iterator which start from `root` node.
    pub fn new(root: &'a MacroPatternMatch<Shifted<Ast>>) -> Self {
        let first_to_visit = LocatedPattern { pattern: root, crumbs: vec![] };
        PatternDfs { to_visit: vec![first_to_visit] }
    }

    /// Obtain all children of `pattern` and push them to `to_visit` queue.
    fn push_children_to_visit(&mut self, pattern: &LocatedPattern<'a>) {
        use ast::MacroPatternMatchRaw::*;
        match pattern.pattern.deref() {
            Except(pat) => self.push_child_to_visit(pattern, &pat.elem, PatternMatchCrumb::Except),
            Tag(pat) => self.push_child_to_visit(pattern, &pat.elem, PatternMatchCrumb::Tag),
            Cls(pat) => self.push_child_to_visit(pattern, &pat.elem, PatternMatchCrumb::Cls),
            Or(pat) => self.push_child_to_visit(pattern, &pat.elem, PatternMatchCrumb::Or),
            Seq(pat) => {
                let (left_elem, right_elem) = &pat.elem;
                self.push_child_to_visit(pattern, right_elem, PatternMatchCrumb::Seq {
                    right: true,
                });
                self.push_child_to_visit(pattern, left_elem, PatternMatchCrumb::Seq {
                    right: false,
                });
            }
            Many(pat) =>
                for (index, elem) in pat.elem.iter().enumerate().rev() {
                    self.push_child_to_visit(pattern, elem, PatternMatchCrumb::Many { index });
                },
            // Other patterns does not have children.
            _ => {}
        }
    }

    fn push_child_to_visit(
        &mut self,
        pattern: &LocatedPattern<'a>,
        child: &'a MacroPatternMatch<Shifted<Ast>>,
        crumb: PatternMatchCrumb,
    ) {
        let loc_pattern = LocatedPattern {
            pattern: child,
            crumbs:  pattern.crumbs.iter().cloned().chain(std::iter::once(crumb)).collect(),
        };
        self.to_visit.push(loc_pattern);
    }
}


// ==========================================
// === Retrieving AST Nodes From Patterns ===
// ==========================================

/// An AST node being inside a Match node
#[allow(missing_docs)]
#[derive(Debug)]
pub struct AstInPattern {
    pub ast:    Shifted<Ast>,
    pub crumbs: Vec<PatternMatchCrumb>,
}

/// Helper function that returns all AST nodes being on leaves of MacroPatternMatch.
pub fn all_ast_nodes_in_pattern(
    pattern: &MacroPatternMatch<Shifted<Ast>>,
) -> impl Iterator<Item = AstInPattern> + '_ {
    use ast::MacroPatternMatchRaw::*;

    PatternDfs::new(pattern).filter_map(|pattern| {
        let opt_ast_and_crumb = match pattern.pattern.deref() {
            Build(pat) => Some((&pat.elem, PatternMatchCrumb::Build)),
            Err(pat) => Some((&pat.elem, PatternMatchCrumb::Err)),
            Tok(pat) => Some((&pat.elem, PatternMatchCrumb::Tok)),
            Blank(pat) => Some((&pat.elem, PatternMatchCrumb::Blank)),
            Var(pat) => Some((&pat.elem, PatternMatchCrumb::Var)),
            Cons(pat) => Some((&pat.elem, PatternMatchCrumb::Cons)),
            Opr(pat) => Some((&pat.elem, PatternMatchCrumb::Opr)),
            Mod(pat) => Some((&pat.elem, PatternMatchCrumb::Mod)),
            Num(pat) => Some((&pat.elem, PatternMatchCrumb::Num)),
            Text(pat) => Some((&pat.elem, PatternMatchCrumb::Text)),
            Block(pat) => Some((&pat.elem, PatternMatchCrumb::Block)),
            Macro(pat) => Some((&pat.elem, PatternMatchCrumb::Macro)),
            Invalid(pat) => Some((&pat.elem, PatternMatchCrumb::Invalid)),
            _ => None,
        };
        opt_ast_and_crumb.map(|(ast, crumb)| AstInPattern {
            ast:    ast.clone(),
            crumbs: pattern.crumbs.into_iter().chain(std::iter::once(crumb)).collect(),
        })
    })
}
