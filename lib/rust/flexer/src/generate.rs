//! This file contains utilities for generating rust code from lexer definitions, allowing the
//! flexer to be specialised for a specific language.

use crate::prelude::*;
use quote::*;
use syn::*;

use crate::automata::dfa::DFA;
use crate::automata::dfa::RuleExecutable;
use crate::automata::state::Identifier;
use crate::automata::state::State;
use crate::group::Group;
use crate::group;

use enso_macro_utils::repr;
use proc_macro2::Literal;
use std::hash::BuildHasher;
use std::result::Result;
use std::fmt;

use crate as flexer;



// =======================
// === Code Generation ===
// =======================

/// Generate specialized code for the provided lexer `definition`.
///
/// This specialized code is a highly-optimised and tailored lexer that dispatches based on simple
/// code-point switches, with no dynamic lookup. This means that it is very fast, and very low
/// overhead.
pub fn specialize
( definition       : &impl flexer::State
, state_type_name  : impl Str
, output_type_name : impl Str
) -> Result<String,GenError> {
    let group_registry = definition.groups();
    let mut body_items = Vec::new();
    body_items.push(run_function(output_type_name)?);
    body_items.push(run_current_state_function());
    body_items.push(step(group_registry));
    for group in group_registry.all().iter() {
        body_items.extend(automaton_for_group(group,group_registry)?)
    }
    let result = wrap_in_impl_for(state_type_name,body_items)?;
    let code   = show_code(&result);
    Ok(code)
}


// === Whole-Lexer Codegen Utilities ===

/// Wrap the provided implementation items into an `impl` block for the provided `state_name` type.
pub fn wrap_in_impl_for
( state_name : impl Into<String>
, body       : Vec<ImplItem>
) -> Result<ItemImpl,GenError> {
    let state_name:Ident  = str_to_ident(state_name.into().as_str())?;
    let mut tree:ItemImpl = parse_quote! {
        #[allow(missing_docs,dead_code,clippy::all)]
        impl #state_name {}
    };
    tree.items.extend(body);
    Ok(tree)
}

/// Generate the `run` function for the specialized lexer.
///
/// This function is what the user of the lexer will call to begin execution.
pub fn run_function(output_type_name:impl Str) -> Result<ImplItem,GenError> {
    let output_type_name = str_to_path(output_type_name)?;
    let tree:ImplItem    = parse_quote! {
        pub fn run<R:LazyReader>(&mut self, mut reader:R) -> LexingResult<#output_type_name> {
            self.set_up();
            reader.advance_char(&mut self.bookmarks);
            while self.run_current_state(&mut reader) == StageStatus::ExitSuccess {}
            let result = match self.status {
                StageStatus::ExitFinished => LexingResult::success(
                    mem::take(&mut self.output)
                ),
                StageStatus::ExitFail => LexingResult::failure(
                    mem::take(&mut self.output)
                ),
                _ => LexingResult::partial(mem::take(&mut self.output))
            };
            self.tear_down();
            result
        }
    };
    Ok(tree)
}

/// Generate the function responsible for executing the lexer in its current state.
pub fn run_current_state_function() -> ImplItem {
    let tree:ImplItem = parse_quote! {
        fn run_current_state<R:LazyReader>(&mut self, reader:&mut R) -> StageStatus {
            self.status = StageStatus::Initial;
            let mut finished = false;

            // Runs until reaching a state that no longer says to continue.
            while let Some(next_state) = self.status.continue_as() {
                self.logger.debug(||format!("Current character is {:?}.",reader.character().char));
                self.logger.debug(||format!("Continuing in {:?}.",next_state));
                self.status = self.step(next_state,reader);

                if finished && reader.finished(self.bookmarks()) {
                    self.logger.info("Input finished.");
                    self.status = StageStatus::ExitFinished
                }
                finished = reader.character().is_eof();

                if self.status.should_continue() {
                    match reader.character().char {
                        Ok(char) => {
                            reader.append_result(char);
                            self.logger.info(||format!("Result is {:?}.",reader.result()));
                        },
                        Err(flexer::prelude::reader::Error::EOF) => {
                            self.logger.info("Reached EOF.");
                        },
                        Err(flexer::prelude::reader::Error::EndOfGroup) => {
                            let current_state = self.current_state();
                            let group_name    = self.groups().group(current_state).name.as_str();
                            let err           = format!("Missing rules for state {}.", group_name);
                            self.logger.error(err.as_str());
                            panic!(err)
                        }
                        Err(_) => {
                            self.logger.error("Unexpected error!");
                            panic!("Unexpected error!")
                        }
                    }
                    reader.advance_char(&mut self.bookmarks);
                }
            }

            self.status
        }
    };
    tree
}

/// Generate the `step` function for the lexer.
///
/// This function is responsible for dispatching based on the current state, consuming a character,
/// and returning the state to transition to.
pub fn step(groups:&group::Registry) -> ImplItem {
    let arms = groups.all().iter().map(|g| step_match_arm(g.id.into())).collect_vec();
    parse_quote! {
        fn step<R:LazyReader>(&mut self, next_state:SubStateId, reader:&mut R) -> StageStatus {
            let current_state:usize = self.current_state().into();
            match current_state {
                #(#arms)*
                _ => unreachable_panic!("Unreachable state reached in lexer."),
            }
        }
    }
}

/// Generate a match arm for the step function.
///
/// There is one match arm per lexer state.
pub fn step_match_arm(number:usize) -> Arm {
    let literal           = Literal::usize_unsuffixed(number);
    let function_name_str = format!("dispatch_in_state_{}",number);
    let func_name:Ident   = parse_str(function_name_str.as_str()).unwrap();
    let arm:Arm = parse_quote! {
        #literal => self.#func_name(next_state,reader),
    };
    arm
}


// === Generation for a Specific Lexer State ===

/// Generate the functions that implement the lexer automaton for a given lexer state.
pub fn automaton_for_group
( group    : &Group
, registry : &group::Registry
) -> Result<Vec<ImplItem>,GenError> {
    let nfa       = registry.to_nfa_from(group.id);
    let mut rules = Vec::with_capacity(nfa.states.len());
    for state in nfa.states.iter() {
        if state.name.is_some() {
            rules.push(rule_for_state(state)?);
        }
    }
    let mut dfa             = DFA::from(&nfa);
    let dispatch_for_dfa    = dispatch_in_state(&dfa,group.id.into())?;
    let mut dfa_transitions = transitions_for_dfa(&mut dfa,group.id.into())?;
    dfa_transitions.push(dispatch_for_dfa);
    dfa_transitions.extend(rules);
    Ok(dfa_transitions)
}

/// Generate a set of transition functions for the provided `dfa`, with identifier `id`.
pub fn transitions_for_dfa(dfa:&mut DFA, id:usize) -> Result<Vec<ImplItem>,GenError> {
    let mut state_has_overlapping_rules:HashMap<usize,bool> = HashMap::new();
    state_has_overlapping_rules.insert(0,false);
    let state_names:Vec<_> = dfa.links.row_indices().map(|ix| (ix, name_for_step(id, ix))).collect();
    let mut transitions    = Vec::with_capacity(state_names.len());
    for (ix,name) in state_names.into_iter() {
        transitions.push(transition_for_dfa(dfa,name,ix,&mut state_has_overlapping_rules)?)
    }
    Ok(transitions)
}

/// Generate a specific transition function for
#[allow(clippy::implicit_hasher)]
pub fn transition_for_dfa<S:BuildHasher>
( dfa             : &mut DFA
, transition_name : Ident
, state_ix        : usize
, has_overlaps    : &mut HashMap<usize,bool,S>
) -> Result<ImplItem,GenError> {
    let match_expr:Expr   = match_for_transition(dfa,state_ix,has_overlaps)?;
    let function:ImplItem = parse_quote! {
        fn #transition_name<R:LazyReader>(&mut self, reader:&mut R) -> StageStatus {
            #match_expr
        }
    };
    Ok(function)
}

/// Generate the pattern match for a given transition function.
pub fn match_for_transition<S:BuildHasher>
( dfa          : &mut DFA
, state_ix     : usize
, has_overlaps : &mut HashMap<usize,bool,S>
) -> Result<Expr,GenError> {
    let overlaps          = *has_overlaps.get(&state_ix).unwrap_or(&false);
    let state             = dfa.callbacks.get(state_ix).expect("Internal error.").clone();
    let mut trigger_state = dfa.links[(state_ix,0)];
    let mut range_start   = u32::min_value();
    let divisions:Vec<_>  = dfa.alphabet_segmentation.divisions_as_vec();
    let mut branches      = Vec::with_capacity(divisions.len());
    for division in divisions.into_iter() {
        let ix                = division.position;
        let sym               = division.symbol;
        let new_trigger_state = dfa.links[(state_ix,ix)];
        if new_trigger_state != trigger_state {
            let range_end             = if sym.value != 0 { sym.value - 1 } else { sym.value };
            let current_trigger_state = trigger_state;
            let current_range_start   = range_start;
            trigger_state             = new_trigger_state;
            range_start               = sym.value;
            let body =
                branch_body(dfa,current_trigger_state,&state,has_overlaps,overlaps)?;
            branches.push(Branch::new(Some(current_range_start..=range_end),body))
        } else {}
    }
    let catch_all_branch_body = branch_body(dfa,trigger_state,&state,has_overlaps,overlaps)?;
    let catch_all_branch      = Branch::new(None,catch_all_branch_body);
    branches.push(catch_all_branch);
    let arms:Vec<Arm> = branches.into_iter().map(Into::into).collect();
    let mut match_expr:ExprMatch = parse_quote! {
        match u32::from(reader.character()) {
            #(#arms)*
        }
    };
    match_expr.arms = arms;
    Ok(Expr::Match(match_expr))
}

/// Generate the branch body for a transition in the DFA.
pub fn branch_body<S:BuildHasher>
( dfa           : &mut DFA
, target_state  : Identifier
, maybe_state   : &Option<RuleExecutable>
, has_overlaps  : &mut HashMap<usize,bool,S>
, rules_overlap : bool
) -> Result<Block,GenError> {
    if target_state == Identifier::INVALID {
        match maybe_state {
            None => {
                Ok(parse_quote! {{
                    StageStatus::ExitFail
                }})
            },
            Some(rule_exec) => {
                let rule:Expr = match parse_str(rule_exec.code.as_str()) {
                    Ok(rule) => rule,
                    Err(_)   => return Err(GenError::BadExpression(rule_exec.code.clone()))
                };
                if rules_overlap {
                    Ok(parse_quote! {{
                        let rule_bookmark    = self.bookmarks.rule_bookmark;
                        let matched_bookmark = self.bookmarks.matched_bookmark;
                        self.bookmarks.rewind(rule_bookmark,reader);
                        self.current_match = reader.pop_result();
                        self.#rule(reader);
                        self.bookmarks.bookmark(matched_bookmark,reader);
                        StageStatus::ExitSuccess
                    }})
                } else {
                    Ok(parse_quote! {{
                        let matched_bookmark = self.bookmarks.matched_bookmark;
                        self.current_match   = reader.pop_result();
                        self.#rule(reader);
                        self.bookmarks.bookmark(matched_bookmark,reader);
                        StageStatus::ExitSuccess
                    }})
                }
            }
        }
    } else {
        let target_state_has_no_rule = match maybe_state {
            Some(state) => if !dfa.has_rule_for(target_state) {
                dfa.callbacks[target_state.id] = Some(state.clone());
                has_overlaps.insert(target_state.id,true);
                true
            } else {
                false
            },
            None => false
        };

        let state_id = Literal::usize_unsuffixed(target_state.id);
        let ret:Expr = parse_quote! {
            StageStatus::ContinueWith(#state_id.into())
        };

        if target_state_has_no_rule && !rules_overlap {
            Ok(parse_quote! {{
                let rule_bookmark = self.bookmarks.rule_bookmark;
                self.bookmarks.bookmark(rule_bookmark,reader);
                #ret
            }})
        } else {
            Ok(parse_quote! {{
                #ret
            }})
        }
    }
}

/// Generate the dispatch function for a given lexer state.
///
/// This dispatch function is responsible for dispatching based on the sub-state of any given lexer
/// state, and is the main part of implementing the actual lexer transitions.
pub fn dispatch_in_state(dfa:&DFA, id:usize) -> Result<ImplItem,GenError> {
    let dispatch_name:Ident = str_to_ident(format!("dispatch_in_state_{}",id))?;
    let state_names  = dfa.links.row_indices().map(|ix| (ix, name_for_step(id,ix))).collect_vec();
    let mut branches = Vec::with_capacity(state_names.len());
    for (ix,name) in state_names.into_iter() {
        let literal = Literal::usize_unsuffixed(ix);
        let arm:Arm = parse_quote! {
            #literal => self.#name(reader),
        };
        branches.push(arm);
    }

    let pattern_match:ExprMatch = parse_quote! {
        match new_state_index.into() {
            #(#branches)*
            _ => unreachable_panic!("Unreachable state reached in lexer.")
        }
    };
    let func:ImplItem = parse_quote! {
        fn #dispatch_name<R:LazyReader>
        ( &mut self
        , new_state_index:SubStateId
        , reader:&mut R
        ) -> StageStatus {
            #pattern_match
        }
    };

    Ok(func)
}

/// Generate a name for a given step function.
pub fn name_for_step(in_state:usize, to_state:usize) -> Ident {
    let name_str = format!("state_{}_to_{}",in_state,to_state);
    parse_str(name_str.as_str()).expect("Impossible to not be a valid identifier.")
}

/// Generate an executable rule function for a given lexer state.
pub fn rule_for_state(state:&State) -> Result<ImplItem,GenError> {
    match &state.name {
        None => unreachable_panic!("Rule for state requested, but state has none."),
        Some(name) => {
            let rule_name = str_to_ident(name)?;
            let code:Expr = match parse_str(state.callback.as_str()) {
                Ok(expr) => expr,
                Err(_)   => return Err(GenError::BadExpression(state.callback.clone()))
            };
            if !has_reader_arg(&code) {
                return Err(GenError::BadCallbackArgument)
            }

            let tree:ImplItem = parse_quote! {
                fn #rule_name<R:LazyReader>(&mut self, reader:&mut R) {
                    #code
                }
            };
            Ok(tree)
        }
    }
}

/// Checks if the given `expr` is a  call with a single argument "reader" being passed.
#[allow(clippy::cmp_owned)]
pub fn has_reader_arg(expr:&Expr) -> bool {
    match expr {
        Expr::MethodCall(expr) => match expr.args.first() {
            Some(Expr::Path(path)) => {
                match path.path.segments.first() {
                    Some(segment) => {
                        segment.ident.to_string() == "reader"
                    }
                    _ => false
                }
            }
            _ => false
        },
        Expr::Call(expr) => match expr.args.first() {
            Some(Expr::Path(path)) => {
                match path.path.segments.first() {
                    Some(segment) => {
                        segment.ident.to_string() == "reader"
                    }
                    _ => false
                }
            }
            _ => false
        }
        _ => false
    }
}



// ================
// === GenError ===
// ================

/// Errors that arise during code generation.
#[derive(Clone,Debug,PartialEq)]
pub enum GenError {
    /// The callback function does not take a single argument `reader`.
    BadCallbackArgument,
    /// The provided string is not a valid rust identifier.
    BadIdentifier(String),
    /// The provided expression isn't a valid rust expression.
    BadExpression(String),
    /// The provided string is not a valid rust literal.
    BadLiteral(String),
    /// The provided string is not a valid rust path.
    BadPath(String),
}


// === Trait Impls ===

impl Display for GenError {
    fn fmt(&self, f:&mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            GenError::BadCallbackArgument => write!(f,
                "Bad argument to a callback function. It must take a single argument `reader`."
            ),
            GenError::BadIdentifier(str) => write!(f,"`{}` is not a valid rust identifier.",str),
            GenError::BadExpression(str) => write!(f,"`{}` is not a valid rust expression.",str),
            GenError::BadLiteral(str)    => write!(f,"`{}` is not a valid rust literal.",str),
            GenError::BadPath(str)       => write!(f,"`{}` is not a valid rust path.",str),
        }
    }
}



// ==============
// === Branch ===
// ==============

/// A representation of a dispatch branch for helping to generate pattern arms.
#[allow(missing_docs)]
#[derive(Clone,Debug,PartialEq)]
struct Branch {
    pub range:Option<RangeInclusive<u32>>,
    pub body:Block
}

impl Branch {
    /// Create a new branch, from the provided `range` and with `body` as the code it executes.
    pub fn new(range:Option<RangeInclusive<u32>>, body:Block) -> Branch {
        Branch {range,body}
    }
}


// === Trait Impls ===

impl Into<Arm> for Branch {
    fn into(self) -> Arm {
        let body = self.body;
        match self.range {
            Some(range) => {
                let range_start = Literal::u32_unsuffixed(*range.start());
                let range_end   = Literal::u32_unsuffixed(*range.end());
                if range.start() == range.end() {
                    parse_quote! {
                        #range_start => #body,
                    }
                } else {
                    parse_quote! {
                        #range_start..=#range_end => #body,
                    }
                }
            }
            None => parse_quote! {
                _ => #body,
            }
        }
    }
}



// =================
// === Utilities ===
// =================

/// Convert a string to an identifier.
pub fn str_to_ident(str:impl Str) -> Result<Ident,GenError> {
    parse_str(str.as_ref()).map_err(|_| GenError::BadIdentifier(str.into()))
}

/// Convert a string to a path.
pub fn str_to_path(str:impl Str) -> Result<Path,GenError> {
    parse_str(str.as_ref()).map_err(|_| GenError::BadPath(str.into()))
}

/// Convert the syntax tree into a string.
pub fn show_code(tokens:&impl ToTokens) -> String {
    repr(tokens)
}


