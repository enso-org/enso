//! Serialization analysis on meta representations.
//!
//! # Test Case Generation
//!
//! The [`testcases`] function supports generation of deserialization test cases that cover all
//! types reachable from some root type in a [`TypeGraph`].
//!
//! The implementation is based on computing a test program built from a small set of operation,
//! and then interpreting the program to generate all the needed test cases.
//!
//! ## Test programs
//!
//! Abstractly, a test program can be considered to be equivalent to a tree, where each node has
//! three possibilities (the implementation is equivalent, but more efficient to execute):
//! - `Constant`: Evaluates to some constant data. The value affects the output but is irrelevant to
//!   control flow. (In the implementation, this is [`Op::U8(_)`], [`Op::U32(_)`], etc.)
//! - `Concat(A, B)`: Evaluates to the concatenation of the evaluation of its two child nodes. (In
//!   the implementation, this operator is implicit in program order.)
//! - `Amb(A, B)`: In every evaluation, this must evaluate to either the value of `A` or the value
//!   of `B`. For completeness, there must be at least one evaluation of the whole program in which
//!   this is evaluated to `A`, and at least one evaluation where it is evaluated to `B`. (In the
//!   implementation, this is an n-ary operator expressed with [`Op::SwitchPush`] /
//!   [`Op::SwitchPop`] / [`Op::Case(_)`].)
//!
//! ## Program generation
//!
//! The input typegraph may contain cycles. The first step of program generation is to select a
//! *basecase* for every sum type such that the type graph, when excluding non-basecase
//! possibilities from every sum type, does not contain any cycles. For details on this problem and
//! the algorithm solving it, see [`select_basecase`].
//!
//! Once we have the information necessary to avoid trying to emit cyclic structures, program
//! generation is straightforward: For product types, we use the equivalent of the `Concat`
//! operation described above; for sum types, the `Amb` operation. Compound primitives like `Option`
//! and `Result` are treated as similar user-defined sum types would be.
//!
//! ## Program interpretation
//!
//! Program interpretation is better described in terms of the sequence of [`Op`]s than the more
//! abstract tree representation described above. The interpreter advances a program counter over
//! every [`Op`] once, in sequence. It maintains a stack of the [`Op::SwitchPop`] corresponding to
//! every [`Op::SwitchPush`]--that is, the join points for the n-ary `Amb` operators that the
//! program counter is currently within. For each [`Op::Case`] (i.e. one possibility of an `Amb`), a
//! test case is generated: The test case will consist of the basecase-mode evaluation of the whole
//! program up to the active [`Op::Case`] in each open switch (this value is maintained as execution
//! proceeds), the present case in each open switch, and then the output of basecase-mode execution
//! from the join point of the switch on top of stack to the end of the program--thus efficiently
//! producing one test case for every [`Op::Case`] in the input, with each case composed of the
//! output of the whole program, using basecase values for all switches not in the stack at the
//! point the case is reached.

use crate::meta::*;

use std::fmt::Write;



const DEBUG: bool = false;



// ============================
// === Test Case Generation ===
// ============================

/// A set of *accept* and *reject* tests for a serialization format.
#[derive(Debug, Clone)]
pub struct TestCases {
    /// Inputs that a deserializer should accept.
    pub accept: Vec<Vec<u8>>,
    /// Inputs that a deserializer should reject.
    pub reject: Vec<Vec<u8>>,
    program:    Vec<Op>,
    debuginfo:  BTreeMap<usize, String>,
}

/// Generate test cases.
///
/// Produces 100% coverage of valid structures (i.e. every variant of every enum occurs in some
/// `accept` case), and a representative set of `reject` cases.
pub fn testcases(graph: &TypeGraph, root: TypeId) -> TestCases {
    let mut builder = ProgramBuilder::new(graph, root);
    builder.type_(root, Default::default());
    let ProgramBuilder { program, debuginfo, .. } = builder;
    if DEBUG {
        eprintln!("{}", fmt_program(&program, &debuginfo));
    }
    let (accept, reject) = Interpreter::run(&program);
    TestCases { accept, reject, program, debuginfo }
}

impl TestCases {
    /// Produce a JSON representation of test case data.
    pub fn to_json(&self) -> String {
        let accept: Vec<_> = self.accept.iter().map(|case| format!("{:?}", case)).collect();
        let accept = accept.join(", \n\t");
        let reject: Vec<_> = self.reject.iter().map(|case| format!("{:?}", case)).collect();
        let reject = reject.join(", \n\t");
        let mut out = String::new();
        writeln!(out, "{{").unwrap();
        writeln!(out, "\"accept\": [").unwrap();
        writeln!(out, "\t{accept}").unwrap();
        writeln!(out, "],").unwrap();
        writeln!(out, "\"reject\": [").unwrap();
        writeln!(out, "\t{reject}").unwrap();
        writeln!(out, "]}}").unwrap();
        out
    }

    /// Render a debug representation of the test program used to generate the cases.
    pub fn program(&self) -> String {
        fmt_program(&self.program, &self.debuginfo)
    }
}

/// Produce a debug representation of a program.
fn fmt_program(program: &[Op], debuginfo: &BTreeMap<usize, String>) -> String {
    let mut out = String::new();
    let mut indent = 0;
    let continuations = collect_continuations(program);
    let mut accept = 0;
    let mut reject = 0;
    for (i, op) in program.iter().enumerate() {
        if *op == Op::SwitchPop {
            indent -= 1
        }
        write!(out, "{:>4}: ", i).unwrap();
        for _ in 0..indent {
            write!(out, "  ").unwrap();
        }
        write!(out, "{:?}", op).unwrap();
        if let Some(debuginfo) = debuginfo.get(&i) {
            write!(out, " -- {}", debuginfo).unwrap();
        }
        if let Some(continuation) = continuations.get(&i) {
            write!(out, " [{}]", continuation).unwrap();
        }
        if *op == Op::Case(Case::Accept) {
            write!(out, " # accept{accept}").unwrap();
            accept += 1;
        }
        if *op == Op::Case(Case::Reject) {
            write!(out, " # reject{reject}").unwrap();
            reject += 1;
        }
        if *op == Op::SwitchPush {
            indent += 1
        }
        writeln!(out).unwrap();
    }
    out
}



// ==========================
// === Program Operations ===
// ==========================

/// Operations for a test-case-generating program.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Op {
    U8(u8),
    U32(u32),
    U64(u64),
    SwitchPush,
    SwitchPop,
    Case(Case),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Case {
    Accept,
    Reject,
}



// ==========================
// === Program Generation ===
// ==========================

/// Generates test-case-generating program for a type graph.
#[derive(Debug)]
struct ProgramBuilder<'g> {
    graph:                 &'g TypeGraph,
    will_visit:            BTreeSet<TypeId>,
    visited:               BTreeSet<TypeId>,
    debuginfo:             BTreeMap<usize, String>,
    program:               Vec<Op>,
    basecase_discriminant: BTreeMap<TypeId, usize>,
}

impl<'g> ProgramBuilder<'g> {
    fn new(graph: &'g TypeGraph, root: TypeId) -> Self {
        let mut graph_ = graph.clone();
        graph_.gc(vec![root]);
        let program = Default::default();
        let visited = Default::default();
        let mut will_visit = BTreeSet::new();
        let mut basecase_discriminant = BTreeMap::new();
        let mut sb_visited = BTreeSet::new();
        for (id, ty) in graph_.types.iter() {
            if let Data::Struct(fields) = &ty.data {
                will_visit.extend(fields.iter().map(|field| field.type_));
                will_visit.extend(ty.discriminants.values());
            }
            select_basecase(graph, id, &mut basecase_discriminant, &mut sb_visited);
            sb_visited.clear();
        }
        let debuginfo = Default::default();
        Self { graph, program, visited, will_visit, debuginfo, basecase_discriminant }
    }

    fn emit(&mut self, op: Op) {
        self.program.push(op);
    }

    fn debug_next(&mut self, debug: impl std::fmt::Display) {
        let n = self.program.len();
        self.debuginfo.insert(n, debug.to_string());
    }

    fn debug_prev(&mut self, debug: impl std::fmt::Display) {
        let n = self.program.len() - 1;
        self.debuginfo.insert(n, debug.to_string());
    }

    fn basecase(&self, id: TypeId) -> bool {
        self.visited.contains(&id) || self.will_visit.contains(&id)
    }

    fn type_(&mut self, id: TypeId, basecase: bool) {
        let basecase = basecase || !self.visited.insert(id);
        let ty = &self.graph[id];
        match &ty.data {
            Data::Struct(_) => self.object(id, basecase),
            Data::Primitive(primitive) => self.primitive(*primitive, basecase, id),
        }
    }

    /// Emit [`Op`]s reflecting the data of a [`Primitive`].
    ///
    /// # Simple primitives
    ///
    /// If the [`Primitive`] is scalar data, like an integer or bool, operations producing an
    /// arbitrary example value will be emitted.
    ///
    /// # Compound primitives
    ///
    /// For all compound primitives (primitives referring to other types), the `basecase` parameter
    /// deterimines whether the output is minimal (as appropriate for previously-encountered types),
    /// or exhaustive.
    ///
    /// If the input is an option:
    /// - If `basecase` is `true`, only the `None` representation will be emitted.
    /// - If `basecase` is `false`, an alternation of the `None` representation, the `Some`
    ///   representation, and a reject-case with an invalid discriminant will be emitted.
    ///
    /// If the input is a sequence:
    /// - If `basecase` is `true`, a zero-length sequence will be emitted.
    /// - If `basecase` is `false`, an alternation of an empty sequence and a 1-object sequence will
    ///   be emitted (this tests the correspondence between the encoded length and number of
    ///   elements). Although an `Option` also allows 0 or 1 objects, an `Option` is encoded with a
    ///   smaller (1-byte) length field, so they are encoded distinctly.
    ///
    /// If the input is a result:
    /// - If `basecase` is `true`, an type that has been determined not to cause recursion will be
    ///   selected.
    /// - If `basecase` is `false`, an alternation of the two possible types will be emitted, along
    ///   with a reject case with an invalid discriminant.
    fn primitive(&mut self, primitive: Primitive, basecase: bool, id: TypeId) {
        match primitive {
            // Value doesn't matter, but this will be recognizable in the output, and will tend not
            // to encode compatibly with other types.
            Primitive::U32 => self.emit(Op::U32(1234567890)),
            // Value 1 chosen to detect errors better: 0 encodes the same way as Option::None.
            Primitive::Bool => self.emit(Op::U8(1)),
            // Value doesn't matter, but this will be recognizable in the output, and will tend not
            // to encode compatibly with other types.
            Primitive::U64 => self.emit(Op::U64(1234567890123456789)),
            Primitive::String => self.emit(Op::U64("".len() as u64)),
            Primitive::Sequence(_) if basecase => self.emit(Op::U64(0)),
            Primitive::Sequence(t0) => {
                self.emit(Op::SwitchPush);
                self.emit(Op::U64(0));
                self.emit(Op::Case(Case::Accept));
                self.emit(Op::U64(1));
                self.type_(t0, basecase);
                self.emit(Op::Case(Case::Accept));
                self.emit(Op::SwitchPop);
            }
            Primitive::Option(_) if basecase => self.emit(Op::U8(0)),
            Primitive::Option(t0) => {
                self.emit(Op::SwitchPush);
                if self.basecase(t0) {
                    self.emit(Op::U8(0));
                } else {
                    self.emit(Op::U8(1));
                    self.type_(t0, basecase);
                }
                self.emit(Op::Case(Case::Accept));
                self.emit(Op::U8(2));
                self.emit(Op::Case(Case::Reject));
                self.emit(Op::SwitchPop);
            }
            Primitive::Result(t0, t1) => {
                let basecase_index = self.basecase_discriminant[&id];
                let types = [t0, t1];
                let t0 = types[basecase_index];
                let t1 = types[1 - basecase_index];
                let i0 = basecase_index as u32;
                let i1 = 1 - basecase_index as u32;
                if basecase {
                    self.emit(Op::U32(i0));
                    self.type_(t0, basecase);
                } else {
                    self.emit(Op::SwitchPush);
                    if !self.basecase(t0) || self.basecase(t1) {
                        self.emit(Op::U32(i0));
                        self.type_(t0, basecase);
                        self.emit(Op::Case(Case::Accept));
                    }
                    if !self.basecase(t1) {
                        self.emit(Op::U32(i1));
                        self.type_(t1, basecase);
                        self.emit(Op::Case(Case::Accept));
                    }
                    self.emit(Op::U32(2));
                    self.emit(Op::Case(Case::Reject));
                    self.emit(Op::SwitchPop);
                }
            }
        }
    }

    /// Emit [`Op`]s reflecting the data of a [`Type`], as identified by ID.
    ///
    /// If `basecase` is true: An example of the specified type will be created that is intended to
    /// be no larger than necessary, and that avoids infinite recursion; this is appropriate when
    /// emitting data for a type that has already been exercised with `basecase=false`, or for a
    /// type that has been determined to occur unconditionally as a field of another type.
    ///
    /// If `basecase` is false, if the type has child types, an alternation of all possible child
    /// types will be emitted, along with a reject-case including a discriminant higher than the
    /// highest valid discriminant, and reject-cases for any invalid discriminants lower than the
    /// highest valid discriminant.
    fn object(&mut self, id: TypeId, basecase: bool) {
        let mut hierarchy = vec![id];
        let mut id = id;
        while let Some(id_) = self.graph[id].parent {
            id = id_;
            hierarchy.push(id);
        }
        self.object_(&mut hierarchy, basecase);
        assert_eq!(&hierarchy, &[])
    }

    /// Emit [`Op`]s reflecting the data of a [`Type`], as identified by a `Vec` `hierarchy` in
    /// which:
    /// - `hierarchy[0]` is a concrete [`Type`].
    /// - `hierarchy[i]` is the parent of `hierarchy[i-1]`.
    /// - `hierarchy[hierarchy.len() - 1]` identifies a type that doesn't have any parent type.
    ///
    /// For a design description see the primary interface, [`Self::object`].
    fn object_(&mut self, hierarchy: &mut Vec<TypeId>, basecase: bool) {
        let id = hierarchy.pop().unwrap();
        let ty = &self.graph[id];
        let fields = match &ty.data {
            Data::Struct(fields) => fields,
            _ => panic!(),
        };
        for (i, field) in fields.iter().enumerate() {
            if ty.child_field == Some(i) {
                if hierarchy.is_empty() {
                    let basecase_discriminant = self.basecase_discriminant[&id];
                    let discriminants = &ty.discriminants;
                    let basecase_ty = discriminants[&basecase_discriminant];
                    hierarchy.push(basecase_ty);
                    if basecase {
                        self.emit(Op::U32(basecase_discriminant as u32));
                        self.object_(hierarchy, basecase);
                    } else {
                        let (&max, _) = discriminants.last_key_value().unwrap();
                        self.emit(Op::SwitchPush);
                        self.emit(Op::U32(basecase_discriminant as u32));
                        self.debug_prev(&self.graph[basecase_ty].name);
                        self.object_(hierarchy, basecase);
                        self.emit(Op::Case(Case::Accept));
                        for i in 0..=(max + 1) {
                            if i == basecase_discriminant {
                                continue;
                            }
                            self.emit(Op::U32(i as u32));
                            match discriminants.get(&i) {
                                Some(id) => {
                                    hierarchy.push(*id);
                                    self.debug_prev(&self.graph[*id].name);
                                    self.object_(hierarchy, basecase);
                                    self.emit(Op::Case(Case::Accept));
                                }
                                None => self.emit(Op::Case(Case::Reject)),
                            }
                        }
                        self.emit(Op::SwitchPop);
                    }
                } else {
                    self.object_(hierarchy, basecase);
                }
            }
            self.type_(field.type_, basecase);
            self.debug_prev(format!(".{}", &field.name));
        }
    }
}

/// Choose a discriminant for the specified type, and if necessary for some other types reachable
/// from it in the composition graph, so that the composition graph for the type is non-recursive.
///
/// If any child type doesn't have own any sum-types, we select it. Otherwise, selections are made
/// according to the following recursive algorithm:
/// - If we have a child that doesn't own any sum-type fields, choose it and return Ok.
/// - Otherwise, recurse into each child; if one returns Ok, choose it and return Ok.
/// - If no child returns Ok, we got here by recursing into a bad choice; return Err.
/// - If we reach a type we have already visited, this choice contains a cycle; return Err. (Because
///   we only visit each type once, the time complexity of this algorithm is linear in the number of
///   types we need to select discriminants for).
///
/// The top-level call will always return Ok because: There must be a sum type in our descendants
/// that has a child that doesn't own any sum-type fields, or there would be a type in the input
/// that is only possible to instantiate with cyclic or infinite data.
fn select_basecase(
    graph: &TypeGraph,
    id: TypeId,
    out: &mut BTreeMap<TypeId, usize>,
    visited: &mut BTreeSet<TypeId>,
) {
    select_basecase_(graph, id, out, visited).unwrap()
}

/// Implementation. See the documentation for [`select_basecase`].
fn select_basecase_(
    graph: &TypeGraph,
    id: TypeId,
    out: &mut BTreeMap<TypeId, usize>,
    visited: &mut BTreeSet<TypeId>,
) -> Result<(), ()> {
    if out.contains_key(&id) {
        return Ok(());
    }
    if !visited.insert(id) {
        return Err(());
    }
    let mut result_discriminants = BTreeMap::new();
    let discriminants = match &graph[id].data {
        Data::Primitive(Primitive::Result(t0, t1)) => {
            result_discriminants.insert(0, *t0);
            result_discriminants.insert(1, *t1);
            &result_discriminants
        }
        _ => &graph[id].discriminants,
    };
    if discriminants.is_empty() {
        return Ok(());
    }
    let mut descendants = BTreeMap::<_, Vec<_>>::new();
    let mut child_fields = BTreeSet::new();
    let mut child_sums = BTreeSet::new();
    for (&i, &child) in discriminants {
        child_fields.clear();
        child_sums.clear();
        child_fields.insert(child);
        while let Some(child_) = child_fields.pop_last() {
            let ty = &graph[child_];
            if ty.child_field.is_some() {
                child_sums.insert(child_);
            }
            match &ty.data {
                Data::Struct(fields) => child_fields.extend(fields.iter().map(|field| field.type_)),
                Data::Primitive(Primitive::Result(_, _)) => {
                    child_sums.insert(child_);
                }
                Data::Primitive(_) => (),
            }
        }
        if child_sums.is_empty() {
            out.insert(id, i);
            return Ok(());
        }
        descendants.insert(i, child_sums.iter().copied().collect());
    }
    for (i, descendants) in descendants {
        let is_ok = |id: &TypeId| select_basecase_(graph, *id, out, visited).is_ok();
        if descendants.iter().all(is_ok) {
            out.insert(id, i);
            return Ok(());
        }
    }
    Err(())
}



// =================
// === Execution ===
// =================

/// Runs a test-case-generating program.
#[derive(Debug, Default)]
struct Interpreter<'p> {
    program:       &'p [Op],
    continuations: BTreeMap<usize, usize>,
}

/// A control-stack frame of the interpreted program.
#[derive(Debug, Default, PartialEq, Eq)]
struct Frame {
    /// A return address, as an index into the sequence of [`Op`]s.
    return_:    usize,
    /// A height of the data stack.
    prefix_len: usize,
}

impl<'p> Interpreter<'p> {
    /// Interpret a program, producing collections of accept-cases and reject-cases.
    fn run(program: &'p [Op]) -> (Vec<Vec<u8>>, Vec<Vec<u8>>) {
        let continuations = collect_continuations(program);
        let self_ = Self { program, continuations };
        self_.run_()
    }

    /// Interpret every instruction in the program, in order. For every case in each switch, emit an
    /// (accept or reject) output consisting of the basecase interpretation of all data before the
    /// given switch, the switch case's data, and then the basecase interpretation of all data after
    /// the switch.
    fn run_(&self) -> (Vec<Vec<u8>>, Vec<Vec<u8>>) {
        let mut accept: Vec<Vec<u8>> = Default::default();
        let mut reject: Vec<Vec<u8>> = Default::default();
        let mut prefix: Vec<u8> = Default::default();
        let mut stack: Vec<Frame> = Default::default();
        for (pc, op) in self.program.iter().enumerate() {
            match op {
                Op::SwitchPush => stack
                    .push(Frame { return_: self.continuations[&pc], prefix_len: prefix.len() }),
                Op::SwitchPop => {
                    let Frame { prefix_len, .. } = stack.pop().unwrap();
                    prefix.truncate(prefix_len);
                    let cont_stack = vec![self.continuations[&pc]];
                    if DEBUG {
                        eprintln!("- delimited continuation: {pc} -> {cont_stack:?}");
                    }
                    self.run_continuation(cont_stack, &mut prefix);
                }
                Op::U8(data) => prefix.push(*data),
                Op::U32(data) => prefix.extend(&data.to_le_bytes()),
                Op::U64(data) => prefix.extend(&data.to_le_bytes()),
                Op::Case(case) => {
                    if DEBUG {
                        match case {
                            Case::Accept => eprint!("accept{}: ", accept.len()),
                            Case::Reject => eprint!("reject{}: ", reject.len()),
                        };
                    }
                    let results = match case {
                        Case::Accept => &mut accept,
                        Case::Reject => &mut reject,
                    };
                    let Frame { prefix_len, .. } = stack.last().unwrap();
                    let stack = stack.iter().map(|frame| frame.return_).collect();
                    let mut data = prefix.clone();
                    if DEBUG {
                        eprintln!("{pc} -> {stack:?}");
                    }
                    let final_pc = self.run_continuation(stack, &mut data);
                    let returned = "Returned from escape continuation";
                    assert_eq!(final_pc, self.program.len(), "{returned} at {final_pc}.");
                    results.push(data);
                    prefix.truncate(*prefix_len);
                }
            }
        }
        assert_eq!(&stack, &[]);
        (accept, reject)
    }

    /// Given an initial return stack, run the program until the last stack frame is exited,
    /// running only basecase cases of each switch encountered, emitting the data to the `Vec`
    /// passed in the `out` parameter.
    ///
    /// The return value is the program counter when the last stack frame was exited.
    ///
    /// If the given stack is the full stack at a certain point in program execution, the
    /// continuation is an escape continuation that will run the program until completion.
    ///
    /// If the given stack is a consecutive slice of the stack at a certain point in program
    /// execution, the continuation is a delimited continuation.
    fn run_continuation(&self, mut stack: Vec<usize>, out: &mut Vec<u8>) -> usize {
        let mut pc = stack.pop().unwrap();
        while let Some(op) = self.program.get(pc) {
            match op {
                Op::SwitchPush => stack.push(self.continuations[&pc]),
                Op::SwitchPop => panic!("Fell through a switch at {pc}."),
                Op::U8(data) => out.push(*data),
                Op::U32(data) => out.extend(&data.to_le_bytes()),
                Op::U64(data) => out.extend(&data.to_le_bytes()),
                Op::Case(Case::Accept) => {
                    if let Some(pc_) = stack.pop() {
                        if DEBUG {
                            eprintln!("- ret: {pc} -> {pc_}");
                        }
                        pc = pc_;
                        continue;
                    }
                    return pc;
                }
                Op::Case(Case::Reject) => panic!("Rejected base case at {}.", pc),
            }
            pc += 1;
        }
        assert_eq!(&stack, &[]);
        pc
    }
}

/// Analyze a program to calculate the index of the target of each [`Op`] that implicitly refers to
/// another location in the program.
fn collect_continuations(program: &[Op]) -> BTreeMap<usize, usize> {
    let mut continuations = BTreeMap::new();
    let mut switch_stack = vec![];
    for (pc, op) in program.iter().enumerate() {
        match op {
            Op::SwitchPush => switch_stack.push(pc),
            Op::SwitchPop => {
                let push_pc = switch_stack.pop().unwrap();
                let pop_pc = pc;
                // A `SwitchPush` pushes its continuation onto the return stack; the return address
                // for an `Ok`/`Fail` is after the switch.
                continuations.insert(push_pc, pop_pc + 1);
                // When we "fall through" a switch after executing all the `Ok`/`Fail` cases, we
                // re-run the switch's first (delimited) continuation in basecase mode before
                // proceeding.
                continuations.insert(pop_pc, push_pc + 1);
            }
            _ => (),
        }
    }
    assert_eq!(&switch_stack, &[]);
    continuations
}
