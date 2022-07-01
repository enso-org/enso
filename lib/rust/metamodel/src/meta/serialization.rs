//! Serialization analysis on meta representations.

use crate::meta::*;
use std::fmt::Write;
use std::mem::take;

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
    let (accept, reject) = Vm::run(&program);
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
            Data::Struct(_) => {
                let mut hierarchy = vec![id];
                let mut id = id;
                while let Some(id_) = self.graph[id].parent {
                    id = id_;
                    hierarchy.push(id);
                }
                self.object(hierarchy, basecase);
            }
            Data::Primitive(primitive) => self.primitive(*primitive, basecase, id),
        }
    }

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
            Primitive::Sequence(t0) if basecase || self.basecase(t0) => self.emit(Op::U64(0)),
            Primitive::Sequence(t0) => {
                self.emit(Op::U64(1));
                self.type_(t0, basecase);
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

    fn object(&mut self, mut hierarchy: Vec<TypeId>, basecase: bool) {
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
                    if basecase {
                        self.emit(Op::U32(basecase_discriminant as u32));
                        self.object(vec![basecase_ty], basecase);
                    } else {
                        let (&max, _) = discriminants.last_key_value().unwrap();
                        self.emit(Op::SwitchPush);
                        self.emit(Op::U32(basecase_discriminant as u32));
                        self.debug_prev(&self.graph[basecase_ty].name);
                        self.object(vec![basecase_ty], basecase);
                        self.emit(Op::Case(Case::Accept));
                        for i in 0..=(max + 1) {
                            if i == basecase_discriminant {
                                continue;
                            }
                            self.emit(Op::U32(i as u32));
                            match discriminants.get(&i) {
                                Some(id) => {
                                    self.debug_prev(&self.graph[*id].name);
                                    self.object(vec![*id], basecase);
                                    self.emit(Op::Case(Case::Accept));
                                }
                                None => self.emit(Op::Case(Case::Reject)),
                            }
                        }
                        self.emit(Op::SwitchPop);
                    }
                } else {
                    self.object(take(&mut hierarchy), basecase);
                }
            }
            self.type_(field.type_, basecase);
            self.debug_prev(format!(".{}", &field.name));
        }
        assert!(hierarchy.is_empty());
    }
}

/// Choose a discriminant for the specified type, and if necessary for some other types reachable
/// from it in the composition graph, so that the composition graph for the type is non-recursive.
fn select_basecase(
    graph: &TypeGraph,
    id: TypeId,
    out: &mut BTreeMap<TypeId, usize>,
    visited: &mut BTreeSet<TypeId>,
) {
    select_basecase_(graph, id, out, visited).unwrap()
}

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
    // - If we have a child that doesn't own any sum-type fields, choose it and return Ok.
    // - Otherwise, recurse into each child; if one returns Ok, choose it and return Ok.
    // - If no child returns Ok, we got here by recursing into a bad choice; return Err.
    //
    // The top-level call will always return Ok because: There must be a sum type in our descendants
    // that has a child that doesn't own any sum-type fields, or there would be a type in the input
    // that is only possible to instantiate with cyclic or infinite data.
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
struct Vm<'p> {
    program:       &'p [Op],
    continuations: BTreeMap<usize, usize>,
}

#[derive(Debug, Default, PartialEq, Eq)]
struct Frame {
    return_:    usize,
    prefix_len: usize,
}

impl<'p> Vm<'p> {
    fn run(program: &'p [Op]) -> (Vec<Vec<u8>>, Vec<Vec<u8>>) {
        let continuations = collect_continuations(program);
        let self_ = Self { program, continuations };
        self_.run_()
    }

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
