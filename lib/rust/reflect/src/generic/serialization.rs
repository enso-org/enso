use crate::generic::*;
use std::mem::take;



// ============================
// === Test Case Generation ===
// ============================

#[derive(Debug, Clone)]
pub struct TestCases {
    pub accept:  Vec<Vec<u8>>,
    pub reject:  Vec<Vec<u8>>,
    program:   Vec<Op>,
    debuginfo: BTreeMap<usize, String>,
}

/// Generate test cases.
///
/// Produces 100% coverage of valid structures (i.e. every variant of every enum occurs in some
/// `accept` case), and a representative set of `reject` cases.
pub fn testcases(graph: &TypeGraph, root: TypeId) -> TestCases {
    let mut builder = ProgramBuilder::new(graph, root);
    builder.type_(root, Default::default());
    let ProgramBuilder { program, debuginfo, .. } = builder;
    eprintln!("{}", fmt_program(&program, &debuginfo));
    let (accept, reject) = Vm::run(&program);
    TestCases { accept, reject, program, debuginfo }
}

impl TestCases {
    pub fn to_json(&self) -> String {
        let accept: Vec<_> = self.accept.iter().map(|case| format!("{:?}", case)).collect();
        let accept = accept.join(", \n\t");
        let reject: Vec<_> = self.reject.iter().map(|case| format!("{:?}", case)).collect();
        let reject = reject.join(", \n\t");
        format!(
            "{{
\"accept\": [
\t{accept}
],
\"reject\": [
\t{reject}
]}}
"
        )
    }

    pub fn to_java(&self) -> String {
        let fmt_cases = |cases: &[Vec<u8>]| {
            let cases: Vec<_> = cases
                .iter()
                .map(|case| {
                    let case: Vec<_> = case.iter().map(|byte| (*byte as i8).to_string()).collect();
                    format!("{{{}}}", case.join(", "))
                })
                .collect();
            let cases = cases.join(", ");
            cases
        };
        let accept = fmt_cases(&self.accept);
        let reject = fmt_cases(&self.reject);
        format!(
            "import out.Tree;
import utils.BincodeMessage;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;

class GeneratedFormatTests {{
    public static void main(String[] args) {{
        byte[][] accept = {{{accept}}};
        byte[][] reject = {{{reject}}};
        int result = 0;
        for (int i = 0; i < accept.length; i++) {{
            ByteBuffer buffer = ByteBuffer.wrap(accept[i]);
            buffer.order(ByteOrder.LITTLE_ENDIAN);
            ByteBuffer context = ByteBuffer.allocate(0);
            BincodeMessage message = new BincodeMessage(buffer, context, 0);
            try {{
                Tree tree = Tree.deserialize(message);
                System.out.print(\"- pass: \");
                System.out.println(tree.toString());
            }} catch (utils.IncompatibleFormatException e) {{
                System.out.println(\"- fail:\");
                e.printStackTrace();
                result = 1;
            }}
        }}
        for (int i = 0; i < reject.length; i++) {{
            ByteBuffer buffer = ByteBuffer.wrap(reject[i]);
            buffer.order(ByteOrder.LITTLE_ENDIAN);
            ByteBuffer context = ByteBuffer.allocate(0);
            BincodeMessage message = new BincodeMessage(buffer, context, 0);
            try {{
                Tree tree = Tree.deserialize(message);
                System.out.print(\"- fail: accepted: \");
                System.out.println(tree.toString());
                result = 1;
            }} catch (utils.IncompatibleFormatException e) {{
                System.out.println(\"- pass: (rejected)\");
            }}
        }}
        System.exit(result);
    }}
}}
"
        )
    }

    pub fn program(&self) -> String {
        fmt_program(&self.program, &self.debuginfo)
    }
}

fn fmt_program(program: &[Op], debuginfo: &BTreeMap<usize, String>) -> String {
    use std::fmt::Write;
    let mut out = String::new();
    let mut indent = 0;
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
    Ok,
    Fail,
}



// ==========================
// === Program Generation ===
// ==========================

/// Generates test-case-generating program for a type graph.
#[derive(Debug)]
struct ProgramBuilder<'g> {
    graph:      &'g TypeGraph,
    will_visit: BTreeSet<TypeId>,
    visited:    BTreeSet<TypeId>,
    debuginfo:  BTreeMap<usize, String>,
    program:    Vec<Op>,
}

impl<'g> ProgramBuilder<'g> {
    fn new(graph: &'g TypeGraph, root: TypeId) -> Self {
        let mut graph_ = graph.clone();
        graph_.gc(vec![root]);
        let out = Default::default();
        let visited = Default::default();
        let mut will_visit = BTreeSet::new();
        for id in graph_.type_ids() {
            let ty = &graph[id];
            match &ty.data {
                Data::Struct(fields) => will_visit.extend(fields.iter().map(|field| field.type_)),
                _ => continue,
            }
            will_visit.extend(ty.discriminants.values());
        }
        let debuginfo = Default::default();
        Self { graph, program: out, visited, will_visit, debuginfo }
    }

    fn emit(&mut self, op: Op) {
        self.program.push(op);
    }

    fn debug(&mut self, debug: impl Into<String>) {
        let n = self.program.len();
        self.debuginfo.insert(n, debug.into());
    }

    fn debug_(&mut self, debug: impl Into<String>) {
        let n = self.program.len() - 1;
        self.debuginfo.insert(n, debug.into());
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
            Data::Primitive(primitive) => self.primitive(*primitive, basecase),
        }
    }

    fn primitive(&mut self, primitive: Primitive, basecase: bool) {
        match primitive {
            Primitive::U32 => self.emit(Op::U32(0)),
            Primitive::Bool => self.emit(Op::U8(1)),
            Primitive::Usize => self.emit(Op::U64(12345678901234567890)),
            Primitive::String => self.emit(Op::U64(0)),
            Primitive::Sequence(t0) if basecase || self.basecase(t0) => self.emit(Op::U64(0)),
            Primitive::Sequence(t0) => {
                self.emit(Op::U64(1));
                self.type_(t0, basecase);
            }
            Primitive::Option(t0) if basecase || self.basecase(t0) => self.emit(Op::U8(0)),
            Primitive::Option(t0) => {
                self.emit(Op::U8(1));
                self.type_(t0, basecase);
            }
            Primitive::Result(t0, t1) => {
                // Trivial heuristic: Assume Err is a better basecase.
                if basecase {
                    self.emit(Op::U32(1));
                    self.type_(t1, basecase);
                } else {
                    self.emit(Op::SwitchPush);
                    self.emit(Op::U32(1));
                    self.type_(t1, basecase);
                    self.emit(Op::Ok);
                    if !self.basecase(t0) {
                        self.emit(Op::U32(0));
                        self.type_(t0, basecase);
                        self.emit(Op::Ok);
                    }
                    self.emit(Op::U32(2));
                    self.emit(Op::Fail);
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
                    let basecase_discriminant = self.select_basecase(id).unwrap();
                    let basecase_ty = ty.discriminants[&basecase_discriminant];
                    if basecase {
                        self.emit(Op::U32(basecase_discriminant as u32));
                        self.object(vec![basecase_ty], basecase);
                    } else {
                        let (&max, _) = ty.discriminants.last_key_value().unwrap();
                        self.emit(Op::SwitchPush);
                        self.emit(Op::U32(basecase_discriminant as u32));
                        self.debug_(self.graph[basecase_ty].name.to_string());
                        self.object(vec![basecase_ty], basecase);
                        self.emit(Op::Ok);
                        for i in 0..=(max + 1) {
                            if i == basecase_discriminant {
                                continue;
                            }
                            self.emit(Op::U32(i as u32));
                            match ty.discriminants.get(&i) {
                                Some(id) => {
                                    self.debug_(self.graph[*id].name.to_string());
                                    self.object(vec![*id], basecase);
                                    self.emit(Op::Ok);
                                }
                                None => self.emit(Op::Fail),
                            }
                        }
                        self.emit(Op::SwitchPop);
                    }
                } else {
                    self.object(take(&mut hierarchy), basecase);
                }
            }
            self.debug(format!(".{}", &field.name));
            self.type_(field.type_, basecase);
        }
        assert!(hierarchy.is_empty());
    }

    fn select_basecase(&self, id: TypeId) -> Option<usize> {
        // The selected case:
        //  - *Must* not cause recursion.
        //  - *Should* be "small".

        // Simple heuristic: If there's a child with no fields that are Objects, choose it.
        'children: for (&i, &id) in &self.graph[id].discriminants {
            let ty = &self.graph[id];
            if ty.child_field.is_some() {
                continue;
            }
            let fields = match &ty.data {
                Data::Struct(fields) => fields,
                Data::Primitive(_) => panic!("Encountered a primitive that extends a class."),
            };
            for field in fields {
                match &self.graph[field.type_].data {
                    Data::Struct(_) => continue 'children,
                    Data::Primitive(Primitive::Option(t0))
                    | Data::Primitive(Primitive::Sequence(t0))
                    if !self.will_visit.contains(t0) =>
                        continue 'children,
                    Data::Primitive(Primitive::Result(t0, t1))
                    if !self.will_visit.contains(t0)
                        && !self.will_visit.contains(t1) =>
                        continue 'children,
                    _ => (),
                }
            }
            return Some(i);
        }
        // FIXME: This fallback is not guaranteed to be recursion-free; some inputs could fail with
        // a stack overflow.
        self.graph[id].discriminants.keys().next().cloned()
    }
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
        let mut stack: Vec<usize> = Default::default();
        let mut prefix_stack: Vec<usize> = Default::default();
        let mut pc = 0;
        while pc != self.program.len() {
            match self.program[pc] {
                Op::SwitchPush => {
                    stack.push(self.continuations[&pc]);
                    prefix_stack.push(prefix.len());
                }
                Op::SwitchPop => {
                    let pc_ = stack.pop();
                    debug_assert_eq!(pc, pc_.unwrap() - 1);
                    pc = self.continuations[&pc];
                    prefix_stack.pop();
                    if let Some(n) = prefix_stack.last() {
                        prefix.truncate(*n);
                    } else {
                        assert_eq!(pc, self.program.len());
                    }
                    continue;
                }
                Op::U8(data) => prefix.push(data),
                Op::U32(data) => prefix.extend(&data.to_le_bytes()),
                Op::U64(data) => prefix.extend(&data.to_le_bytes()),
                Op::Ok => {
                    accept.push(self.run_continuation(stack.clone(), prefix.clone()));
                    prefix.truncate(*prefix_stack.last().unwrap());
                }
                Op::Fail => {
                    reject.push(self.run_continuation(stack.clone(), prefix.clone()));
                    prefix.truncate(*prefix_stack.last().unwrap());
                }
            }
            pc += 1;
        }
        assert_eq!(&prefix_stack, &[]);
        assert_eq!(&prefix, &[]);
        assert_eq!(&stack, &[]);
        (accept, reject)
    }

    fn run_continuation(&self, mut stack: Vec<usize>, mut out: Vec<u8>) -> Vec<u8> {
        let mut pc = stack.pop().unwrap();
        while pc != self.program.len() {
            match self.program[pc] {
                Op::SwitchPush => stack.push(self.continuations[&pc]),
                Op::SwitchPop => panic!("Fell through a switch at {}.", pc),
                Op::U8(data) => out.push(data),
                Op::U32(data) => out.extend(&data.to_le_bytes()),
                Op::U64(data) => out.extend(&data.to_le_bytes()),
                Op::Ok => {
                    pc = stack.pop().unwrap();
                    continue;
                }
                Op::Fail => panic!("Rejected base case at {}.", pc),
            }
            pc += 1;
        }
        assert_eq!(&stack, &[]);
        out
    }
}

fn collect_continuations(program: &[Op]) -> BTreeMap<usize, usize> {
    let mut continuations = BTreeMap::new();
    let mut switch_continuations_awaited = vec![];
    let mut switch_phis_awaited = vec![];
    for (i, op) in program.iter().enumerate() {
        match op {
            Op::SwitchPush => switch_continuations_awaited.push(i),
            Op::SwitchPop => {
                continuations.insert(switch_continuations_awaited.pop().unwrap(), i + 1);
                switch_phis_awaited.push(i);
            }
            Op::Ok | Op::Fail =>
                if let Some(phi) = switch_phis_awaited.pop() {
                    continuations.insert(phi, i + 1);
                },
            _ => (),
        }
    }
    if let Some(phi) = switch_phis_awaited.pop() {
        continuations.insert(phi, program.len());
    }
    assert_eq!(&switch_continuations_awaited, &[]);
    assert_eq!(&switch_phis_awaited, &[]);
    eprintln!("{:?}", &continuations);
    continuations
}
