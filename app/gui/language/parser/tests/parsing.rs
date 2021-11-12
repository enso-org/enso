#![feature(generators, generator_trait)]

use parser::prelude::*;

use ast::test_utils::expect_shape;
use ast::*;
use parser::api::Metadata;
use parser::api::ParsedSourceFile;
use serde::de::DeserializeOwned;
use serde::Deserialize;
use serde::Serialize;
use std::str::FromStr;
use wasm_bindgen_test::wasm_bindgen_test;
use wasm_bindgen_test::wasm_bindgen_test_configure;

wasm_bindgen_test_configure!(run_in_browser);



// ===============
// === Helpers ===
// ===============

/// Asserts that given AST is a Var with given name.
fn assert_var<StringLike: Into<String>>(ast: &Ast, name: StringLike) {
    let actual: &Var = expect_shape(ast);
    let expected = Var { name: name.into() };
    assert_eq!(*actual, expected);
}

/// Asserts that given AST is an Opr with given name.
fn assert_opr<StringLike: Into<String>>(ast: &Ast, name: StringLike) {
    let actual: &Opr = expect_shape(ast);
    let expected = Opr { name: name.into() };
    assert_eq!(*actual, expected);
}

fn roundtrip_program_with(parser: &parser::Parser, program: &str) {
    let ast = parser.parse(program.to_string(), Default::default()).unwrap();
    assert_eq!(ast.repr(), program, "{:#?}", ast);
}

fn roundtrip_program(program: &str) {
    let parser = parser::Parser::new_or_panic();
    roundtrip_program_with(&parser, program);
}



// ================
// === Metadata ===
// ================

/// Wrapper for using any serializable type as metadata.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
struct FauxMetadata<T>(T);

impl<T: Default + Serialize + DeserializeOwned> Metadata for FauxMetadata<T> {}



// ===============
// === Fixture ===
// ===============

/// Persists parser (which is expensive to construct, so we want to reuse it
/// between tests. Additionally, hosts a number of helper methods.
struct Fixture {
    parser: parser::Parser,
}

impl Fixture {
    // === Helper methods ===

    /// Create a new fixture, obtaining a default parser.
    fn new() -> Fixture {
        Fixture { parser: parser::Parser::new_or_panic() }
    }

    /// Program is expected to be single line module. The line's Shape subtype
    /// is obtained and passed to `tester`.
    fn test_shape<T, F>(&mut self, program: &str, tester: F)
    where
        for<'t> &'t Shape<Ast>: TryInto<&'t T>,
        F: FnOnce(&T) -> (), {
        let ast = self.parser.parse_line_ast(program).unwrap();
        let shape = expect_shape(&ast);
        tester(shape);
    }


    // === Test Methods ===

    fn blank_line_round_trip(&mut self) {
        let program = "main = \n    foo\n   \n    bar";
        let ast = self.parser.parse_module(program, default()).unwrap();
        assert_eq!(ast.repr(), program);
    }

    fn deserialize_metadata(&mut self) {
        let term = ast::Module { lines: vec![ast::BlockLine { elem: None, off: 0 }] };
        let ast = known::KnownAst::new_no_id(term);
        let file = ParsedSourceFile { ast, metadata: serde_json::json!({}) };
        let code = String::try_from(&file).unwrap();
        assert_eq!(self.parser.parse_with_metadata(code).unwrap(), file);
    }

    fn deserialize_unrecognized(&mut self) {
        let unfinished = "`";
        self.test_shape(unfinished, |shape: &Unrecognized| {
            assert_eq!(shape.str, "`");
        });
    }

    #[allow(dead_code)] // TODO [mwu] https://github.com/enso-org/enso/issues/1016
    fn deserialize_unexpected(&mut self) {
        let unexpected = "import";
        let ast = self.parser.parse_line_ast(unexpected).unwrap();
        // This does not deserialize to "Unexpected" but to a very complex macro match tree that has
        // Unexpected somewhere within. We just make sure that it is somewhere, and that confirms
        // that we are able to deserialize such node.
        let has_unexpected =
            ast.iter_recursive().find(|ast| matches!(ast.shape(), Shape::Unexpected(_)));
        assert!(has_unexpected.is_some());
    }

    fn deserialize_invalid_quote(&mut self) {
        let unfinished = "'a''";
        self.test_shape(unfinished, |shape: &Prefix<Ast>| {
            // ignore shape.func, being TextUnclosed tested elsewhere
            let arg: &InvalidQuote = expect_shape(&shape.arg);
            let expected_quote = Text { str: "''".into() };
            assert_eq!(arg.quote, expected_quote.into());
        });
    }

    fn deserialize_inline_block(&mut self) {
        let unfinished = "'''a";
        self.test_shape(unfinished, |shape: &Prefix<Ast>| {
            let func: &InlineBlock = expect_shape(&shape.func);
            let expected_quote = Text { str: "'''".into() };
            assert_eq!(func.quote, expected_quote.into());
            assert_var(&shape.arg, "a");
        });
    }

    fn deserialize_blank(&mut self) {
        let expect_blank = |_: &Blank| {};
        let _ast = self.test_shape("_", expect_blank);
    }

    fn deserialize_var(&mut self) {
        self.test_shape("foo", |var: &Var| {
            let expected_var = Var { name: "foo".into() };
            assert_eq!(var, &expected_var);
        });
    }

    fn deserialize_cons(&mut self) {
        let name = "FooBar";
        self.test_shape(name, |shape: &Cons| {
            assert_eq!(shape.name, name);
        });
    }

    fn deserialize_mod(&mut self) {
        self.test_shape("+=", |shape: &Mod| {
            assert_eq!(shape.name, "+");
        });
    }

    fn deserialize_invalid_suffix(&mut self) {
        self.test_shape("foo'bar", |shape: &InvalidSuffix<Ast>| {
            assert_var(&shape.elem, "foo'");
            assert_eq!(shape.suffix, "bar");
        });
    }

    fn deserialize_number(&mut self) {
        self.test_shape("127", |shape: &Number| {
            assert_eq!(shape.base, None);
            assert_eq!(shape.int, "127");
        });

        self.test_shape("16_ff", |shape: &Number| {
            assert_eq!(shape.base.as_ref().unwrap(), "16");
            assert_eq!(shape.int, "ff");
        });
    }

    fn deserialize_text_line_raw(&mut self) {
        self.test_shape("\"foo\"", |shape: &TextLineRaw| {
            let (segment,) = (&shape.text).expect_tuple();
            let expected = SegmentPlain { value: "foo".to_string() };
            assert_eq!(*segment, expected.into());
        });

        let tricky_raw = r#""\\\'\n""#;
        self.test_shape(tricky_raw, |shape: &TextLineRaw| {
            let segments: (_,) = (&shape.text).expect_tuple();
            assert_eq!(*segments.0, SegmentPlain { value: r"\\\'\n".to_string() }.into());
        });
    }

    fn test_text_fmt_segment<F>(&mut self, program: &str, tester: F)
    where F: FnOnce(&SegmentFmt<Ast>) -> () {
        self.test_shape(program, |shape: &TextLineFmt<Ast>| {
            let (segment,) = (&shape.text).expect_tuple();
            tester(segment)
        });
    }

    fn deserialize_text_line_fmt(&mut self) {
        use SegmentFmt::SegmentExpr;

        // plain segment
        self.test_shape("'foo'", |shape: &TextLineFmt<Ast>| {
            let (segment,) = (&shape.text).expect_tuple();
            let expected = SegmentPlain { value: "foo".into() };
            assert_eq!(*segment, expected.into());
        });

        // escapes
        let tricky_fmt = r#"'\\\'\"'"#;
        self.test_shape(tricky_fmt, |shape: &TextLineFmt<Ast>| {
            let segments: (_, _, _) = (&shape.text).expect_tuple();
            assert_eq!(*segments.0, Slash {}.into());
            assert_eq!(*segments.1, Quote {}.into());
            assert_eq!(*segments.2, Invalid { str: '"' }.into());
        });

        // expression empty
        let expr_fmt = r#"'``'"#;
        self.test_text_fmt_segment(expr_fmt, |segment| match segment {
            SegmentExpr(expr) => assert_eq!(expr.value, None),
            _ => panic!("wrong segment type received"),
        });

        // expression non-empty
        let expr_fmt = r#"'`foo`'"#;
        self.test_text_fmt_segment(expr_fmt, |segment| match segment {
            SegmentExpr(expr) => assert_var(expr.value.as_ref().unwrap(), "foo"),
            _ => panic!("wrong segment type received"),
        });

        self.test_text_fmt_segment(r#"'\n'"#, |segment| {
            let expected = EscapeCharacter { c: 'n' };
            assert_eq!(*segment, expected.into());
        });
        self.test_text_fmt_segment(r#"'\u0394'"#, |segment| {
            let expected = EscapeUnicode16 { digits: "0394".into() };
            assert_eq!(*segment, expected.into());
        });
        // TODO [MWU] We don't test Unicode21 as it is not yet supported by the
        //            parser.
        self.test_text_fmt_segment(r#"'\U0001f34c'"#, |segment| {
            let expected = EscapeUnicode32 { digits: "0001f34c".into() };
            assert_eq!(*segment, expected.into());
        });
    }

    fn deserialize_text_block_raw(&mut self) {
        let program = "\"\"\" \n  \n   X";
        self.test_shape(program, |shape: &TextBlockRaw| {
            assert_eq!(shape.spaces, 1);
            assert_eq!(shape.offset, 0);

            let (line,) = (&shape.text).expect_tuple();
            let (empty_line,) = (&line.empty_lines).expect_tuple();
            assert_eq!(*empty_line, 2);

            let (segment,) = (&line.text).expect_tuple();
            let expected_segment = SegmentPlain { value: "   X".into() };
            assert_eq!(*segment, expected_segment.into());
        });
    }

    fn deserialize_text_block_fmt(&mut self) {
        let program = "'''  \n\n X\n Y";
        self.test_shape(program, |shape: &TextBlockFmt<Ast>| {
            assert_eq!(shape.spaces, 2);
            assert_eq!(shape.offset, 0);
            assert_eq!(shape.text.len(), 2);

            let (line1, line2) = (&shape.text).expect_tuple();
            let (empty_line,) = (&line1.empty_lines).expect_tuple();
            assert_eq!(*empty_line, 0);
            let (segment,) = (&line1.text).expect_tuple();
            let expected_segment = SegmentPlain { value: " X".into() };
            assert_eq!(*segment, expected_segment.into());

            assert!(line2.empty_lines.is_empty());
            let (segment,) = (&line2.text).expect_tuple();
            let expected_segment = SegmentPlain { value: " Y".into() };
            assert_eq!(*segment, expected_segment.into());
        });
    }


    fn deserialize_unfinished_text(&mut self) {
        let unfinished = r#""\"#;
        self.test_shape(unfinished, |shape: &TextUnclosed<Ast>| {
            let line = &shape.line;
            let line: &TextLineRaw = line.try_into().unwrap();

            let (segment,) = (&line.text).expect_tuple();
            let expected = SegmentPlain { value: r"\".into() };
            assert_eq!(*segment, expected.into());
        });
    }

    fn deserialize_dangling_base(&mut self) {
        self.test_shape("16_", |shape: &DanglingBase| {
            assert_eq!(shape.base, "16");
        });
    }

    fn deserialize_prefix(&mut self) {
        self.test_shape("foo   bar", |shape: &Prefix<Ast>| {
            assert_var(&shape.func, "foo");
            assert_eq!(shape.off, 3);
            assert_var(&shape.arg, "bar");
        });
    }

    fn deserialize_infix(&mut self) {
        self.test_shape("foo +  bar", |shape: &Infix<Ast>| {
            assert_var(&shape.larg, "foo");
            assert_eq!(shape.loff, 1);
            assert_opr(&shape.opr, "+");
            assert_eq!(shape.roff, 2);
            assert_var(&shape.rarg, "bar");
        });
    }
    fn deserialize_left(&mut self) {
        self.test_shape("foo +", |shape: &SectionLeft<Ast>| {
            assert_var(&shape.arg, "foo");
            assert_eq!(shape.off, 1);
            assert_opr(&shape.opr, "+");
        });
    }
    fn deserialize_right(&mut self) {
        self.test_shape("+ bar", |shape: &SectionRight<Ast>| {
            assert_opr(&shape.opr, "+");
            assert_eq!(shape.off, 1);
            assert_var(&shape.arg, "bar");
        });
    }
    fn deserialize_sides(&mut self) {
        self.test_shape("+", |shape: &SectionSides<Ast>| {
            assert_opr(&shape.opr, "+");
        });
    }

    fn deserialize_block(&mut self) {
        self.test_shape(" foo\n bar", |block: &Block<Ast>| {
            assert_eq!(block.ty, BlockType::Continuous {});
            assert_eq!(block.indent, 1);
            assert_eq!(block.empty_lines.len(), 0);
            assert_eq!(block.is_orphan, true);

            let first_line = &block.first_line;
            assert_eq!(first_line.off, 0);
            assert_var(&first_line.elem, "foo");

            let (second_line,) = (&block.lines).expect_tuple();
            assert_eq!(second_line.off, 0);
            assert_var(second_line.elem.as_ref().unwrap(), "bar");
        });
    }

    fn deserialize_annotation(&mut self) {
        self.test_shape("@Tail_call", |annotation: &Annotation| {
            let expected_annotation = Annotation { name: "@Tail_call".into() };
            assert_eq!(annotation, &expected_annotation);
        });
    }

    /// Tests parsing a number of sample macro usages.
    ///
    /// As macros generate usually really huge ASTs, this test only checks
    /// that we are able to deserialize the response and that it is a macro
    /// match node. Node contents is not covered.
    fn deserialize_macro_matches(&mut self) {
        let macro_usages = vec![
            "[]",
            "[1,2,3]",
            "{x}",
            "polyglot java import com.example.MyClass",
            "foo -> bar",
            "()",
            "(foo -> bar)",
            "a b c -> bar",
            "type Maybe a\n    Just val:a",
            "if foo > 8 then 10 else 9",
            "skip bar",
            "freeze bar",
            "case foo of\n  bar",
            "import foo",
            "import",
            "export bar",
            "from bar import all",
            "from bar export bo",
            "a ->",
            "-> a",
            "(a -> b) -> c",
        ];

        for macro_usage in macro_usages.iter() {
            let ast = self.parser.parse_line_ast(*macro_usage).unwrap();
            expect_shape::<Match<Ast>>(&ast);
        }
    }

    fn deserialize_macro_ambiguous(&mut self) {
        self.test_shape("if  foo", |shape: &Ambiguous<Ast>| {
            let segment = &shape.segs.head;
            assert_var(&segment.head, "if");

            let segment_body = segment.body.as_ref().unwrap();
            assert_eq!(segment_body.off, 2);
            assert_var(&segment_body.wrapped, "foo");
        });
    }

    fn run(&mut self) {
        // Shapes not covered by separate test:
        // * Opr (doesn't parse on its own, covered by Infix and other)
        // * Module (covered by every single test, as parser wraps everything into module)
        self.blank_line_round_trip();
        self.deserialize_metadata();
        self.deserialize_unrecognized();
        //self.deserialize_unexpected(); // TODO [mwu] https://github.com/enso-org/enso/issues/1016
        self.deserialize_invalid_quote();
        self.deserialize_inline_block();
        self.deserialize_blank();
        self.deserialize_var();
        self.deserialize_cons();
        self.deserialize_mod();
        self.deserialize_invalid_suffix();
        self.deserialize_number();
        self.deserialize_text_line_raw();
        self.deserialize_text_line_fmt();
        self.deserialize_text_block_raw();
        self.deserialize_text_block_fmt();
        self.deserialize_unfinished_text();
        self.deserialize_dangling_base();
        self.deserialize_prefix();
        self.deserialize_infix();
        self.deserialize_left();
        self.deserialize_right();
        self.deserialize_sides();
        self.deserialize_block();
        self.deserialize_annotation();
        self.deserialize_macro_matches();
        self.deserialize_macro_ambiguous();
    }
}

/// A single entry point for all the tests here using external parser.
///
/// Setting up the parser is costly, so we run all tests as a single batch.
/// Until proper CI solution for calling external parser is devised, this
/// test is marked with `#[ignore]`.
#[wasm_bindgen_test]
fn parser_tests() {
    Fixture::new().run()
}

/// Test case for https://github.com/enso-org/ide/issues/296
#[wasm_bindgen_test]
fn block_roundtrip() {
    let programs = vec![
        "main = 10 + 10",
        "main =\n    a = 10\n    b = 20\n    a * b",
        "main =\n    foo a =\n        a * 10\n    foo 10\n    print \"hello\"",
        "main =\n    foo\n    \n    bar",
        "main =\n    \n    foo\n    \n    bar",
    ];
    for program in programs {
        roundtrip_program(program);
    }
}

/// Test case for https://github.com/enso-org/ide/issues/296
#[wasm_bindgen_test]
fn nested_macros() {
    let parser = parser::Parser::new_or_panic();

    // Generate nested brackets. Stop at 8 because it gets slower and slower.
    // At 12 the deserialization fails on WASM.
    // At 14 the parsing fails in parser-service.
    for i in 0..8 {
        let program = format!("{}{}{}", "[".repeat(i), "foo", "]".repeat(i));
        roundtrip_program_with(&parser, &program);
    }

    // Cases from https://github.com/enso-org/ide/issues/1351
    let program = r#"from Standard.Base import all

main =
    operator13 = Json.from_pairs [["a", 42], ["foo", [1,2,3]]]
    var1 = [operator13, operator13]"#;
    roundtrip_program_with(&parser, &program);

    let program = r#"triplets n = 1.up_to n . to_vector . flat_map a->
    a+1 . up_to n . to_vector . flat_map b->
        b+1 . up_to n . to_vector . flat_map c->
            if a+b+c == n then [[a,b,c]] else []
n = 10
here.triplets n
IO.println(here.triplets n)"#;
    roundtrip_program_with(&parser, &program);
}

#[wasm_bindgen_test]
fn dealing_with_invalid_metadata() {
    let f = Fixture::new();

    let id = ast::Id::from_str("52233542-5c73-430b-a2b7-a68aaf81341b").unwrap();
    let var = ast::Ast::new(ast::Var { name: "variable1".into() }, Some(id));
    let module = ast::Module::from_line(var);
    let ast = known::Module::new_no_id(module);
    let metadata = FauxMetadata("certainly_not_a_number".to_string());

    // Make sure that out metadata cannot be deserialized as `FauxMetadata<i32>`.
    let serialized_text_metadata = serde_json::to_string(&metadata).unwrap();
    assert!(serde_json::from_str::<FauxMetadata<i32>>(&serialized_text_metadata).is_err());

    let parsed_file = parser::api::ParsedSourceFile { ast, metadata };
    let generated = parsed_file.serialize().unwrap();
    let expected_generated = r#"variable1


#### METADATA ####
[[{"index":{"value":0},"size":{"value":9}},"52233542-5c73-430b-a2b7-a68aaf81341b"]]
"certainly_not_a_number""#;
    assert_eq!(generated.content, expected_generated);
    let r = f.parser.parse_with_metadata::<FauxMetadata<i32>>(generated.content).unwrap();
    assert_eq!(r.metadata, default());
}
