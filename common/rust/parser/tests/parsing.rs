use prelude::*;

use ast::*;
use parser::api::IsParser;


// ===============
// === Helpers ===
// ===============

/// Takes Ast being a module with a single line and returns that line's AST.
fn expect_single_line(ast:&Ast) -> &Ast {
    let module:&Module<Ast> = expect_shape(ast);
    let (line,)             = (&module.lines).expect_tuple();
    line.elem.as_ref().unwrap()
}

/// "Downcasts" given AST's Shape to `T`.
fn expect_shape<'t,T>(ast:&'t Ast) -> &'t T
where &'t Shape<Ast>: TryInto<&'t T> {
    match ast.shape().try_into() {
        Ok(shape) => shape,
        _         => {
            let expected_typename = std::any::type_name::<T>();
            panic!("failed converting shape into {}",expected_typename)
        },
    }
}

/// Asserts that given AST is a Var with given name.
fn assert_var<StringLike: Into<String>>(ast:&Ast, name:StringLike) {
    let actual  : &Var = expect_shape(ast);
    let expected       = Var {name:name.into()};
    assert_eq!(*actual,expected);
}

/// Asserts that given AST is an Opr with given name.
fn assert_opr<StringLike:Into<String>>(ast:&Ast, name:StringLike) {
    let actual  : &Opr = expect_shape(ast);
    let expected       = Opr {name:name.into()};
    assert_eq!(*actual,expected);
}


// ===================
// === ExpectTuple ===
// ===================

// === Trait ===
/// Helper allowing converting between collections and tuples. Does heavy
/// unwarapping, shouldn't be used outside test environment.
trait ExpectTuple<T> {
    /// Convert Self to tuple `T`. Panic if collection has different count of
    /// elements.
    fn expect_tuple(self) -> T;
}

// === Implementations ===
// TODO [MWU] boilerplate below should be generated with macro

impl<Collection:IntoIterator>
ExpectTuple<(Collection::Item,)> for Collection {
    fn expect_tuple(self) -> (Collection::Item,) {
        let mut iter = self.into_iter();
        let     v1   = iter.next().unwrap();
        assert!(iter.next().is_none());
        (v1,)
    }
}

impl<Collection: IntoIterator>
ExpectTuple<(Collection::Item,Collection::Item)>
for Collection {
    fn expect_tuple(self) -> (Collection::Item,Collection::Item) {
        let mut iter = self.into_iter();
        let     v1   = iter.next().unwrap();
        let     v2   = iter.next().unwrap();
        assert!(iter.next().is_none());
        (v1,v2)
    }
}

impl<Collection: IntoIterator>
ExpectTuple<(Collection::Item,Collection::Item,Collection::Item)>
for Collection {
    fn expect_tuple
    (self) -> (Collection::Item,Collection::Item,Collection::Item) {
        let mut iter = self.into_iter();
        let     v1   = iter.next().unwrap();
        let     v2   = iter.next().unwrap();
        let     v3   = iter.next().unwrap();
        assert!(iter.next().is_none());
        (v1,v2,v3)
    }
}

// ===============
// === Fixture ===
// ===============

/// Persists parser (which is expensive to construct, so we want to reuse it
/// between tests. Additionally, hosts a number of helper methods.
struct Fixture(parser::Parser);

impl Fixture {
    // === Helper methods ===

    /// Create a new fixture, obtaining a default parser.
    fn new() -> Fixture {
        Fixture(parser::Parser::new_or_panic())
    }

    /// Runs parser on given input, panics on any error.
    fn parse(&mut self, program:&str) -> Ast {
        self.0.parse(program.into()).unwrap()
    }

    /// Program is expected to be single line module. The line's AST is
    /// returned. Panics otherwise.
    fn parse_line(&mut self, program:&str) -> Ast {
        let ast  = self.parse(program);
        let line = expect_single_line(&ast);
        line.clone()

    }
    /// Program is expected to be single line module. The line's Shape subtype
    /// is obtained and passed to `tester`.
    fn test_shape<T,F>(&mut self, program:&str, tester:F)
    where for<'t> &'t Shape<Ast>: TryInto<&'t T>,
                      F         : FnOnce(&T) -> () {
        let ast   = self.parse_line(program);
        let shape = expect_shape(&ast);
        tester(shape);
    }

    // === Test Methods ===
    fn deserialize_unrecognized(&mut self) {
        let unfinished = "`";
        self.test_shape(unfinished,|shape:&Unrecognized| {
            assert_eq!(shape.str,"`");
        });
    }

    fn deserialize_invalid_quote(&mut self) {
        let unfinished = "'a''";
        self.test_shape(unfinished,|shape:&Prefix<Ast>| {
            // ignore shape.func, being TextUnclosed tested elsewhere
            let arg:&InvalidQuote = expect_shape(&shape.arg);
            let expected_quote    = Text {str:"''".into()};
            assert_eq!(arg.quote,expected_quote.into());
        });
    }

    fn deserialize_inline_block(&mut self) {
        let unfinished = "'''a";
        self.test_shape(unfinished,|shape:&Prefix<Ast>| {
            let func:&InlineBlock = expect_shape(&shape.func);
            let expected_quote    = Text {str: "'''".into()};
            assert_eq!(func.quote,expected_quote.into());
            assert_var(&shape.arg,"a");
        });
    }

    fn deserialize_blank(&mut self) {
        let expect_blank = |_:&Blank| {};
        let _ast         = self.test_shape("_",expect_blank);
    }

    fn deserialize_var(&mut self) {
        self.test_shape("foo",|var: &Var| {
            let expected_var = Var {name:"foo".into()};
            assert_eq!(var,&expected_var);
        });
    }

    fn deserialize_cons(&mut self) {
        let name = "FooBar";
        self.test_shape(name,|shape:&Cons| {
            assert_eq!(shape.name,name);
        });
    }

    fn deserialize_mod(&mut self) {
        self.test_shape("+=",|shape:&Mod| {
            assert_eq!(shape.name,"+");
        });
    }

    fn deserialize_invalid_suffix(&mut self) {
        self.test_shape("foo'bar",|shape:&InvalidSuffix<Ast>| {
            assert_var(&shape.elem,"foo'");
            assert_eq!(shape.suffix,"bar");
        });
    }

    fn deserialize_number(&mut self) {
        self.test_shape("127",|shape:&Number| {
            assert_eq!(shape.base,None);
            assert_eq!(shape.int,"127");
        });

        self.test_shape("16_ff",|shape:&Number| {
            assert_eq!(shape.base.as_ref().unwrap(),"16");
            assert_eq!(shape.int,"ff");
        });
    }

    fn deserialize_text_line_raw(&mut self) {
        self.test_shape("\"foo\"",|shape:&TextLineRaw| {
            let (segment,)  = (&shape.text).expect_tuple();
            let expected = SegmentPlain{value: "foo".to_string()};
            assert_eq!(*segment,expected.into());
        });

        let tricky_raw = r#""\\\"\n""#;
        self.test_shape(tricky_raw,|shape:&TextLineRaw| {
            let segments: (_,_,_) = (&shape.text).expect_tuple();
            assert_eq!(*segments.0,Slash {}.into());
            assert_eq!(*segments.1,RawQuote {}.into());
            assert_eq!(*segments.2,Invalid {str: 'n'}.into());
            // Quote (fmt one) cannot be escaped in raw string. So no test for
            // it, even though it belongs to the same enum.
        });
    }

    fn deserialize_text_line_fmt(&mut self) {
        use SegmentFmt::SegmentExpr;

        // plain segment
        self.test_shape("'foo'",|shape:&TextLineFmt<Ast>| {
            let (segment,)  = (&shape.text).expect_tuple();
            let expected = SegmentPlain{value: "foo".into()};
            assert_eq!(*segment,expected.into());
        });

        // escapes
        let tricky_fmt = r#"'\\\'\"'"#;
        self.test_shape(tricky_fmt,|shape:&TextLineFmt<Ast>| {
            let segments: (_,_,_) = (&shape.text).expect_tuple();
            assert_eq!(*segments.0,Slash{}.into() );
            assert_eq!(*segments.1,Quote{}.into() );
            assert_eq!(*segments.2,Invalid{ str: '"'}.into() );
        });

        // expression empty
        let expr_fmt = r#"'``'"#;
        self.test_shape(expr_fmt,|shape:&TextLineFmt<Ast>| {
            let (segment,)  = (&shape.text).expect_tuple();
            match segment {
                SegmentExpr(expr) => assert_eq!(expr.value,None),
                _                 => panic!("wrong segment type received"),
            }
        });

        // expression non-empty
        let expr_fmt = r#"'`foo`'"#;
        self.test_shape(expr_fmt,|shape:&TextLineFmt<Ast>| {
            let (segment,)  = (&shape.text).expect_tuple();
            match segment {
                SegmentExpr(expr) =>
                    assert_var(expr.value.as_ref().unwrap(),"foo"),
                _ => panic!("wrong segment type received"),
            }
        });

        let expr_fmt = r#"'\n\u0394\U0001f34c'"#;
        self.test_shape(expr_fmt,|shape:&TextLineFmt<Ast>| {
            let segments: (_,_,_)  = (&shape.text).expect_tuple();

            let expected = Escape::Character{c:'n'};
            assert_eq!(*segments.0,expected.into());

            let expected = Escape::Unicode16{digits: "0394".into()};
            assert_eq!(*segments.1,expected.into());

            // TODO We don't test Unicode21 as it is not yet supported by
            //      parser.

            let expected = Escape::Unicode32{digits: "0001f34c".into()};
            assert_eq!(*segments.2,expected.into());
        });
    }

    fn deserialize_text_block_raw(&mut self) {
        let program = "\"\"\" \n  \n   X";
        self.test_shape(program,|shape:&TextBlockRaw| {
            assert_eq!(shape.spaces,1);
            assert_eq!(shape.offset,0);

            let (line,)       = (&shape.text).expect_tuple();
            let (empty_line,) = (&line.empty_lines).expect_tuple();
            assert_eq!(*empty_line,2);

            let (segment,)       = (&line.text).expect_tuple();
            let expected_segment = SegmentPlain { value: "   X".into() };
            assert_eq!(*segment,expected_segment.into());
        });
    }

    fn deserialize_text_block_fmt(&mut self) {
        let program = "'''  \n\n X\n Y";
        self.test_shape(program,|shape:&TextBlockFmt<Ast>| {
            assert_eq!(shape.spaces,2);
            assert_eq!(shape.offset,0);
            assert_eq!(shape.text.len(),2);

            let (line1,line2) = (&shape.text).expect_tuple();
            let (empty_line,) = (&line1.empty_lines).expect_tuple();
            assert_eq!(*empty_line,0);
            let (segment,)       = (&line1.text).expect_tuple();
            let expected_segment = SegmentPlain { value: " X".into() };
            assert_eq!(*segment,expected_segment.into());

            assert!(line2.empty_lines.is_empty());
            let (segment,)       = (&line2.text).expect_tuple();
            let expected_segment = SegmentPlain { value: " Y".into() };
            assert_eq!(*segment,expected_segment.into());
        });
    }


    fn deserialize_unfinished_text(&mut self) {
        let unfinished = r#""\"#;
        self.test_shape(unfinished,|shape:&TextUnclosed<Ast>| {
            let line              = &shape.line;
            let line:&TextLineRaw = line.try_into().unwrap();

            let (segment,) = (&line.text).expect_tuple();
            let expected   = Unfinished {};
            assert_eq!(*segment,expected.into());
        });
    }

    fn deserialize_dangling_base(&mut self) {
        self.test_shape("16_",|shape:&DanglingBase| {
            assert_eq!(shape.base,"16");
        });
    }

    fn deserialize_prefix(&mut self) {
        self.test_shape("foo   bar",|shape:&Prefix<Ast>| {
            assert_var(&shape.func,"foo");
            assert_eq!(shape.off,3);
            assert_var(&shape.arg,"bar");
        });
    }

    fn deserialize_infix(&mut self) {
        self.test_shape("foo +  bar",|shape:&Infix<Ast>| {
            assert_var(&shape.larg,"foo");
            assert_eq!(shape.loff,1);
            assert_opr(&shape.opr,"+");
            assert_eq!(shape.roff,2);
            assert_var(&shape.rarg,"bar");
        });
    }
    fn deserialize_left(&mut self) {
        self.test_shape("foo +",|shape:&SectionLeft<Ast>| {
            assert_var(&shape.arg,"foo");
            assert_eq!(shape.off,1);
            assert_opr(&shape.opr,"+");
        });
    }
    fn deserialize_right(&mut self) {
        self.test_shape("+ bar",|shape:&SectionRight<Ast>| {
            assert_opr(&shape.opr,"+");
            assert_eq!(shape.off,1);
            assert_var(&shape.arg,"bar");
        });
    }
    fn deserialize_sides(&mut self) {
        self.test_shape("+",|shape:&SectionSides<Ast>| {
            assert_opr(&shape.opr,"+");
        });
    }

    fn deserialize_block(&mut self) {
        self.test_shape(" foo\n bar",|block:&Block<Ast>| {
            assert_eq!(block.ty,BlockType::Continuous{});
            assert_eq!(block.indent,1);
            assert_eq!(block.empty_lines.len(),0);
            assert_eq!(block.is_orphan,true);

            let first_line = &block.first_line;
            assert_eq!(first_line.off,0);
            assert_var(&first_line.elem,"foo");

            let (second_line,) = (&block.lines).expect_tuple();
            assert_eq!(second_line.off,0);
            assert_var(second_line.elem.as_ref().unwrap(),"bar");
        });
    }

    /// Tests parsing a number of sample macro usages.
    ///
    /// As macros geneerate usually really huge ASTs, this test only checks
    /// that we are able to deserialize the response and that it is a macro
    /// match node. Node contents is not covered.
    fn deserialize_macro_matches(&mut self) {
        let macro_usages = vec!
            [ "foo -> bar"
            , "()"
            , "(foo -> bar)"
            , "type Maybe a\n    Just val:a"
            , "foreign Python3\n  bar"
            , "if foo > 8 then 10 else 9"
            , "skip bar"
            , "freeze bar"
            , "case foo of\n  bar"
            ];

        for macro_usage in macro_usages.iter() {
            let ast = self.parse_line(macro_usage);
            expect_shape::<Match<Ast>>(&ast);
        };
    }

    fn deserialize_macro_ambiguous(&mut self) {
        self.test_shape("if  foo",|shape:&Ambiguous| {
            let segment = &shape.segs.head;
            assert_var(&segment.head,"if");

            let segment_body = segment.body.as_ref().unwrap();
            assert_eq!(segment_body.off,2);
            assert_var(&segment_body.wrapped,"foo");
        });
    }

    fn run(&mut self) {
        // Shapes not covered by separate test:
        // * Opr (doesn't parse on its own, covered by Infix and other)
        // * Module (covered by every single test, as parser wraps everything
        //   into module)
        self.deserialize_unrecognized();
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
        self.deserialize_macro_matches();
        self.deserialize_macro_ambiguous();
    }
}

/// A single entry point for all the tests here using external parser.
///
/// Setting up the parser is costly, so we run all tests as a single batch.
/// Until proper CI solution for calling external parser is devised, this
/// test is marked with `#[ignore]`.
#[test]
#[ignore]
fn parser_tests() {
    Fixture::new().run()
}
