# Parser Design

## 1. Lexer (Code -> Token Stream)

- Lexer needs to be generic over the input stream encoding to support utf-16
  coming from the JVM.
- Is there any use case that requires the lexer to read an actual file?
- The prelude needs to be released to crates.io otherwise we're going to rapidly
  get out of sync.
- I don't think it makes sense to have separate `Var` and `Cons` identifiers. We
  should instead have `Name`, with functions `is_referrent` and `is_variable`.
  This better mirrors how the language actually treats names.
- What actually is the flexer?
- What should the AST look like?

Lexer reads source file (lazily, line by line) or uses in-memory `&str` and produces token stream of `Var`, `Cons`, `Opr`, `Number`, `Text`, `Invalid`, and `Block`. Please note that `Block` is part of the token stream on purpose. It is important that the source code is easy to parse visually, so if you see a block, it should be a block. Discovering blocks in lexer allows us to prevent all other parts of parser, like macros, from breaking this assumption. Moreover, it makes the design of the following stages a lot simpler.  Enso lexer should always succeed, on any input stream (token stream could contain `Invalid` tokens). 

Lexer is defined using Rust procedural macro system. We are using procedural macros, because the lexer definition produces a Rust code (pastes it "in-place" of the macro usage). Let's consider a very simple lexer definition:

```rust
use crate::prelude::*; // Needs to be a released crate

use flexer;
use flexer::Flexer;



// =============
// === Token ===
// =============

pub struct Token {
    location : flexer::Location,
    ast      : TokenAst,
}

enum TokenAst {
    Var(ImString),
    Cons(ImString),
    Blank,
    ...
}

impl Token {
    pub fn new(location:Location, ast:TokenAst) -> Self {
        Self {location,ast}  
    }
  
    pub fn var(location:Location, name:impl Into<ImString>) -> Self {
        let ast = TokenAst::Var(name.into());
        Self::new(location,ast)      
    }
  
    ...
}



// =============
// === Lexer ===
// =============

#[derive(Debug,Default)]
struct Lexer {
    current : Option<Token>,
    tokens  : Vec<Token>,
    state   : Flexer::State
}

impl Lexer {
    fn on_ident(&mut self, tok:Token) {
        self.current = Some(tok);
        self.state.push(self.ident_sfx_check);
    }

    fn on_ident_err_sfx(&mut self) {
        println!("OH NO!")
    }

    fn on_no_ident_err_sfx(&mut self) {
        let current = std::mem::take(&mut self.current).unwrap();
        self.tokens.push_back(current);
    }
}

impl Flexer::Definition Lexer {
    fn state     (&    self) -> &    flexer::State { &    self.state }
    fn state_mut (&mut self) -> &mut flexer::State { &mut self.state }
}

pub fn lexer_source_code() -> String {
    let lexer = Flexer::<Lexer>::new();
  
    let chr     = alphaNum | '_';
    let blank   = Pattern::from('_');
    let body    = chr.many >> '\''.many();
    let var     = lowerLetter >> body;
    let cons    = upperLetter >> body;
    let breaker = "^`!@#$%^&*()-=+[]{}|;:<>,./ \t\r\n\\";
  
    let sfx_check = lexer.add(State("Identifier Suffix Check"));
  
    lexer.rule(lexer.root,var,"self.on_ident(Token::var(self.start_location,self.current_match()))");
    lexer.rule(lexer.root,cons,"self.on_ident(token::cons(self.start_location,self.current_match()))");
    lexer.rule(lexer.root,blank,"self.on_ident(token::blank(self.start_location))");
    lexer.rule(sfx_check,err_sfx,"self.on_ident_err_sfx()");
    lexer.rule(sfx_check,Flexer::always,"self.on_no_ident_err_sfx()");
    ...
    lexer.generate_specialized_code()
}

```

The idea here is that we are describing regexp-like patterns and tell what should happen when the pattern is matched. For example, after matching the `var` pattern, the code `self.on_ident(ast::Var)` should be evaluated. The code is passed as string, because it will be part of the generated, highly specialized, very fast lexer.

Technically, the patterns are first translated to a state machine, and then to a bunch of if-then-else statements in such a way, that parsing is always `O(n)` where `n` is the input size. Logically, the regular expressions are matched top-bottom  and the first fully-matched expression is chosen (unlike in the popular lexer generator flex, which uses longest match instead). After the expression is chosen, the associated function is executed and the process starts over again till the end of the input stream. Only the rules from the currently active state are considered. State is just a named (for debug purposes only) set of rules. Lexer always starts with the `lexer.root` state. You can make other state active by running (from within Flexer instance) `state.push(new_state)`, and pop it using `state.pop()`.

The `lexer.generate_specialized_code` first works in a few steps:

1. It takes all rules and states and generates an NFA state machine.
2. It generates DFA state machine using some custom optimizations to make sure that the regexps are matched in order and the associated code chunks are not lost.
3. It generates a highly tailored lexer `Engine` struct. One of the fields of the engine is the `Lexer` struct we defined above. The engine contains a main "loop" which consumes char by char, evaluates a big if-then-else machinery generated from the NFA, and evaluates functions from the `Lexer`. Please note that the functions start with `self`, that's because `Engine` implements `Deref` and `DerefMut` to `Lexer`.

The generation of the if-then-else code block is not defined in this document, but can be observed by:

1. Inspecting the current code in Scala.
2. Printing the Java code generated by current Scala Flexer implementation.
3. Talking with @wdanilo about it.



## 2. Macro Resolution (Token Stream -> Chunked AST Stream incl spaec-unaware AST)

To be described in detail taking into consideration all current use cases. For the current documentation of macro resolution, take a look here: https://github.com/luna/enso/blob/main/lib/syntax/specialization/shared/src/main/scala/org/enso/syntax/text/Parser.scala 

Before implementing this step, we need to talk about handling of space-unaware AST (the AST produced by user-macros).



## 3. Operator Resolution (Chunked AST Stream -> Chunked AST Stream with Opr Apps)

Using modified [Shunting-yard algorithm](https://en.wikipedia.org/wiki/Shunting-yard_algorithm). The algorithm is modified to support sections. The Scala implementation is here: https://github.com/luna/enso/blob/main/lib/syntax/definition/src/main/scala/org/enso/syntax/text/prec/Operator.scala . Unfortunatelly, we cannot use recursion in Rust, so it needs to be re-worked.



## 4. Finalization and Special Rules Discovery (Chunked AST Stream with Opr Apps -> AST)

To be described in detail taking into consideration all current use cases.





