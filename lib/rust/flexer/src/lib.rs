#![deny(unconditional_recursion)]
#![feature(test)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unsafe_code)]
#![warn(unused_import_braces)]

//! This module exports the API for defining a simple lexer based on a deterministic finite state
//! automaton.
//!
//! Lexers defined using the Flexer are capable of lexing languages of significant complexity, and
//! while they're defined in a simple way (akin to regular grammars), they can work even with
//! context-sensitive languages.
//!
//! The process of defining a lexer involves the user doing the following:
//!
//! 1.  Creating a `Lexer` type that wraps the [`Flexer`].
//! 2.  Creating a `State` type, to hold the user-defined lexing state.
//! 3.  Implementing [`State`] for the `State` type.
//! 4.  Implementing [`Definition`] for the `Lexer`, along with lexing transition rules to lex the
//!     language.
//!
//! The result of defining a lexer using the flexer is a hybrid of the code written using this
//! library, and also the code that this library generates to specialize your lexer.
//!
//! # Writing a Lexer
//!
//! As the Flexer is a library for writing lexers, it would be remiss of us not to include a worked
//! example for how to define a lexer. The following example defines a lexer for a small language,
//! and shows you how to integrate the flexer code generation step with your project's build.
//!
//! ## The Language
//!
//! We're going to define a lexer for a very simple language, represented by the following
//! [EBNF](https://en.wikipedia.org/wiki/Extended_Backus%E2%80%93Naur_form) grammar.
//!
//! ```plain
//! a-word      = 'a'+;
//! b-word      = 'b'+;
//! word        = a-word | b-word;
//! space       = ' ';
//! spaced-word = space, word;
//! language    = word, spaced-word*;
//! ```
//!
//! ## The Lexer's Output
//!
//! Every lexer needs the ability to write a stream of tokens as its output. A flexer-based lexer
//! can use any type that it wants as its output type, but this language is going to use a very
//! simple `Token` type, wrapped into a `TokenStream`.
//!
//! ```
//! #[derive(Clone)]
//! pub enum Token {
//!     /// A word from the input, consisting of a sequence of all `a` or all `b`.
//!     Word(String),
//!     /// A token that the lexer is unable to recognise.
//!     Unrecognized(String)
//! }
//!
//! #[derive(Clone,Default)]
//! pub struct TokenStream {
//!     tokens:Vec<Token>
//! }
//!
//! impl TokenStream {
//!     pub fn push(&mut self,token:Token) {
//!         self.tokens.push(token)
//!     }
//! }
//! ```
//!
//! These tokens will be inserted into the token stream by our lexer as it recognises valid portions
//! of our language.
//!
//! Whatever you choose as the `Output` type of your lexer, it will need to implement both
//! [`std::clone::Clone`] and [`std::default::Default`].
//!
//! ## The Lexer's State
//!
//! Every Flexer-based lexer operates over a state that holds all of the user-defined state
//! information required to define the particular lexer. This state type must conform to the
//! [`State`] trait, which defines important functionality that it must provide to the flexer.
//!
//! In our language, we want to only be able to match words with a preceding space character once
//! we've seen an initial word that doesn't have one. To this end, we need a state in our lexer to
//! record that we've 'seen' the first word. As required by the [`State`] trait, we also need to
//! provide the flexer with an initial state, the state registry, and the bookmarks we use.
//!
//! ```
//! use flexer::group;
//! use flexer::prelude::reader::BookmarkManager;
//! use flexer::State;
//! #
//! #
//! # // === Token ===
//! #
//! # #[derive(Clone)]
//! # pub enum Token {
//! #     /// A word from the input, consisting of a sequence of all `a` or all `b`.
//! #     Word(String),
//! #     /// A token that the lexer is unable to recognise.
//! #     Unrecognized(String)
//! # }
//! #
//! # #[derive(Clone,Default)]
//! # pub struct TokenStream {
//! #     tokens:Vec<Token>
//! # }
//! #
//! # impl TokenStream {
//! #     pub fn push(&mut self,token:Token) {
//! #         self.tokens.push(token)
//! #     }
//! # }
//!
//!
//! // === LexerState ===
//!
//! #[derive(Debug)]
//! pub struct LexerState {
//!     /// The registry for groups in the lexer.
//!     lexer_states:group::Registry,
//!     /// The initial state of the lexer.
//!     initial_state:group::Identifier,
//!     /// The state entered when the first word has been seen.
//!     seen_first_word_state:group::Identifier,
//!     /// The bookmarks for this lexer.
//!     bookmarks:BookmarkManager
//! }
//! ```
//!
//! The flexer library provides useful functionality to help with defining your lexer state, such as
//! [`group::Registry`] for containing the various states through which your lexer may transition,
//! amd [`prelude::reader::BookmarkManager`] for storing bookmarks.
//!
//! > ### Bookmarks
//! > In order to enable arbitrary lookahead, the flexer provides a system for "bookmarking" a point
//! > in the input stream so that the lexer may return to it later. In fact, this mechanism is used
//! > _by default_ in the implementation to deal with overlapping rules, and so the
//! > [`prelude::reader::BookmarkManager`] provides some bookmarks for you by default.
//! >
//! > As a user, however, you can define additional bookmarks as part of your state, and mark or
//! > return to them as part of your lexer's transition functions (more on this below).
//!
//! Now that we have our state type, we need to define an implementation of [`State`] for it. This
//! is a mostly trivial exercise, but two functions ([`State::new()`] and [`State::specialize`])
//! require special attention. We'll look at both below.
//!
//! ```
//! use flexer::generate;
//! # use flexer::group;
//! use flexer::generate::GenError;
//! use flexer::prelude::AnyLogger;
//! # use flexer::prelude::reader::BookmarkManager;
//! # use flexer::State;
//! #
//! #
//! # // === Token ===
//! #
//! # #[derive(Clone)]
//! # pub enum Token {
//! #     /// A word from the input, consisting of a sequence of all `a` or all `b`.
//! #     Word(String),
//! #     /// A token that the lexer is unable to recognise.
//! #     Unrecognized(String)
//! # }
//! #
//! # #[derive(Clone,Default)]
//! # pub struct TokenStream {
//! #     tokens:Vec<Token>
//! # }
//! #
//! # impl TokenStream {
//! #     pub fn push(&mut self,token:Token) {
//! #         self.tokens.push(token)
//! #     }
//! # }
//! #
//! #
//! # // === LexerState ===
//! #
//! # #[derive(Debug)]
//! # pub struct LexerState {
//! #     /// The registry for groups in the lexer.
//! #     lexer_states:group::Registry,
//! #     /// The initial state of the lexer.
//! #     initial_state:group::Identifier,
//! #     /// The state entered when the first word has been seen.
//! #     seen_first_word_state:group::Identifier,
//! #     /// The bookmarks for this lexer.
//! #     bookmarks:BookmarkManager
//! # }
//!
//! impl flexer::State for LexerState {
//!     fn new(_logger:&impl AnyLogger) -> Self {
//!         // Here we construct all of the elements needed for our lexer state. This function can
//!         // contain arbitrarily complex logic and is only called once at initialization time.
//!         let mut lexer_states      = group::Registry::default();
//!         let initial_state         = lexer_states.define_group("ROOT",None);
//!         let seen_first_word_state = lexer_states.define_group("SEEN FIRST WORD",None);
//!         let bookmarks             = BookmarkManager::new();
//!         Self{lexer_states,initial_state,seen_first_word_state,bookmarks}
//!     }
//!
//!     fn initial_state(&self) -> group::Identifier {
//!         self.initial_state
//!     }
//!
//!     fn groups(&self) -> &group::Registry {
//!         &self.lexer_states
//!     }
//!
//!     fn groups_mut(&mut self) -> &mut group::Registry {
//!         &mut self.lexer_states
//!     }
//!
//!     fn bookmarks(&self) -> &BookmarkManager {
//!         &self.bookmarks
//!     }
//!
//!     fn bookmarks_mut(&mut self) -> &mut BookmarkManager {
//!         &mut self.bookmarks
//!     }
//!
//!     fn specialize(&self) -> Result<String,GenError> {
//!         // It is very important to pass both the type name of your lexer and your output
//!         // correctly here. This function should always be implemented as a call to the
//!         // below-used function.
//!         generate::specialize(self,"TestLexer","Token")
//!     }
//! }
//! ```
//!
//! ## Defining the Lexer Type
//!
//! With our state type defined, we now have the prerequisites for defining the lexer itself!
//!
//! The notion behind the way we define lexers in the flexer is to use a chain of
//! [`std::ops::Deref`] implementations to make the disparate parts feel like a cohesive whole.
//! The [`Flexer`] itself already implements deref to your state type, so all that remains is to do
//! the following:
//!
//! 1.  Define your lexer struct itself, containing an instance of the [`Flexer`], parametrised by
//!     your state and output types.
//!
//! ```
//! use flexer::Flexer;
//! # use flexer::generate;
//! # use flexer::group;
//! # use flexer::prelude::GenError;
//! # use flexer::prelude::AnyLogger;
//! use flexer::prelude::logger::Disabled;
//! # use flexer::prelude::reader::BookmarkManager;
//! # use flexer::State;
//!
//! type Logger = Disabled;
//! #
//! #
//! # // === Token ===
//! #
//! # #[derive(Clone)]
//! # pub enum Token {
//! #     /// A word from the input, consisting of a sequence of all `a` or all `b`.
//! #     Word(String),
//! #     /// A token that the lexer is unable to recognise.
//! #     Unrecognized(String)
//! # }
//! #
//! # #[derive(Clone,Default)]
//! # pub struct TokenStream {
//! #     tokens:Vec<Token>
//! # }
//! #
//! # impl TokenStream {
//! #     pub fn push(&mut self,token:Token) {
//! #         self.tokens.push(token)
//! #     }
//! # }
//! #
//! #
//! # // === LexerState ===
//! #
//! # #[derive(Debug)]
//! # pub struct LexerState {
//! #     /// The registry for groups in the lexer.
//! #     lexer_states:group::Registry,
//! #     /// The initial state of the lexer.
//! #     initial_state:group::Identifier,
//! #     /// The state entered when the first word has been seen.
//! #     seen_first_word_state:group::Identifier,
//! #     /// The bookmarks for this lexer.
//! #     bookmarks:BookmarkManager
//! # }
//! #
//! # impl flexer::State for LexerState {
//! #     fn new(_logger:&impl AnyLogger) -> Self {
//! #         // Here we construct all of the elements needed for our lexer state. This function can
//! #         // contain arbitrarily complex logic and is only called once at initialization time.
//! #         let mut lexer_states      = group::Registry::default();
//! #         let initial_state         = lexer_states.define_group("ROOT",None);
//! #         let seen_first_word_state = lexer_states.define_group("SEEN FIRST WORD",None);
//! #         let bookmarks             = BookmarkManager::new();
//! #         Self{lexer_states,initial_state,seen_first_word_state,bookmarks}
//! #     }
//! #
//! #     fn initial_state(&self) -> group::Identifier {
//! #         self.initial_state
//! #     }
//! #
//! #     fn groups(&self) -> &group::Registry {
//! #         &self.lexer_states
//! #     }
//! #
//! #     fn groups_mut(&mut self) -> &mut group::Registry {
//! #         &mut self.lexer_states
//! #     }
//! #
//! #     fn bookmarks(&self) -> &BookmarkManager {
//! #         &self.bookmarks
//! #     }
//! #
//! #     fn bookmarks_mut(&mut self) -> &mut BookmarkManager {
//! #         &mut self.bookmarks
//! #     }
//! #
//! #     fn specialize(&self) -> Result<String,GenError> {
//! #         // It is very important to pass both the type name of your lexer and your output
//! #         // correctly here. This function should always be implemented as a call to the
//! #         // below-used function.
//! #         generate::specialize(self,"TestLexer","Token")
//! #     }
//! # }
//!
//!
//! // === Lexer ===
//!
//! pub struct Lexer {
//!    lexer:Flexer<LexerState,TokenStream,Logger>
//! }
//! ```
//!
//! You'll note that the `Flexer` also takes a logging implementation from the Enso logging library
//! as a type parameter. This lets the client of the library configure the behaviour of logging in
//! their lexer. We recommend aliasing the current logger type (as shown above) for ease of use.
//!
//! 2.  Implement a `new()` function for your lexer.
//!
//! ```
//! # use flexer::Flexer;
//! # use flexer::generate;
//! # use flexer::group;
//! # use flexer::prelude::AnyLogger;
//! # use flexer::prelude::GenError;
//! # use flexer::prelude::logger::Disabled;
//! # use flexer::prelude::reader::BookmarkManager;
//! # use flexer::State;
//! #
//! # type Logger = Disabled;
//! #
//! #
//! # // === Token ===
//! #
//! # #[derive(Clone)]
//! # pub enum Token {
//! #     /// A word from the input, consisting of a sequence of all `a` or all `b`.
//! #     Word(String),
//! #     /// A token that the lexer is unable to recognise.
//! #     Unrecognized(String)
//! # }
//! #
//! # #[derive(Clone,Default)]
//! # pub struct TokenStream {
//! #     tokens:Vec<Token>
//! # }
//! #
//! # impl TokenStream {
//! #     pub fn push(&mut self,token:Token) {
//! #         self.tokens.push(token)
//! #     }
//! # }
//! #
//! #
//! # // === LexerState ===
//! #
//! # #[derive(Debug)]
//! # pub struct LexerState {
//! #     /// The registry for groups in the lexer.
//! #     lexer_states:group::Registry,
//! #     /// The initial state of the lexer.
//! #     initial_state:group::Identifier,
//! #     /// The state entered when the first word has been seen.
//! #     seen_first_word_state:group::Identifier,
//! #     /// The bookmarks for this lexer.
//! #     bookmarks:BookmarkManager
//! # }
//! #
//! # impl flexer::State for LexerState {
//! #     fn new(_logger:&impl AnyLogger) -> Self {
//! #         // Here we construct all of the elements needed for our lexer state. This function can
//! #         // contain arbitrarily complex logic and is only called once at initialization time.
//! #         let mut lexer_states      = group::Registry::default();
//! #         let initial_state         = lexer_states.define_group("ROOT",None);
//! #         let seen_first_word_state = lexer_states.define_group("SEEN FIRST WORD",None);
//! #         let bookmarks             = BookmarkManager::new();
//! #         Self{lexer_states,initial_state,seen_first_word_state,bookmarks}
//! #     }
//! #
//! #     fn initial_state(&self) -> group::Identifier {
//! #         self.initial_state
//! #     }
//! #
//! #     fn groups(&self) -> &group::Registry {
//! #         &self.lexer_states
//! #     }
//! #
//! #     fn groups_mut(&mut self) -> &mut group::Registry {
//! #         &mut self.lexer_states
//! #     }
//! #
//! #     fn bookmarks(&self) -> &BookmarkManager {
//! #         &self.bookmarks
//! #     }
//! #
//! #     fn bookmarks_mut(&mut self) -> &mut BookmarkManager {
//! #         &mut self.bookmarks
//! #     }
//! #
//! #     fn specialize(&self) -> Result<String,GenError> {
//! #         // It is very important to pass both the type name of your lexer and your output
//! #         // correctly here. This function should always be implemented as a call to the
//! #         // below-used function.
//! #         generate::specialize(self,"TestLexer","Token")
//! #     }
//! # }
//! #
//! #
//! # // === Lexer ===
//! #
//! # pub struct Lexer {
//! #    lexer:Flexer<LexerState,TokenStream,Logger>
//! # }
//!
//! impl Lexer {
//!     pub fn new() -> Self {
//!         let lexer = Flexer::new(Logger::new("Lexer"));
//!         Lexer{lexer}
//!     }
//! }
//! ```
//!
//! 3.  Define [`std::ops::Deref`] and [`std::ops::DerefMut`] for your lexer.
//!
//! ```
//! # use flexer::Flexer;
//! # use flexer::generate;
//! # use flexer::group;
//! # use flexer::prelude::AnyLogger;
//! # use flexer::prelude::GenError;
//! # use flexer::prelude::logger::Disabled;
//! # use flexer::prelude::reader::BookmarkManager;
//! # use flexer::State;
//! use std::ops::Deref;
//! use std::ops::DerefMut;
//! #
//! # type Logger = Disabled;
//! #
//! #
//! # // === Token ===
//! #
//! # #[derive(Clone)]
//! # pub enum Token {
//! #     /// A word from the input, consisting of a sequence of all `a` or all `b`.
//! #     Word(String),
//! #     /// A token that the lexer is unable to recognise.
//! #     Unrecognized(String)
//! # }
//! #
//! # #[derive(Clone,Default)]
//! # pub struct TokenStream {
//! #     tokens:Vec<Token>
//! # }
//! #
//! # impl TokenStream {
//! #     pub fn push(&mut self,token:Token) {
//! #         self.tokens.push(token)
//! #     }
//! # }
//! #
//! #
//! # // === LexerState ===
//! #
//! # #[derive(Debug)]
//! # pub struct LexerState {
//! #     /// The registry for groups in the lexer.
//! #     lexer_states:group::Registry,
//! #     /// The initial state of the lexer.
//! #     initial_state:group::Identifier,
//! #     /// The state entered when the first word has been seen.
//! #     seen_first_word_state:group::Identifier,
//! #     /// The bookmarks for this lexer.
//! #     bookmarks:BookmarkManager
//! # }
//! #
//! # impl flexer::State for LexerState {
//! #     fn new(_logger:&impl AnyLogger) -> Self {
//! #         // Here we construct all of the elements needed for our lexer state. This function can
//! #         // contain arbitrarily complex logic and is only called once at initialization time.
//! #         let mut lexer_states      = group::Registry::default();
//! #         let initial_state         = lexer_states.define_group("ROOT",None);
//! #         let seen_first_word_state = lexer_states.define_group("SEEN FIRST WORD",None);
//! #         let bookmarks             = BookmarkManager::new();
//! #         Self{lexer_states,initial_state,seen_first_word_state,bookmarks}
//! #     }
//! #
//! #     fn initial_state(&self) -> group::Identifier {
//! #         self.initial_state
//! #     }
//! #
//! #     fn groups(&self) -> &group::Registry {
//! #         &self.lexer_states
//! #     }
//! #
//! #     fn groups_mut(&mut self) -> &mut group::Registry {
//! #         &mut self.lexer_states
//! #     }
//! #
//! #     fn bookmarks(&self) -> &BookmarkManager {
//! #         &self.bookmarks
//! #     }
//! #
//! #     fn bookmarks_mut(&mut self) -> &mut BookmarkManager {
//! #         &mut self.bookmarks
//! #     }
//! #
//! #     fn specialize(&self) -> Result<String,GenError> {
//! #         // It is very important to pass both the type name of your lexer and your output
//! #         // correctly here. This function should always be implemented as a call to the
//! #         // below-used function.
//! #         generate::specialize(self,"TestLexer","Token")
//! #     }
//! # }
//! #
//! #
//! # // === Lexer ===
//! #
//! # pub struct Lexer {
//! #    lexer:Flexer<LexerState,TokenStream,Logger>
//! # }
//! #
//! # impl Lexer {
//! #     pub fn new() -> Self {
//! #         let lexer = Flexer::new(Logger::new("Lexer"));
//! #         Lexer{lexer}
//! #     }
//! # }
//!
//! impl Deref for Lexer {
//!     type Target = Flexer<LexerState,TokenStream,Logger> ;
//!     fn deref(&self) -> &Self::Target {
//!         &self.lexer
//!     }
//! }
//! impl DerefMut for Lexer {
//!     fn deref_mut(&mut self) -> &mut Self::Target {
//!         &mut self.lexer
//!     }
//! }
//! ```
//!
//! You'll note that here we've instantiated the flexer with a `Logger`. This is used for providing
//! debug information during development, and can be accessed from all scopes of your lexer. In
//! release mode, however, logging calls at the "trace", "debug", and "info" levels are optimised
//! away.
//!
//! ## Defining the Lexing Rules
//!
//! Flexer-based lexers operate by matching on a series of [`automata::pattern::Pattern`]s that
//! describe the language that it is trying to lex. It combines these patterns with "transition
//! functions" that may execute arbitrary code when a pattern matches on the lexer's input.
//!
//! In order to define the lexing rules, we need to implement [`Definition`] for our lexer,
//! particularly the [`Definition::define()`] function.
//!
//! ```
//! use flexer::automata::pattern::Pattern;
//! # use flexer::Flexer;
//! # use flexer::generate;
//! use flexer::group::Registry;
//! # use flexer::group;
//! # use flexer::prelude::AnyLogger;
//! # use flexer::prelude::GenError;
//! # use flexer::prelude::logger::Disabled;
//! # use flexer::prelude::reader::BookmarkManager;
//! # use flexer::State;
//! use flexer;
//! # use std::ops::Deref;
//! # use std::ops::DerefMut;
//! #
//! # type Logger = Disabled;
//! #
//! #
//! # // === Token ===
//! #
//! # #[derive(Clone)]
//! # pub enum Token {
//! #     /// A word from the input, consisting of a sequence of all `a` or all `b`.
//! #     Word(String),
//! #     /// A token that the lexer is unable to recognise.
//! #     Unrecognized(String)
//! # }
//! #
//! # #[derive(Clone,Default)]
//! # pub struct TokenStream {
//! #     tokens:Vec<Token>
//! # }
//! #
//! # impl TokenStream {
//! #     pub fn push(&mut self,token:Token) {
//! #         self.tokens.push(token)
//! #     }
//! # }
//! #
//! #
//! # // === LexerState ===
//! #
//! # #[derive(Debug)]
//! # pub struct LexerState {
//! #     /// The registry for groups in the lexer.
//! #     lexer_states:group::Registry,
//! #     /// The initial state of the lexer.
//! #     initial_state:group::Identifier,
//! #     /// The state entered when the first word has been seen.
//! #     seen_first_word_state:group::Identifier,
//! #     /// The bookmarks for this lexer.
//! #     bookmarks:BookmarkManager
//! # }
//! #
//! # impl flexer::State for LexerState {
//! #     fn new(_logger:&impl AnyLogger) -> Self {
//! #         // Here we construct all of the elements needed for our lexer state. This function can
//! #         // contain arbitrarily complex logic and is only called once at initialization time.
//! #         let mut lexer_states      = group::Registry::default();
//! #         let initial_state         = lexer_states.define_group("ROOT",None);
//! #         let seen_first_word_state = lexer_states.define_group("SEEN FIRST WORD",None);
//! #         let bookmarks             = BookmarkManager::new();
//! #         Self{lexer_states,initial_state,seen_first_word_state,bookmarks}
//! #     }
//! #
//! #     fn initial_state(&self) -> group::Identifier {
//! #         self.initial_state
//! #     }
//! #
//! #     fn groups(&self) -> &group::Registry {
//! #         &self.lexer_states
//! #     }
//! #
//! #     fn groups_mut(&mut self) -> &mut group::Registry {
//! #         &mut self.lexer_states
//! #     }
//! #
//! #     fn bookmarks(&self) -> &BookmarkManager {
//! #         &self.bookmarks
//! #     }
//! #
//! #     fn bookmarks_mut(&mut self) -> &mut BookmarkManager {
//! #         &mut self.bookmarks
//! #     }
//! #
//! #     fn specialize(&self) -> Result<String,GenError> {
//! #         // It is very important to pass both the type name of your lexer and your output
//! #         // correctly here. This function should always be implemented as a call to the
//! #         // below-used function.
//! #         generate::specialize(self,"TestLexer","Token")
//! #     }
//! # }
//! #
//! #
//! # // === Lexer ===
//! #
//! # pub struct Lexer {
//! #    lexer:Flexer<LexerState,TokenStream,Logger>
//! # }
//! #
//! # impl Lexer {
//! #     pub fn new() -> Self {
//! #         let lexer = Flexer::new(Logger::new("Lexer"));
//! #         Lexer{lexer}
//! #     }
//! # }
//! #
//! # impl Deref for Lexer {
//! #     type Target = Flexer<LexerState,TokenStream,Logger> ;
//! #     fn deref(&self) -> &Self::Target {
//! #         &self.lexer
//! #     }
//! # }
//! # impl DerefMut for Lexer {
//! #     fn deref_mut(&mut self) -> &mut Self::Target {
//! #         &mut self.lexer
//! #     }
//! # }
//!
//! impl flexer::Definition for Lexer {
//!     fn define() -> Self {
//!         // First we instantiate our lexer. Definitions take place _directly_ on the lexer, and
//!         // manipulate runtime state.
//!         let mut lexer = Self::new();
//!
//!         // Then, we define the patterns that we're going to use. For an overview of the p
//!         let a_word        = Pattern::char('a').many1();
//!         let b_word        = Pattern::char('b').many1();
//!         let space         = Pattern::char(' ');
//!         let spaced_a_word = &space >> &a_word;
//!         let spaced_b_word = &space >> &b_word;
//!         let any           = Pattern::any();
//!         let end           = Pattern::eof();
//!
//!         // Next, we define groups of lexer rules. This uses the groups that we've defined in our
//!         // lexer's state, and the patterns we've defined above.
//!         let root_group_id = lexer.initial_state;
//!         let root_group    = lexer.groups_mut().group_mut(root_group_id);
//!         root_group.create_rule(&a_word,"self.on_first_word(reader)");
//!         root_group.create_rule(&b_word,"self.on_first_word(reader)");
//!         root_group.create_rule(&end,   "self.on_no_err_suffix_first_word(reader)");
//!         root_group.create_rule(&any,   "self.on_err_suffix_first_word(reader)");
//!
//!         let seen_first_word_group_id = lexer.seen_first_word_state;
//!         let seen_first_word_group    = lexer.groups_mut().group_mut(seen_first_word_group_id);
//!         seen_first_word_group.create_rule(&spaced_a_word,"self.on_spaced_word(reader)");
//!         seen_first_word_group.create_rule(&spaced_b_word,"self.on_spaced_word(reader)");
//!         seen_first_word_group.create_rule(&end,          "self.on_no_err_suffix(reader)");
//!         seen_first_word_group.create_rule(&any,          "self.on_err_suffix(reader)");
//!
//!         lexer
//!     }
//!
//!     /// This function just returns the lexer's groups.
//!     fn groups(&self) -> &Registry {
//!         self.lexer.groups()
//!     }
//!
//!     /// Code you want to run before lexing begins.
//!     fn set_up(&mut self) {}
//!
//!     /// Code you want to run after lexing finishes.
//!     fn tear_down(&mut self) {}
//! }
//! ```
//!
//! > ### Transition Functions
//! > You may be wondering why the transition functions are specified as strings. This allows us to
//! > generate highly-efficient, specialized code for your lexer once you define it. More on this
//! > later.
//!
//! A [`group::Group`] in the lexer is like a state that operates on a stack. A transition function
//! can arbitrarily activate or deactivate a group on the flexer's stack, allowing you to perform
//! context-sensitive lexing behaviour. For more information (including on how to use parent groups
//! to inherit rules), see the relevant module.
//!
//! For more information on the [`automata::pattern::Pattern`] APIs used above, please see the
//! relevant module in this crate.
//!
//! ## Defining the Transition Functions
//!
//! You'll have noticed that, up above, we told the rules to use a bunch of transition functions
//! that we've not yet talked about. These functions can be defined anywhere you like, as long as
//! they are in scope in the file in which you are defining your lexer. We do, however, recommend
//! defining them on your lexer itself, so they can access and manipulate lexer state, so that's
//! what we're going to do here.
//!
//! ```
//! # use flexer::automata::pattern::Pattern;
//! # use flexer::Flexer;
//! # use flexer::generate;
//! # use flexer::group::Registry;
//! # use flexer::group;
//! # use flexer::prelude::AnyLogger;
//! use flexer::prelude::LazyReader;
//! # use flexer::prelude::GenError;
//! # use flexer::prelude::logger::Disabled;
//! # use flexer::prelude::reader::BookmarkManager;
//! # use flexer::State;
//! # use flexer;
//! # use std::ops::Deref;
//! # use std::ops::DerefMut;
//! #
//! # type Logger = Disabled;
//! #
//! #
//! # // === Token ===
//! #
//! # #[derive(Clone)]
//! # pub enum Token {
//! #     /// A word from the input, consisting of a sequence of all `a` or all `b`.
//! #     Word(String),
//! #     /// A token that the lexer is unable to recognise.
//! #     Unrecognized(String)
//! # }
//! #
//! # #[derive(Clone,Default)]
//! # pub struct TokenStream {
//! #     tokens:Vec<Token>
//! # }
//! #
//! # impl TokenStream {
//! #     pub fn push(&mut self,token:Token) {
//! #         self.tokens.push(token)
//! #     }
//! # }
//! #
//! #
//! # // === LexerState ===
//! #
//! # #[derive(Debug)]
//! # pub struct LexerState {
//! #     /// The registry for groups in the lexer.
//! #     lexer_states:group::Registry,
//! #     /// The initial state of the lexer.
//! #     initial_state:group::Identifier,
//! #     /// The state entered when the first word has been seen.
//! #     seen_first_word_state:group::Identifier,
//! #     /// The bookmarks for this lexer.
//! #     bookmarks:BookmarkManager
//! # }
//! #
//! # impl flexer::State for LexerState {
//! #     fn new(_logger:&impl AnyLogger) -> Self {
//! #         // Here we construct all of the elements needed for our lexer state. This function can
//! #         // contain arbitrarily complex logic and is only called once at initialization time.
//! #         let mut lexer_states      = group::Registry::default();
//! #         let initial_state         = lexer_states.define_group("ROOT",None);
//! #         let seen_first_word_state = lexer_states.define_group("SEEN FIRST WORD",None);
//! #         let bookmarks             = BookmarkManager::new();
//! #         Self{lexer_states,initial_state,seen_first_word_state,bookmarks}
//! #     }
//! #
//! #     fn initial_state(&self) -> group::Identifier {
//! #         self.initial_state
//! #     }
//! #
//! #     fn groups(&self) -> &group::Registry {
//! #         &self.lexer_states
//! #     }
//! #
//! #     fn groups_mut(&mut self) -> &mut group::Registry {
//! #         &mut self.lexer_states
//! #     }
//! #
//! #     fn bookmarks(&self) -> &BookmarkManager {
//! #         &self.bookmarks
//! #     }
//! #
//! #     fn bookmarks_mut(&mut self) -> &mut BookmarkManager {
//! #         &mut self.bookmarks
//! #     }
//! #
//! #     fn specialize(&self) -> Result<String,GenError> {
//! #         // It is very important to pass both the type name of your lexer and your output
//! #         // correctly here. This function should always be implemented as a call to the
//! #         // below-used function.
//! #         generate::specialize(self,"TestLexer","Token")
//! #     }
//! # }
//! #
//! #
//! # // === Lexer ===
//! #
//! # pub struct Lexer {
//! #    lexer:Flexer<LexerState,TokenStream,Logger>
//! # }
//! #
//! # impl Lexer {
//! #     pub fn new() -> Self {
//! #         let lexer = Flexer::new(Logger::new("Lexer"));
//! #         Lexer{lexer}
//! #     }
//! # }
//! #
//! # impl Deref for Lexer {
//! #     type Target = Flexer<LexerState,TokenStream,Logger> ;
//! #     fn deref(&self) -> &Self::Target {
//! #         &self.lexer
//! #     }
//! # }
//! # impl DerefMut for Lexer {
//! #     fn deref_mut(&mut self) -> &mut Self::Target {
//! #         &mut self.lexer
//! #     }
//! # }
//! #
//! # impl flexer::Definition for Lexer {
//! #     fn define() -> Self {
//! #         // First we instantiate our lexer. Definitions take place _directly_ on the lexer, and
//! #         // manipulate runtime state.
//! #         let mut lexer = Self::new();
//! #
//! #         // Then, we define the patterns that we're going to use. For an overview of the p
//! #         let a_word        = Pattern::char('a').many1();
//! #         let b_word        = Pattern::char('b').many1();
//! #         let space         = Pattern::char(' ');
//! #         let spaced_a_word = &space >> &a_word;
//! #         let spaced_b_word = &space >> &b_word;
//! #         let any           = Pattern::any();
//! #         let end           = Pattern::eof();
//! #
//! #         // Next, we define groups of lexer rules. This uses the groups that we've defined in our
//! #         // lexer's state, and the patterns we've defined above.
//! #         let root_group_id = lexer.initial_state;
//! #         let root_group    = lexer.groups_mut().group_mut(root_group_id);
//! #         root_group.create_rule(&a_word,"self.on_first_word(reader)");
//! #         root_group.create_rule(&b_word,"self.on_first_word(reader)");
//! #         root_group.create_rule(&end,   "self.on_no_err_suffix_first_word(reader)");
//! #         root_group.create_rule(&any,   "self.on_err_suffix_first_word(reader)");
//! #
//! #         let seen_first_word_group_id = lexer.seen_first_word_state;
//! #         let seen_first_word_group    = lexer.groups_mut().group_mut(seen_first_word_group_id);
//! #         seen_first_word_group.create_rule(&spaced_a_word,"self.on_spaced_word(reader)");
//! #         seen_first_word_group.create_rule(&spaced_b_word,"self.on_spaced_word(reader)");
//! #         seen_first_word_group.create_rule(&end,          "self.on_no_err_suffix(reader)");
//! #         seen_first_word_group.create_rule(&any,          "self.on_err_suffix(reader)");
//! #
//! #         lexer
//! #     }
//! #
//! #     /// This function just returns the lexer's groups.
//! #     fn groups(&self) -> &Registry {
//! #         self.lexer.groups()
//! #     }
//! #
//! #     /// Code you want to run before lexing begins.
//! #     fn set_up(&mut self) {}
//! #
//! #     /// Code you want to run after lexing finishes.
//! #     fn tear_down(&mut self) {}
//! # }
//!
//! impl Lexer {
//!      pub fn on_first_word<R:LazyReader>(&mut self, _reader:&mut R) {
//!         let str = self.current_match.clone();
//!         let ast = Token::Word(str);
//!         self.output.push(ast);
//!         let id = self.seen_first_word_state;
//!         self.push_state(id);
//!     }
//!
//!     pub fn on_spaced_word<R:LazyReader>(&mut self, _reader:&mut R) {
//!         let str = self.current_match.clone();
//!         let ast = Token::Word(String::from(str.trim()));
//!         self.output.push(ast);
//!     }
//!
//!     pub fn on_err_suffix_first_word<R:LazyReader>(&mut self, _reader:&mut R) {
//!         let ast = Token::Unrecognized(self.current_match.clone());
//!         self.output.push(ast);
//!     }
//!
//!     pub fn on_err_suffix<R:LazyReader>(&mut self, reader:&mut R) {
//!         self.on_err_suffix_first_word(reader);
//!         self.pop_state();
//!     }
//!
//!     pub fn on_no_err_suffix_first_word<R:LazyReader>(&mut self, _reader:&mut R) {}
//!
//!     pub fn on_no_err_suffix<R:LazyReader>(&mut self, reader:&mut R) {
//!         self.on_no_err_suffix_first_word(reader);
//!         self.pop_state();
//!     }
//! }
//! ```
//!
//! > ### Magic Transition Functions
//! > The transition functions are the 'secret sauce', so to speak, of the Flexer. They are called
//! > when a rule matches, and allow arbitrary code to manipulate the lexer. This means that the
//! > flexer can be used to define very complex grammars while still keeping a simple interface and
//! > ensuring performant execution.
//!
//! You'll note that all of these functions have a couple of things in common:
//!
//! 1.  They have a type parameter `R` that conforms to the [`prelude::LazyReader`] trait.
//! 2.  They take an argument of type `R`, that is the reader over which the lexer is running.
//!
//! Both of these, combined, allow the transition functions to manipulate the text being read by the
//! lexer.
//!
//! ## Specializing the Lexer
//!
//! In order to actually _use_ the lexer that you've defined, you need to specialize it to the rules
//! that you define. Unfortunately, `cargo` doesn't have support for post-build hooks, and so this
//! is a little more involved than we'd like it to be.
//!
//! 1.  Create a file that performs the definition of the lexer as above. It can use multiple files
//!     in its crate as long as they are publicly exposed.
//! 2.  Create a separate cargo project that has a prebuild hook in its `build.rs`.
//! 3.  In that build.rs, you need to:
//!     1. Import the lexer definition and instantiate it using `::define()`.
//!     2. Call [`State::specialize()`] on the resultant lexer. This will generate a string that
//!        contains the optimised lexer implementation.
//!     3. Write both the generated code and the code from the original lexer definition into an
//!        output file.
//! 4.  Re-export this output file from your cargo project's `lib.rs`.
//!
//! The process of specialization will generate quite a bit of code, but most importantly it will
//! generate `pub fn run<R:LazyReader>(&mut self, mut reader:R) -> Result<Output>`, where `Output`
//! is your lexer's token type. All of these functions are defined on your lexer type (the one whose
//! name is provided to `specialize()`.
//!
//! ## In Summary
//!
//! The flexer allows its clients to define highly optimised lexer implementations that are capable
//! of lexing languages of a high complexity.

use crate::prelude::*;
use prelude::logger::*;

use crate::generate::GenError;
use prelude::logger::AnyLogger;
use prelude::reader::BookmarkManager;

pub mod automata;
pub mod data;
pub mod generate;
pub mod group;

/// Useful libraries for working with the flexer.
pub mod prelude {
    pub use crate::generate::GenError;
    pub use enso_prelude::*;
    pub use lazy_reader::LazyReader;
    pub use lazy_reader::Reader;
    pub use logger::AnyLogger;

    /// The lazy reader library.
    pub mod reader {
        pub use lazy_reader::*;
    }

    /// The Enso logging library.
    pub mod logger {
        pub use enso_logger::*;
        pub use enso_logger::disabled::Logger as Disabled;
        pub use enso_logger::enabled::Logger as Enabled;
    }
}



// =================
// === Constants ===
// =================

mod constants {
    /// The number of 'frames' to reserve in the state stack, aiming to avoid re-allocation in hot
    /// code paths.
    pub const STATE_STACK_RESERVATION:usize = 1024;
}



// ==============
// === Flexer ===
// ==============

/// The flexer is an engine for generating lexers.
///
/// Akin to flex and other lexer generators, it is given a definition as a series of rules from
/// which it then generates code for a highly optimised lexer implemented on top of a
/// [DFA](https://en.wikipedia.org/wiki/Deterministic_finite_automaton).
///
/// Lexers defined using the flexer work on a stack of _states_, where a state is represented by a
/// [`crate::group::Group`]. Being in a given state (represented below by the top of the
/// `state_stack`) means that the flexer can match a certain set of rules associated with that
/// state. The user may cause the lexer to transition between states by pushing and popping states
/// on the stack, thus allowing a much more flexible lexing engine than pure regular grammars.
#[derive(Clone,Debug)]
pub struct Flexer<Definition,Output,Logger> {
    /// The stack of states that are active during lexer execution.
    pub state_stack:NonEmptyVec<group::Identifier>,
    /// The result of the current stage of the DFA.
    pub status:StageStatus,
    /// The tokens that have been lexed.
    pub output:Output,
    /// The text of the current match of the lexer.
    pub current_match:String,
    /// A logger for the flexer, accessible in user definitions.
    pub logger:Logger,
    /// The definition of the user-provided state for the lexer.
    definition:Definition,
}

impl<Definition,Output,Logger> Flexer<Definition,Output,Logger>
where Definition : State,
      Logger     : AnyLogger<Owned=Logger>,
      Output     : Default {
    /// Create a new lexer instance.
    pub fn new(parent_logger:impl AnyLogger) -> Flexer<Definition,Output,Logger> {
        let logger           = <Logger>::sub(&parent_logger,"Flexer");
        let status           = default();
        let output           = default();
        let definition       = Definition::new(&logger);
        let initial_state_id = definition.initial_state();
        let mut state_stack  = NonEmptyVec::singleton(initial_state_id);
        let current_match    = default();

        state_stack.reserve(constants::STATE_STACK_RESERVATION);
        Flexer{state_stack,status,output,definition,current_match,logger}
    }
}

impl<Definition,Output,Logger> Flexer<Definition,Output,Logger>
where Definition : State,
      Output     : Clone,
      Logger     : AnyLogger<Owned=Logger> {
    /// Get the lexer result.
    pub fn result(&mut self) -> &Output {
        &self.output
    }

    /// Get the lexer's initial state.
    pub fn initial_state(&self) -> group::Identifier {
        self.definition.initial_state()
    }

    /// Get the state that the lexer is currently in.
    pub fn current_state(&self) -> group::Identifier {
        *self.state_stack.last()
    }

    /// Tell the lexer to enter the state described by `state`.
    pub fn push_state(&mut self, state:group::Identifier) {
        self.logger.group_begin(
            ||format!("Enter State: {}",self.groups().group(state).name.as_str())
        );
        self.state_stack.push(state);
    }

    /// End the current state, returning the popped state identifier if one was ended.
    ///
    /// It will never end the initial state of the lexer.
    pub fn pop_state(&mut self) -> Option<group::Identifier> {
        let result = self.state_stack.pop();
        match result {
            None        => (),
            Some(ident) => debug!(self.logger,"Leave State: {self.groups().group(ident)}"),
        };
        self.logger.group_end();
        result
    }

    /// End states until the specified `state` is reached, leaving the lexer in `state`.
    ///
    /// If `state` does not exist on the lexer's stack, then the lexer will be left in the root
    /// state. Additionally, this function cannot pop the final occurrence of the root state.
    pub fn pop_states_until(&mut self, state:group::Identifier) -> group::Identifier {
        while self.current_state() != state && self.current_state() != self.initial_state() {
            self.pop_state();
        }
        *self.state_stack.last()
    }

    /// End states up to and including the first instance of `state`, returning the identifier of
    /// the new state the lexer is in.
    ///
    /// If `state` does not exist on the lexer's stack, the lexer will be left in the root state.
    /// Additionally, this function cannot pop the final occurrence of the root state.
    pub fn pop_states_including(&mut self, state:group::Identifier) -> group::Identifier {
        while self.current_state() != state && self.current_state() != self.initial_state() {
            self.pop_state();
        }
        self.pop_state();
        *self.state_stack.last()
    }

    /// Check if the lexer is currently in the state described by `state`.
    pub fn is_in_state(&self, state:group::Identifier) -> bool {
        self.current_state() == state
    }
}

// === Trait Impls ===

impl<Definition,Output,Logger> Deref for Flexer<Definition,Output,Logger> {
    type Target = Definition;
    fn deref(&self) -> &Self::Target {
        &self.definition
    }
}

impl<Definition,Output,Logger> DerefMut for Flexer<Definition,Output,Logger> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.definition
    }
}



// ==================
// === SubStateId ===
// ==================

/// An identifier for a sub-state of the lexer to transition to.
#[derive(Copy,Clone,Debug,Default,PartialEq)]
pub struct SubStateId(usize);

impl SubStateId {
    /// Create a new `SubStateId` with the specified value.
    pub fn new(val:usize) -> SubStateId {
        SubStateId(val)
    }
}


// === Trait Impls ===

impl From<usize> for SubStateId {
    fn from(val:usize) -> Self {
        SubStateId::new(val)
    }
}

impl From<&usize> for SubStateId {
    fn from(val:&usize) -> Self {
        SubStateId::new(*val)
    }
}

impl Into<usize> for SubStateId {
    fn into(self) -> usize {
        self.0
    }
}



// ===================
// === StageStatus ===
// ===================

/// The result of executing a single step of the DFA.
#[derive(Clone,Copy,Debug,PartialEq)]
pub enum StageStatus {
    /// The initial state of a lexer stage.
    Initial,
    /// The stage exits successfully, having consumed a complete token.
    ExitSuccess,
    /// The stage exits unsuccessfully.
    ExitFail,
    /// A single step of the DFA has executed successfully.
    ExitFinished,
    /// The lexer should continue, transitioning to the included state.
    ContinueWith(SubStateId)
}

impl StageStatus {
    /// Check if the lexer stage should continue.
    pub fn should_continue(&self) -> bool {
        self.continue_as().is_some()
    }

    /// Obtain the state to which the lexer should transition, iff the lexer should continue.
    pub fn continue_as(&self) -> Option<SubStateId> {
        match self {
            StageStatus::Initial           => Some(SubStateId::new(0)),
            StageStatus::ContinueWith(val) => Some(*val),
            _                              => None
        }
    }
}


// === Trait Impls ===

impl Default for StageStatus {
    fn default() -> Self {
        StageStatus::Initial
    }
}



// ==============
// === Result ===
// ==============

/// The result of executing the lexer on a given input.
#[derive(Clone,Debug)]
pub struct LexingResult<T> {
    /// The kind of the result, representing _how_ the lexer completed.
    pub kind:ResultKind,
    /// The tokens that the lexer was able to process.
    pub tokens:T
}

impl<T> LexingResult<T> {
    /// Create a new lexer result using the provided `kind` and `tokens`.
    pub fn new(kind:ResultKind,tokens:T) -> LexingResult<T> {
        LexingResult {kind,tokens}
    }

    /// Create a new success result, with the provided `tokens`.
    pub fn success(tokens:T) -> LexingResult<T> {
        LexingResult::new(ResultKind::Success, tokens)
    }

    /// Create a new partial lex result, with the provided `tokens`.
    pub fn partial(tokens:T) -> LexingResult<T> {
        LexingResult::new(ResultKind::Partial, tokens)
    }

    /// Create a failure result, with the `tokens` it _did_ manage to consume.
    pub fn failure(tokens:T) -> LexingResult<T> {
        LexingResult::new(ResultKind::Failure, tokens)
    }
}

/// The kind of lexer result.
#[derive(Copy,Clone,Debug)]
pub enum ResultKind {
    /// The lexer succeeded, returning the contained token stream.
    Success,
    /// The lexer succeeded on part of the input, returning the contained token stream.
    Partial,
    /// The lexer failed on the input, returning any tokens it _did_ manage to consume.
    Failure
}



// =============
// === State ===
// =============

/// Contains the state needed by the flexer from a lexer implementation.
///
/// The types for which this trait is implemented will normally also contain the user-defined state
/// for that lexer.
pub trait State {
    /// Create a new instance of the lexer's state.
    ///
    /// This function is guaranteed to be called at most once per run of the lexer.
    fn new(parent_logger:&impl AnyLogger) -> Self;
    /// Return the _initial_ lexing state.
    fn initial_state(&self) -> group::Identifier;
    /// Return a reference to the group registry for a given lexer.
    fn groups(&self) -> &group::Registry;
    /// Return a mutable reference to the group registry for a given lexer.
    fn groups_mut(&mut self) -> &mut group::Registry;
    /// Get an immutable reference to the bookmark manager for this state.
    fn bookmarks(&self) -> &BookmarkManager;
    /// Get a mutable reference to the bookmark manager for this state.
    fn bookmarks_mut(&mut self) -> &mut BookmarkManager;
    /// Generate code to specialize the flexer for the user's particular lexer definition.
    ///
    /// This function should be implemented as a call to [`generate::specialize`], passing
    /// the name of your lexer, and the name of your lexer's output type as a string.
    fn specialize(&self) -> Result<String,GenError>;
}



// ==================
// === Definition ===
// ==================

/// Allows for the definition of flexer-based lexers.
pub trait Definition {
    /// Define the custom lexer.
    fn define() -> Self;
    /// Obtain the registry of groups for the lexer.
    fn groups(&self) -> &group::Registry;
    /// Run before any lexing takes place.
    fn set_up(&mut self);
    /// Run after lexing has completed.
    fn tear_down(&mut self);
}
