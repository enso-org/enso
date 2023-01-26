// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use enso_prelude::*;

use ast::prelude::FallibleResult;

mod translation;


// ==============
// === Export ===
// ==============

pub use ast::IdMap;
pub use parser_scala::DocParser;

pub mod api;


#[derive(Debug, Clone, CloneRef)]
pub struct Parser {
    parser: Rc<enso_parser::Parser>,
}


// === Core methods provided by the underlying parser ===

impl Parser {
    pub fn new() -> Self {
        let parser = Rc::new(enso_parser::Parser::new());
        Self { parser }
    }

    pub fn parse(&self, program: String, ids: IdMap) -> api::Result<ast::Ast> {
        let tree = self.parser.run(&program);
        Ok(translation::to_legacy_ast(&tree))
    }

    pub fn parse_with_metadata<M: api::Metadata>(
        &self,
        program: String,
    ) -> api::Result<api::ParsedSourceFile<M>> {
        let (code, meta) = enso_parser::metadata::extract(&program);
        let tree = self.parser.run(code);
        Ok(api::ParsedSourceFile {
            ast:      ast::known::Module::try_from(
                translation::to_legacy_ast_module(&tree).unwrap(),
            )
            .unwrap(),
            // TODO: Log errors.
            metadata: meta.and_then(|meta| serde_json::from_str(meta).ok()).unwrap_or_default(),
        })
    }

    pub fn parse_module(&self, program: impl Str, ids: IdMap) -> api::Result<ast::known::Module> {
        let tree = self.parser.run(program.as_ref());
        let ast = translation::to_legacy_ast_module(&tree).unwrap();
        ast::known::Module::try_from(ast).map_err(|_| api::Error::NonModuleRoot)
    }
}


// === Convenience methods ===

impl Parser {
    pub fn parse_line_ast(&self, program: impl Str) -> FallibleResult<ast::Ast> {
        self.parse_line(program).map(|line| line.elem)
    }

    pub fn parse_line(&self, program: impl Str) -> FallibleResult<ast::BlockLine<ast::Ast>> {
        self.parse_line_with_id_map(program, default())
    }

    pub fn parse_line_ast_with_id_map(
        &self,
        program: impl Str,
        id_map: IdMap,
    ) -> FallibleResult<ast::Ast> {
        self.parse_line_with_id_map(program, id_map).map(|line| line.elem)
    }

    /// Program is expected to be single non-empty line module. Return the parsed line.
    fn parse_line_with_id_map(
        &self,
        program: impl Str,
        id_map: IdMap,
    ) -> FallibleResult<ast::BlockLine<ast::Ast>> {
        let module = self.parse_module(program, id_map)?;
        let mut lines =
            module.lines.clone().into_iter().filter_map(|line| line.map(|elem| elem).transpose());
        if let Some(first_non_empty_line) = lines.next() {
            if lines.next().is_some() {
                Err(api::TooManyLinesProduced.into())
            } else {
                Ok(first_non_empty_line)
            }
        } else {
            Err(api::NoLinesProduced.into())
        }
    }
}
