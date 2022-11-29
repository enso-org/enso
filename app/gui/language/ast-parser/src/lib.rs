// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]

use enso_prelude::*;

use ast::prelude::FallibleResult;


// ==============
// === Export ===
// ==============

pub use ast::IdMap;
pub use parser_scala::DocParser;

pub mod api {
    use ast::id_map::JsonIdMap;
    use ast::HasIdMap;
    use ast::HasRepr;
    use enso_prelude::*;
    use enso_text::index::*;
    use enso_text::unit::*;

    #[derive(Clone, Debug, Eq, PartialEq)]
    pub struct ParsedSourceFile<M> {
        /// Ast representation.
        pub ast:      ast::known::Module,
        /// Raw metadata in json.
        pub metadata: M,
    }

    impl<M: Metadata> ParsedSourceFile<M> {
        /// Serialize to the SourceFile structure,
        pub fn serialize(&self) -> std::result::Result<SourceFile, serde_json::Error> {
            const NEWLINES_BEFORE_TAG: usize = 3;
            const METADATA_TAG: &str = "#### METADATA ####";
            fn to_json_single_line(
                val: &impl serde::Serialize,
            ) -> std::result::Result<String, serde_json::Error> {
                let json = serde_json::to_string(val)?;
                let line = json.chars().filter(|c| *c != '\n' && *c != '\r').collect();
                Ok(line)
            }

            let code = self.ast.repr().into();
            let before_tag = "\n".repeat(NEWLINES_BEFORE_TAG);
            let before_idmap = "\n";
            let json_id_map = JsonIdMap::from_id_map(&self.ast.id_map(), &code);
            let id_map = to_json_single_line(&json_id_map)?;
            let before_metadata = "\n";
            let metadata = to_json_single_line(&self.metadata)?;

            let id_map_start =
                code.len().value + before_tag.len() + METADATA_TAG.len() + before_idmap.len();
            let id_map_start_bytes = Byte::from(id_map_start);
            let metadata_start = id_map_start + id_map.len() + before_metadata.len();
            let metadata_start_bytes = Byte::from(metadata_start);
            let content = format!(
                "{code}{before_tag}{METADATA_TAG}{before_idmap}{id_map}{before_metadata}{metadata}"
            );
            Ok(SourceFile {
                content,
                code:     (0.byte()..code.len().to_byte()).into(),
                id_map:   (id_map_start_bytes..id_map_start_bytes + ByteDiff::from(id_map.len()))
                    .into(),
                metadata: (metadata_start_bytes
                    ..metadata_start_bytes + ByteDiff::from(metadata.len()))
                    .into(),
            })
        }
    }

    impl<M: Metadata> Display for ParsedSourceFile<M> {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self.serialize() {
                Ok(serialized) => write!(f, "{}", serialized),
                Err(_) => write!(f, "[UNREPRESENTABLE SOURCE FILE]"),
            }
        }
    }

    impl<M> From<parser_scala::api::ParsedSourceFile<M>> for ParsedSourceFile<M> {
        fn from(value: parser_scala::api::ParsedSourceFile<M>) -> Self {
            let ast = value.ast.into();
            let metadata = value.metadata;
            Self { ast, metadata }
        }
    }


    // === TODO: Extract these from [`parser_scala`] when the rest of the library is eliminated. ===
    pub use parser_scala::api::Metadata;
    pub use parser_scala::api::PruneUnusedIds;
    pub use parser_scala::api::Result;
    pub use parser_scala::api::SourceFile;
}



#[derive(Debug, Clone, CloneRef)]
pub struct Parser {
    scala_parser: parser_scala::Parser,
}


// === Core methods provided by the underlying parser ===

impl Parser {
    pub fn new() -> api::Result<Self> {
        parser_scala::Parser::new().map(|scala_parser| Self { scala_parser })
    }

    pub fn parse(&self, program: String, ids: IdMap) -> api::Result<ast::Ast> {
        self.scala_parser.parse(program, ids)
    }

    pub fn parse_with_metadata<M: api::Metadata>(
        &self,
        program: String,
    ) -> api::Result<api::ParsedSourceFile<M>> {
        self.scala_parser
            .parse_with_metadata(program)
            .map(Into::into)
    }
}


// === Convenience methods ===

impl Parser {
    pub fn new_or_panic() -> Self {
        Self::new().unwrap_or_else(|e| panic!("Failed to create a parser: {:?}", e))
    }

    pub fn parse_module(&self, program: impl Str, ids: IdMap) -> api::Result<ast::known::Module> {
        let ast = self.parse(program.into(), ids)?;
        ast::known::Module::try_from(ast).map_err(|_| parser_scala::api::Error::NonModuleRoot)
    }

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
                Err(parser_scala::api::TooManyLinesProduced.into())
            } else {
                Ok(first_non_empty_line)
            }
        } else {
            Err(parser_scala::api::NoLinesProduced.into())
        }
    }
}
