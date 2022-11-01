//! A module with utilities managing imports.
use crate::prelude::*;

use crate::module;

use ast::known;
use ast::Ast;
use ast::HasRepr;
use serde::Deserialize;
use serde::Serialize;
use std::collections::BTreeSet;



// =================
// === Constants ===
// =================

const LIST_SEPARATOR: char = ',';
const ALIAS_KEYWORD: &str = "as";
const ALL_KEYWORD: &str = "all";
const HIDING_KEYWORD: &str = "hiding";



// ===============
// === Aliases ===
// ===============

/// Id for an import.
pub type Id = u64;



// =====================
// === ImportedNames ===
// =====================

/// A structure describing what names are imported from the module in a specific import declaration.
#[allow(missing_docs)]
#[derive(Clone, Debug, Eq, Deserialize, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum ImportedNames {
    /// The import is `import <module> [as <alias>]` and only module name is imported.
    Module { alias: Option<String> },
    /// The import is `from <module> import all`, and all names defined in the module are imported.
    All,
    /// The import is `from <module> import all hiding <not_imported>`, and all names except
    /// specified in `not_imported` list are imported
    AllExcept { not_imported: BTreeSet<String> },
    /// The import is `from <module> import <names>`, and only the specified `names` are imported.
    List { names: BTreeSet<String> },
}

impl ImportedNames {
    /// Create [`ImportedNames`] structure from the second `Match` segment body.
    ///
    /// The unqualified imports are always parsed as [`Match`](crate::Shape::Match) AST node, where
    /// the second segment starts from `import` and ends with end of the import declaration. Thus,
    /// the second segment body may be `all`, `all hiding <comma-separated-name-list>`, or just
    /// comma separated name list.
    fn from_unqualified_import_match_second_segment(segment: impl AsRef<str>) -> Self {
        let is_token_sep = |c: char| c.is_ascii_whitespace() || c == LIST_SEPARATOR;
        let scope_split = segment.as_ref().split(is_token_sep);
        let mut scope_tokens = scope_split.filter(|tok| !tok.is_empty());
        let first_token = scope_tokens.next();
        let second_token = scope_tokens.next();
        let third_and_further_tokens = scope_tokens;
        match (first_token, second_token) {
            (Some("all"), Some("hiding")) =>
                Self::AllExcept { not_imported: third_and_further_tokens.map(Into::into).collect() },
            (Some("all"), _) => Self::All,
            (first_name, second_name) => {
                let all_names =
                    first_name.into_iter().chain(second_name).chain(third_and_further_tokens);
                Self::List { names: all_names.map(Into::into).collect() }
            }
        }
    }
}

/// Representation of a single import declaration.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Deserialize, Serialize, Hash)]
pub struct Info {
    /// The segments of the qualified name of the imported module.
    ///
    /// This field is not Qualified name to cover semantically illegal imports that are possible to
    /// be typed in and are representable in the text.
    /// This includes targets with too few segments or segments not being valid referent names.
    pub module:   Vec<String>,
    /// Imported names from [`module`].
    pub imported: ImportedNames,
}

impl Info {
    /// Create qualified import (i.e. `import <module-name>`) importing the given module without
    /// alias.
    pub fn new_qualified(module: &module::QualifiedName) -> Self {
        Self {
            module:   module.segments().map(|segment| segment.to_string()).collect(),
            imported: ImportedNames::Module { alias: None },
        }
    }

    /// Obtain the qualified name of the module.
    pub fn qualified_module_name(&self) -> FallibleResult<module::QualifiedName> {
        module::QualifiedName::from_all_segments(&self.module)
    }

    /// Construct from an AST. Fails if the Ast is not an import declaration.
    pub fn from_ast(ast: &Ast) -> Option<Self> {
        let macro_match = known::Match::try_from(ast).ok()?;
        Self::from_match(macro_match)
    }

    /// Construct from a macro match AST. Fails if the Ast is not an import declaration.
    pub fn from_match(ast: known::Match) -> Option<Self> {
        if ast::macros::is_match_qualified_import(&ast) {
            Some(Self {
                module:   Self::module_name_from_str(ast.segs.head.body.repr()),
                // TODO[ao] the current parser does not recognize aliases for imports. Should be
                //     fixed with the new parser. Once new parser will be integrated, the alias
                //     support will be implemented as task
                //     https://www.pivotaltracker.com/story/show/183590537
                imported: ImportedNames::Module { alias: None },
            })
        } else if ast::macros::is_match_unqualified_import(&ast) {
            let module = ast.segs.head.body.repr();
            let imported = ast.segs.tail.first().map_or_default(|s| s.body.repr());
            Some(Self::from_module_and_scope_str(module, imported))
        } else {
            None
        }
    }

    /// Create [`Info`] from unqualified import segment's body representations.
    ///
    /// The unqualified imports are always parsed as [`Match`](crate::Shape::Match) AST node, where
    /// the first segment contains keyword `from` and module name, and second segment the rest of
    /// the import.
    fn from_module_and_scope_str(module: impl AsRef<str>, imported: impl AsRef<str>) -> Self {
        Self {
            module:   Self::module_name_from_str(module),
            imported: ImportedNames::from_unqualified_import_match_second_segment(imported),
        }
    }

    fn module_name_from_str(module: impl AsRef<str>) -> Vec<String> {
        let name = module.as_ref().trim();
        if name.is_empty() {
            Vec::new()
        } else {
            let segments = name.split(ast::opr::predefined::ACCESS);
            let trimmed = segments.map(str::trim);
            trimmed.map(Into::into).collect()
        }
    }

    /// Return the ID of the import.
    ///
    /// The ID is based on a hash of the qualified name of the imported target. This ID is GUI
    /// internal and not known in the engine.
    pub fn id(&self) -> Id {
        let mut hasher = DefaultHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }
}

impl Display for Info {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let module = self.module.join(ast::opr::predefined::ACCESS);
        let import_kw = ast::macros::QUALIFIED_IMPORT_KEYWORD;
        let from_kw = ast::macros::UNQUALIFIED_IMPORT_KEYWORD;
        match &self.imported {
            ImportedNames::Module { alias } => {
                write!(f, "{} {}", import_kw, module)?;
                if let Some(alias) = alias {
                    write!(f, " {} {}", ALIAS_KEYWORD, alias)?;
                }
                Ok(())
            }
            ImportedNames::All => write!(f, "{} {} {} {}", from_kw, module, import_kw, ALL_KEYWORD),
            ImportedNames::List { names } => {
                let names = names.iter().join(", ");
                write!(f, "{} {} {} {}", from_kw, module, import_kw, names)
            }
            ImportedNames::AllExcept { not_imported: hidden_names } => {
                let names = hidden_names.iter().join(", ");
                write!(
                    f,
                    "{} {} {} {} {} {}",
                    from_kw, module, import_kw, ALL_KEYWORD, HIDING_KEYWORD, names
                )
            }
        }
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use parser_scala::Parser;

    struct Fixture {
        parser: Parser,
    }

    impl Fixture {
        fn new() -> Self {
            Self { parser: Parser::new_or_panic() }
        }

        fn run_case(&self, code: &str, expected: Info) {
            let ast = self.parser.parse_line_ast(code).expect("Parsing import declaration failed");
            let info = Info::from_ast(&ast);
            assert_eq!(info, Some(expected));
        }
    }

    #[wasm_bindgen_test]
    fn qualified_import_info_from_ast() {
        let test = Fixture::new();
        let make_info = |module: &[&str]| Info {
            module:   module.iter().map(|&s| s.to_owned()).collect(),
            imported: ImportedNames::Module { alias: None },
        };

        let normal_case = "import Standard.Base.Data";
        let normal_case_expected = make_info(&["Standard", "Base", "Data"]);
        test.run_case(normal_case, normal_case_expected);

        let weird_spaces = "import   Standard  .Base .   Data   ";
        let weird_spaces_expected = make_info(&["Standard", "Base", "Data"]);
        test.run_case(weird_spaces, weird_spaces_expected);

        let single_segment = "import local";
        let single_segment_expected = make_info(&["local"]);
        test.run_case(single_segment, single_segment_expected);
    }

    #[wasm_bindgen_test]
    fn unrestricted_import_info_from_ast() {
        let test = Fixture::new();
        let make_info = |module: &[&str]| Info {
            module:   module.iter().map(|&s| s.to_owned()).collect(),
            imported: ImportedNames::All,
        };

        let normal_case = "from Standard.Base import all";
        let normal_case_expected = make_info(&["Standard", "Base"]);
        test.run_case(normal_case, normal_case_expected);

        let weird_spaces = "from   Standard  . Base import   all  ";
        let weird_spaces_expected = make_info(&["Standard", "Base"]);
        test.run_case(weird_spaces, weird_spaces_expected);
    }

    #[wasm_bindgen_test]
    fn restricted_import_info_from_ast() {
        let test = Fixture::new();
        let make_info = |module: &[&str], names: &[&str]| Info {
            module:   module.iter().map(|&s| s.to_owned()).collect(),
            imported: ImportedNames::List { names: names.iter().map(|&s| s.to_owned()).collect() },
        };

        let normal_case = "from Standard.Base import Foo, Bar";
        let normal_case_expected = make_info(&["Standard", "Base"], &["Foo", "Bar"]);
        test.run_case(normal_case, normal_case_expected);

        let weird_spaces = "from   Standard  . Base import  Foo ,  Bar ,Buz";
        let weird_spaces_expected = make_info(&["Standard", "Base"], &["Foo", "Bar", "Buz"]);
        test.run_case(weird_spaces, weird_spaces_expected);

        let single_name = "from Standard.Base import Foo";
        let single_name_expected = make_info(&["Standard", "Base"], &["Foo"]);
        test.run_case(single_name, single_name_expected);
    }

    #[wasm_bindgen_test]
    fn hiding_import_info_from_ast() {
        let test = Fixture::new();
        let make_info = |module: &[&str], hidden_names: &[&str]| Info {
            module:   module.iter().map(|&s| s.to_owned()).collect(),
            imported: ImportedNames::AllExcept {
                not_imported: hidden_names.iter().map(|&s| s.to_owned()).collect(),
            },
        };

        let normal_case = "from Standard.Base import all hiding Foo, Bar";
        let normal_case_expected = make_info(&["Standard", "Base"], &["Foo", "Bar"]);
        test.run_case(normal_case, normal_case_expected);

        let weird_spaces = "from   Standard  . Base import  all  hiding  Foo ,  Bar ,Buz";
        let weird_spaces_expected = make_info(&["Standard", "Base"], &["Foo", "Bar", "Buz"]);
        test.run_case(weird_spaces, weird_spaces_expected);

        let single_name = "from Standard.Base import all hiding Foo";
        let single_name_expected = make_info(&["Standard", "Base"], &["Foo"]);
        test.run_case(single_name, single_name_expected);
    }
}
