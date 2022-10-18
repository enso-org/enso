use crate::prelude::*;

use crate::module;
use ast::known;
use ast::Ast;
use ast::HasRepr;
use serde::Deserialize;
use serde::Serialize;


const LIST_SEPARATOR: char = ',';
pub const ALIAS_KEYWORD: &str = "as";
pub const ALL_KEYWORD: &str = "all";
pub const HIDING_KEYWORD: &str = "hiding";

/// Id for an import.
pub type Id = u64;

#[derive(Clone, Debug, Eq, Deserialize, Hash, Ord, PartialEq, PartialOrd, Serialize)]
pub enum ImportedScope {
    /// `import <module> [as <alias>]`
    Module { alias: Option<String> },
    /// `from <module> import all`
    All,
    /// `from <module> import all hiding <hidden_names>`
    AllHidingNames { hidden_names: Vec<String> },
    /// `from <module> import <names>`
    Names { names: Vec<String> },
}

impl ImportedScope {
    fn from_match_second_segment(segment: impl AsRef<str>) -> Self {
        let is_token_sep = |c: char| c.is_ascii_whitespace() || c == LIST_SEPARATOR;
        let scope_split = segment.as_ref().split(is_token_sep);
        let mut scope_tokens = scope_split.filter(|tok| !tok.is_empty());
        let first_token = scope_tokens.next();
        let second_token = scope_tokens.next();
        let third_and_further_tokens = scope_tokens;
        match (first_token, second_token) {
            (Some("all"), Some("hiding")) => Self::AllHidingNames {
                hidden_names: third_and_further_tokens.map(Into::into).collect(),
            },
            (Some("all"), _) => Self::All,
            (first_name, second_name) => Self::Names {
                names: first_name
                    .into_iter()
                    .chain(second_name)
                    .chain(third_and_further_tokens)
                    .map(Into::into)
                    .collect(),
            },
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
    pub imported: ImportedScope,
}

impl Info {
    pub fn new_qualified(module: &module::QualifiedName) -> Self {
        Self {
            module:   module.segments().map(|segment| segment.to_string()).collect(),
            imported: ImportedScope::Module { alias: None },
        }
    }

    /// Obtain the qualified name of the imported module.
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
                //     fixed with the new parser.
                imported: ImportedScope::Module { alias: None },
            })
        } else if ast::macros::is_match_unqualified_import(&ast) {
            let module = ast.segs.head.body.repr();
            let imported = ast.segs.tail.first().map_or_default(|s| s.body.repr());
            Some(Self::from_module_and_scope_str(module, imported))
        } else {
            None
        }
    }

    pub fn from_module_and_scope_str(module: impl AsRef<str>, imported: impl AsRef<str>) -> Self {
        Self {
            module:   Self::module_name_from_str(module),
            imported: ImportedScope::from_match_second_segment(imported),
        }
    }

    pub fn module_name_from_str(module: impl AsRef<str>) -> Vec<String> {
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
            ImportedScope::Module { alias } => {
                write!(f, "{} {}", import_kw, module)?;
                if let Some(alias) = alias {
                    write!(f, " {} {}", ALIAS_KEYWORD, alias)?;
                }
                Ok(())
            }
            ImportedScope::All => write!(f, "{} {} {} {}", from_kw, module, import_kw, ALL_KEYWORD),
            ImportedScope::Names { names } => {
                let names = names.join(", ");
                write!(f, "{} {} {} {}", from_kw, module, import_kw, names)
            }
            ImportedScope::AllHidingNames { hidden_names } => {
                let names = hidden_names.join(", ");
                write!(
                    f,
                    "{} {} {} {} {} {}",
                    from_kw, module, import_kw, ALL_KEYWORD, HIDING_KEYWORD, names
                )
            }
        }
    }
}
