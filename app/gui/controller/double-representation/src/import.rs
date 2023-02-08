//! A module with utilities managing imports.

use crate::prelude::*;

use crate::name::NamePath;
use crate::name::QualifiedName;

use ast::Ast;
use std::collections::BTreeSet;



// =================
// === Constants ===
// =================

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
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
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

/// Representation of a single import declaration.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd, Hash)]
pub struct Info {
    /// The path of the qualified name of the imported module.
    pub module:   NamePath,
    /// Imported names from [`module`].
    pub imported: ImportedNames,
}

impl Info {
    /// Create qualified import (i.e. `import <module-name>`) importing the given module without
    /// alias.
    pub fn new_qualified(module: impl Into<NamePath>) -> Self {
        Self { module: module.into(), imported: ImportedNames::Module { alias: None } }
    }

    /// Create a unqualified import importing one name from given module (i. e. `from <module-name>
    /// import <name>`).
    pub fn new_single_name(module: impl Into<NamePath>, name: impl Into<String>) -> Self {
        Self {
            module:   module.into(),
            imported: ImportedNames::List { names: [name.into()].into() },
        }
    }

    /// Obtain the qualified name of the module.
    pub fn qualified_module_name(&self) -> FallibleResult<QualifiedName> {
        QualifiedName::from_all_segments(&self.module)
    }

    /// Construct from an AST, if the Ast is an import declaration.
    pub fn from_ast(ast: &Ast) -> Option<Self> {
        if let ast::Shape::Tree(ast::Tree {
            type_info: ast::TreeType::Import { module, imported },
            ..
        }) = ast.shape()
        {
            let module = module.clone();
            let imported = match imported.clone() {
                ast::ImportedNames::All { except } if except.is_empty() => ImportedNames::All,
                ast::ImportedNames::All { except } =>
                    ImportedNames::AllExcept { not_imported: except },
                ast::ImportedNames::List { names } => ImportedNames::List { names },
                ast::ImportedNames::Module { alias } => ImportedNames::Module { alias },
            };
            Some(Info { module, imported })
        } else {
            None
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

    /// Return the module path as [`QualifiedName`]. Returns [`Err`] if the path is not a valid
    /// module name.
    pub fn module_qualified_name(&self) -> FallibleResult<QualifiedName> {
        QualifiedName::from_all_segments(self.module.iter())
    }
}

impl Display for Info {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let module = self.module.iter().map(ImString::as_str).join(ast::opr::predefined::ACCESS);
        let import_kw = ast::macros::QUALIFIED_IMPORT_KEYWORD;
        let from_kw = ast::macros::UNQUALIFIED_IMPORT_KEYWORD;
        match &self.imported {
            ImportedNames::Module { alias } => {
                write!(f, "{import_kw} {module}")?;
                if let Some(alias) = alias {
                    write!(f, " {ALIAS_KEYWORD} {alias}")?;
                }
                Ok(())
            }
            ImportedNames::All => write!(f, "{from_kw} {module} {import_kw} {ALL_KEYWORD}"),
            ImportedNames::List { names } => {
                let names = names.iter().join(", ");
                write!(f, "{from_kw} {module} {import_kw} {names}")
            }
            ImportedNames::AllExcept { not_imported: hidden_names } => {
                let names = hidden_names.iter().join(", ");
                write!(f, "{from_kw} {module} {import_kw} {ALL_KEYWORD} {HIDING_KEYWORD} {names}")
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
    use parser::Parser;

    struct Fixture {
        parser: Parser,
    }

    impl Fixture {
        fn new() -> Self {
            Self { parser: Parser::new() }
        }

        fn run_case(&self, code: &str, expected: Info) {
            let ast = self.parser.parse_line_ast(code).expect("Parsing import declaration failed");
            let info = Info::from_ast(&ast);
            assert_eq!(info, Some(expected));
        }
    }

    #[test]
    fn qualified_import_info_from_ast() {
        let test = Fixture::new();
        let make_info = |module: &[&str]| Info {
            module:   module.iter().map(|&s| ImString::new(s)).collect(),
            imported: ImportedNames::Module { alias: None },
        };

        let normal_case = "import Standard.Base.Data";
        let normal_case_expected = make_info(&["Standard", "Base", "Data"]);
        test.run_case(normal_case, normal_case_expected);

        let single_segment = "import local";
        let single_segment_expected = make_info(&["local"]);
        test.run_case(single_segment, single_segment_expected);
    }

    #[test]
    fn unrestricted_import_info_from_ast() {
        let test = Fixture::new();
        let make_info = |module: &[&str]| Info {
            module:   module.iter().map(|&s| ImString::new(s)).collect(),
            imported: ImportedNames::All,
        };

        let normal_case = "from Standard.Base import all";
        let normal_case_expected = make_info(&["Standard", "Base"]);
        test.run_case(normal_case, normal_case_expected);
    }

    #[test]
    fn restricted_import_info_from_ast() {
        let test = Fixture::new();
        let make_info = |module: &[&str], names: &[&str]| Info {
            module:   module.iter().map(|&s| ImString::new(s)).collect(),
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

    #[test]
    fn hiding_import_info_from_ast() {
        let test = Fixture::new();
        let make_info = |module: &[&str], hidden_names: &[&str]| Info {
            module:   module.iter().map(|&s| ImString::new(s)).collect(),
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
