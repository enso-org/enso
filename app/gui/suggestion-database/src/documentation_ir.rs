//! The intermediate representation of the entry's documentation.
//!
//! [`EntryDocumentation`] contains all the necessary information to generate HTML
//! documentation of the specific entry. [`EntryDocumentation`] is created by aggregating
//! documentation of the entry, and also its children entries, such as methods of the type or types
//! defined in the module.

use crate::prelude::*;

use crate::entry;
use crate::entry::Argument;
use crate::entry::Kind;
use crate::Entry;
use crate::NoSuchEntry;
use crate::SuggestionDatabase;

use double_representation::name::QualifiedName;
use enso_doc_parser::DocSection;
use enso_doc_parser::Mark;
use std::cmp::Ordering;



// ==========================
// === EntryDocumentation ===
// ==========================

// === EntryDocumentation ===

/// The documentation of the specific entry.
#[derive(Debug, PartialEq, From, Clone, CloneRef)]
pub enum EntryDocumentation {
    /// No documentation available.
    Placeholder(Placeholder),
    /// Documentation for the entry.
    Docs(Documentation),
}

impl Default for EntryDocumentation {
    fn default() -> Self {
        Placeholder::NoDocumentation.into()
    }
}

impl EntryDocumentation {
    /// Constructor.
    pub fn new(db: &SuggestionDatabase, id: &entry::Id) -> Result<Self, NoSuchEntry> {
        let entry = db.lookup(*id);
        let result = match entry {
            Ok(entry) => match entry.kind {
                Kind::Type => {
                    let type_docs = TypeDocumentation::new(*id, &entry, db)?.into();
                    Documentation::Type(type_docs).into()
                }
                Kind::Module => {
                    let module_docs = ModuleDocumentation::new(*id, &entry, db)?.into();
                    Documentation::Module(module_docs).into()
                }
                Kind::Constructor => Self::constructor_docs(db, &entry)?,
                Kind::Method => Self::method_docs(db, &entry)?,
                Kind::Function => {
                    let function_docs = FunctionDocumentation::from_entry(&entry).into();
                    Documentation::Function(function_docs).into()
                }
                Kind::Local => {
                    let local_docs = LocalDocumentation::from_entry(&entry).into();
                    Documentation::Local(local_docs).into()
                }
            },
            Err(_) => {
                error!("No entry found for id: {id:?}");
                Self::Placeholder(Placeholder::NoDocumentation)
            }
        };
        Ok(result)
    }

    /// Qualified name of the function-like entry. See [`Documentation::function_name`].
    pub fn function_name(&self) -> Option<&QualifiedName> {
        match self {
            EntryDocumentation::Docs(docs) => docs.function_name(),
            _ => None,
        }
    }

    /// Create documentation for a hard-coded builtin entry.
    pub fn builtin(sections: impl IntoIterator<Item = &DocSection>) -> Self {
        let sections = Rc::new(BuiltinDocumentation::from_doc_sections(sections.into_iter()));
        Self::Docs(Documentation::Builtin(sections))
    }

    fn method_docs(
        db: &SuggestionDatabase,
        entry: &Entry,
    ) -> Result<EntryDocumentation, NoSuchEntry> {
        let self_type = match &entry.self_type {
            Some(self_type) => self_type,
            None => {
                error!("Method without self type: {}.", entry.qualified_name());
                return Ok(Placeholder::NoDocumentation.into());
            }
        };
        let return_type = db.lookup_by_qualified_name(self_type);
        match return_type {
            Some((id, parent)) => {
                let name = entry.qualified_name().into();
                match parent.kind {
                    Kind::Type => {
                        let type_docs = TypeDocumentation::new(id, &parent, db)?.into();
                        Ok(Documentation::Method { name, type_docs }.into())
                    }
                    Kind::Module => {
                        let module_docs = ModuleDocumentation::new(id, &parent, db)?;
                        let module_docs = module_docs.into();
                        Ok(Documentation::ModuleMethod { name, module_docs }.into())
                    }
                    _ => {
                        error!("Unexpected parent kind for method {}.", entry.qualified_name());
                        Ok(Placeholder::NoDocumentation.into())
                    }
                }
            }
            None => {
                error!("Parent entry for method {} not found.", entry.qualified_name());
                Ok(Self::Placeholder(Placeholder::NoDocumentation))
            }
        }
    }

    fn constructor_docs(
        db: &SuggestionDatabase,
        entry: &Entry,
    ) -> Result<EntryDocumentation, NoSuchEntry> {
        let return_type = &entry.return_type;
        let return_type = db.lookup_by_qualified_name(return_type);

        match return_type {
            Some((id, parent)) => {
                let name = entry.qualified_name().into();
                let type_docs = TypeDocumentation::new(id, &parent, db)?.into();
                Ok(Documentation::Constructor { name, type_docs }.into())
            }
            None => {
                error!("No return type found for constructor {}.", entry.qualified_name());
                Ok(Placeholder::NoDocumentation.into())
            }
        }
    }
}

// === Placeholder ===

/// No documentation is available for the entry.
#[derive(Debug, Clone, CloneRef, PartialEq)]
#[allow(missing_docs)]
pub enum Placeholder {
    /// Documentation is empty.
    NoDocumentation,
    /// Documentation for the Virtual Component group.
    VirtualComponentGroup { name: ImString },
}

// === Documentation ===

/// Documentation of the entry, split into variants for each entry kind.
#[derive(Debug, Clone, CloneRef, PartialEq)]
#[allow(missing_docs)]
pub enum Documentation {
    Module(Rc<ModuleDocumentation>),
    Type(Rc<TypeDocumentation>),
    Constructor { name: Rc<QualifiedName>, type_docs: Rc<TypeDocumentation> },
    Method { name: Rc<QualifiedName>, type_docs: Rc<TypeDocumentation> },
    ModuleMethod { name: Rc<QualifiedName>, module_docs: Rc<ModuleDocumentation> },
    Function(Rc<FunctionDocumentation>),
    Local(Rc<LocalDocumentation>),
    Builtin(Rc<BuiltinDocumentation>),
}

impl Documentation {
    /// Qualified name of the documented function. Functions are part of the documentation for
    /// the larger entity, e.g., constructor documentation is embedded into the type
    /// documentation. The returned qualified name is used to scroll to the corresponding section in
    /// a larger documentation page.
    pub fn function_name(&self) -> Option<&QualifiedName> {
        match self {
            Documentation::Constructor { name, .. } => Some(name),
            Documentation::Method { name, .. } => Some(name),
            Documentation::ModuleMethod { name, .. } => Some(name),
            _ => None,
        }
    }
}

// =========================
// === TypeDocumentation ===
// =========================

/// Documentation of the [`EntryKind::Type`] entries.
#[derive(Debug, Clone, CloneRef, PartialEq, Eq)]
#[allow(missing_docs)]
pub struct TypeDocumentation {
    pub name:         Rc<QualifiedName>,
    pub arguments:    Rc<Vec<Argument>>,
    pub tags:         Tags,
    pub synopsis:     Synopsis,
    pub constructors: Constructors,
    pub methods:      Methods,
    pub examples:     Examples,
}

impl PartialOrd for TypeDocumentation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.name.cmp(&other.name))
    }
}

impl Ord for TypeDocumentation {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl TypeDocumentation {
    /// Constructor.
    pub fn new(id: entry::Id, entry: &Entry, db: &SuggestionDatabase) -> Result<Self, NoSuchEntry> {
        let FilteredDocSections { tags, synopsis, examples } =
            FilteredDocSections::new(entry.documentation.iter());
        Ok(Self {
            name: entry.qualified_name().into(),
            arguments: entry.arguments.clone().into(),
            tags,
            synopsis,
            constructors: Constructors::of_type(id, db)?,
            methods: Methods::of_entry(id, db)?,
            examples,
        })
    }
}

// ===========================
// === ModuleDocumentation ===
// ===========================

/// Documentation of the [`EntryKind::Module`] entries.
#[derive(Debug, Clone, CloneRef, PartialEq)]
#[allow(missing_docs)]
pub struct ModuleDocumentation {
    pub name:     Rc<QualifiedName>,
    pub tags:     Tags,
    pub synopsis: Synopsis,
    pub types:    Types,
    pub methods:  Methods,
    pub examples: Examples,
}

impl ModuleDocumentation {
    /// Constructor.
    pub fn new(id: entry::Id, entry: &Entry, db: &SuggestionDatabase) -> Result<Self, NoSuchEntry> {
        let FilteredDocSections { tags, synopsis, examples } =
            FilteredDocSections::new(entry.documentation.iter());
        Ok(Self {
            name: entry.qualified_name().into(),
            tags,
            synopsis,
            types: Types::of_module(id, db)?,
            methods: Methods::of_entry(id, db)?,
            examples,
        })
    }
}



// =============================
// === FunctionDocumentation ===
// =============================

/// Documentation of the [`EntryKind::Function`] entries.
#[derive(Debug, Clone, CloneRef, PartialEq)]
#[allow(missing_docs)]
pub struct FunctionDocumentation {
    pub name:      Rc<QualifiedName>,
    pub arguments: Rc<Vec<Argument>>,
    pub tags:      Tags,
    pub synopsis:  Synopsis,
    pub examples:  Examples,
}

impl FunctionDocumentation {
    /// Constructor.
    pub fn from_entry(entry: &Entry) -> Self {
        let FilteredDocSections { tags, synopsis, examples } =
            FilteredDocSections::new(entry.documentation.iter());
        Self {
            name: entry.qualified_name().into(),
            arguments: entry.arguments.clone().into(),
            tags,
            synopsis,
            examples,
        }
    }
}



// ==========================
// === LocalDocumentation ===
// ==========================

/// Documentation of the [`EntryKind::Local`] entries.
#[derive(Debug, Clone, CloneRef, PartialEq)]
#[allow(missing_docs)]
pub struct LocalDocumentation {
    pub name:        Rc<QualifiedName>,
    pub tags:        Tags,
    pub return_type: Rc<QualifiedName>,
    pub synopsis:    Synopsis,
    pub examples:    Examples,
}

impl LocalDocumentation {
    /// Constructor.
    pub fn from_entry(entry: &Entry) -> Self {
        let FilteredDocSections { tags, synopsis, examples } =
            FilteredDocSections::new(entry.documentation.iter());
        Self {
            name: entry.qualified_name().into(),
            return_type: entry.return_type.clone().into(),
            tags,
            synopsis,
            examples,
        }
    }
}



// ============================
// === BuiltinDocumentation ===
// ============================

/// Documentation of hard-coded builtin entries.
#[derive(Debug, Clone, CloneRef, PartialEq)]
#[allow(missing_docs)]
pub struct BuiltinDocumentation {
    pub synopsis: Synopsis,
}

impl BuiltinDocumentation {
    /// Constructor.
    pub fn from_doc_sections(sections: impl IntoIterator<Item = &DocSection>) -> Self {
        let FilteredDocSections { tags, synopsis, examples } =
            FilteredDocSections::new(sections.into_iter());
        debug_assert!(tags.is_empty());
        debug_assert!(examples.is_empty());
        Self { synopsis }
    }
}



// ============
// === Tags ===
// ============

/// Tags attached to the entry. Corresponds to [`DocSection::Tagged`].
#[derive(Debug, Clone, CloneRef, PartialEq, Eq, Deref, Default)]
pub struct Tags {
    list: SortedVec<Tag>,
}

/// A single tag.
#[derive(Debug, Clone, CloneRef, PartialEq, Eq)]
pub struct Tag {
    /// Name of the tag.
    pub name: ImString,
    /// Optional tag value. Can be empty.
    pub body: ImString,
}

impl PartialOrd for Tag {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.name.cmp(&other.name))
    }
}

impl Ord for Tag {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl Tag {
    /// Constructor.
    pub fn new<N: Into<ImString>, B: Into<ImString>>(name: N, body: B) -> Self {
        Self { name: name.into(), body: body.into() }
    }
}

// ================
// === Synopsis ===
// ================

/// The most common portion of the documentation. A list of paragraphs. Can contain
/// [`DocSection::Paragraph`], [`DocSection::Keyed`] or [`DocSection::Marked`] (except for
/// examples) items. Examples and tags are handled separately (see [`Examples`] and [`Tags`]).
#[derive(Debug, Clone, CloneRef, Deref, PartialEq, Eq, Default)]
pub struct Synopsis {
    list: Rc<Vec<DocSection>>,
}

impl Synopsis {
    /// Constructor.
    pub fn from_doc_sections(sections: impl IntoIterator<Item = DocSection>) -> Self {
        let list = sections.into_iter().collect_vec();
        Self { list: Rc::new(list) }
    }
}

// =============
// === Types ===
// =============

/// A list of types defined in the module.
#[derive(Debug, Clone, CloneRef, PartialEq, Default, Deref)]
pub struct Types {
    list: SortedVec<TypeDocumentation>,
}

impl Types {
    /// Collect all types defined in the module.
    pub fn of_module(module_id: entry::Id, db: &SuggestionDatabase) -> Result<Self, NoSuchEntry> {
        let types = db.lookup_hierarchy(module_id)?;
        let lookup = |id: &entry::Id| {
            if let Ok(entry) = db.lookup(*id) {
                Some((*id, entry))
            } else {
                None
            }
        };
        let entries = types.iter().flat_map(lookup);
        let entries = entries.flat_map(|(id, entry)| TypeDocumentation::new(id, &entry, db).ok());
        Ok(Self { list: entries.collect_vec().into() })
    }
}

// =================
// === Functions ===
// =================

/// A list of functions. See [`Function`].
#[derive(Debug, Clone, CloneRef, Deref, PartialEq, Eq, Default)]
pub struct Functions {
    list: SortedVec<Function>,
}

impl Functions {
    /// Build list from the list of entries.
    pub fn from_entries(entries: impl Iterator<Item = Rc<Entry>>) -> Self {
        let methods = entries.map(|entry| Function::from_entry(&entry)).collect_vec();
        Self { list: methods.into() }
    }
}

// === Function ===

/// A documentation of a single function. It can be either a module function, or a type method, or a
/// type constructor. It corresponds to entries with kinds [`EntryKind::Function`],
/// [`EntryKind::Method`] and [`EntryKind::Constructor`].
#[derive(PartialEq, Debug, Clone, CloneRef, Eq)]
#[allow(missing_docs)]
pub struct Function {
    pub name:      Rc<QualifiedName>,
    pub tags:      Tags,
    pub arguments: Rc<Vec<Argument>>,
    pub synopsis:  Synopsis,
    pub examples:  Examples,
}

impl PartialOrd for Function {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.name.cmp(&other.name))
    }
}

impl Ord for Function {
    fn cmp(&self, other: &Self) -> Ordering {
        self.name.cmp(&other.name)
    }
}

impl Function {
    /// Constructor.
    pub fn from_entry(entry: &Entry) -> Self {
        let FilteredDocSections { tags, synopsis, examples } =
            FilteredDocSections::new(entry.documentation.iter());
        Self {
            name: entry.qualified_name().into(),
            tags,
            arguments: entry.arguments.clone().into(),
            synopsis,
            examples,
        }
    }
}

// === Methods ===

/// A list of methods defined for the type or module.
#[derive(Debug, Deref, PartialEq, Eq, Clone, CloneRef, Default, From)]
pub struct Methods(Functions);

impl Methods {
    /// Collect all the methods defined for the type.
    pub fn of_entry(entry_id: entry::Id, db: &SuggestionDatabase) -> Result<Self, NoSuchEntry> {
        let methods = db.lookup_hierarchy(entry_id)?;
        let entries = methods.iter().flat_map(|id| db.lookup(*id).ok());
        let functions = entries.filter(|entry| entry.kind == Kind::Method);
        let functions = Functions::from_entries(functions);
        Ok(functions.into())
    }
}

// === Constructors ===

/// A list of constructors defined for the type.
#[derive(Debug, Deref, PartialEq, Eq, Clone, CloneRef, Default, From)]
pub struct Constructors(Functions);

impl Constructors {
    /// Collect all the constructors defined for the type.
    pub fn of_type(entry_id: entry::Id, db: &SuggestionDatabase) -> Result<Self, NoSuchEntry> {
        let children = db.lookup_hierarchy(entry_id)?;
        let entries = children.iter().flat_map(|id| db.lookup(*id).ok());
        let constructors = entries.filter(|entry| entry.kind == Kind::Constructor);
        let methods = Functions::from_entries(constructors);
        Ok(Self(methods))
    }
}

// ================
// === Examples ===
// ================

/// Examples of the entry usage. Can contain only [`DocSection::Marked`] with [`Mark::Example`]
/// items.
#[derive(Debug, Clone, CloneRef, Default, PartialEq, Eq, Deref)]
pub struct Examples {
    list: Rc<Vec<DocSection>>,
}

impl Examples {
    /// Constructor.
    pub fn from_doc_sections(sections: impl IntoIterator<Item = DocSection>) -> Self {
        let is_example = |section: &DocSection| {
            matches!(section, DocSection::Marked { mark: Mark::Example, .. })
        };
        let examples = sections.into_iter().filter(is_example).collect_vec();
        Self { list: examples.into() }
    }
}

// =================
// === SortedVec ===
// =================

/// A vector that is always sorted. It is used to store documentation items (such as methods) in a
/// sorted order.
#[derive(Debug, Clone, CloneRef, Deref, PartialEq, Eq)]
#[clone_ref(bound = "T: Clone")]
pub struct SortedVec<T> {
    inner: Rc<Vec<T>>,
}

impl<T> Default for SortedVec<T> {
    fn default() -> Self {
        Self { inner: default() }
    }
}

impl<T: PartialOrd + Ord> SortedVec<T> {
    /// Constructor. Sorts the given items.
    pub fn new(items: impl IntoIterator<Item = T>) -> Self {
        let vec = items.into_iter().sorted().collect_vec().into();
        Self { inner: vec }
    }
}

impl<T: PartialOrd + Ord> From<Vec<T>> for SortedVec<T> {
    fn from(mut vec: Vec<T>) -> Self {
        vec.sort();
        Self { inner: vec.into() }
    }
}

impl<'a, T: Clone + PartialOrd + Ord> From<&'a [T]> for SortedVec<T> {
    fn from(items: &'a [T]) -> Self {
        SortedVec::new(items.iter().cloned())
    }
}

// ===========================
// === FilteredDocSections ===
// ===========================

/// Helper structure for splitting entry's [`DocSection`]s into [`Tags`], [`Synopsis`], and
/// [`Examples`].
struct FilteredDocSections {
    tags:     Tags,
    synopsis: Synopsis,
    examples: Examples,
}

impl FilteredDocSections {
    /// Constructor.
    pub fn new<'a>(doc_sections: impl Iterator<Item = &'a DocSection>) -> Self {
        let mut tags = Vec::new();
        let mut synopsis = Vec::new();
        let mut examples = Vec::new();
        for section in doc_sections {
            match section {
                DocSection::Tag { name, body } => tags.push(Tag::new(name, body)),
                DocSection::Marked { mark: Mark::Example, .. } => examples.push(section.clone()),
                section => synopsis.push(section.clone()),
            }
        }
        Self {
            tags:     Tags { list: SortedVec::new(tags) },
            synopsis: Synopsis { list: synopsis.into() },
            examples: Examples { list: examples.into() },
        }
    }
}


// =============
// === Tests ===
// =============

#[cfg(test)]
mod tests {
    use super::*;
    use crate::doc_section;
    use crate::mock_suggestion_database;
    use double_representation::name::QualifiedName;

    #[test]
    fn test_entry_documentation_not_found() {
        let db = mock_db();
        // Arbitrary non-existing entry id.
        let entry_id = 10;
        let docs = EntryDocumentation::new(&db, &entry_id).unwrap();
        assert_eq!(docs, EntryDocumentation::Placeholder(Placeholder::NoDocumentation));
    }

    fn assert_docs(db: &SuggestionDatabase, name: Rc<QualifiedName>, expected: Documentation) {
        let (entry_id, _) = db.lookup_by_qualified_name(&*name).unwrap();
        let docs = EntryDocumentation::new(db, &entry_id).unwrap();
        assert_eq!(EntryDocumentation::Docs(expected), docs);
    }

    #[test]
    fn test_documentation_of_constructor() {
        let db = mock_db();
        let name = Rc::new(QualifiedName::from_text("Standard.Base.A.Foo").unwrap());
        let type_docs = a_type().into();
        let expected = Documentation::Constructor { name: name.clone(), type_docs };
        assert_docs(&db, name, expected);
    }


    #[test]
    fn test_documentation_of_method() {
        let db = mock_db();

        // === Type method ===

        let name = Rc::new(QualifiedName::from_text("Standard.Base.A.baz").unwrap());
        let type_docs = a_type().into();
        let expected = Documentation::Method { name: name.clone(), type_docs };
        assert_docs(&db, name, expected);


        // === Module method ===

        let name = Rc::new(QualifiedName::from_text("Standard.Base.module_method").unwrap());
        let module_docs = module_docs().into();
        let expected = Documentation::ModuleMethod { name: name.clone(), module_docs };
        assert_docs(&db, name, expected);
    }

    #[test]
    fn test_documentation_of_module() {
        let db = mock_db();
        let expected = Documentation::Module(Rc::new(module_docs()));
        let name = Rc::new(QualifiedName::from_text("Standard.Base").unwrap());
        assert_docs(&db, name, expected);
    }

    #[test]
    fn test_documentation_of_type() {
        let db = mock_db();

        // === Type Standard.Base.A ===

        let expected = Documentation::Type(Rc::new(a_type()));
        let name = QualifiedName::from_text("Standard.Base.A").unwrap();
        let (entry_id, _) = db.lookup_by_qualified_name(&name).unwrap();
        let docs = EntryDocumentation::new(&db, &entry_id).unwrap();
        assert_eq!(EntryDocumentation::Docs(expected), docs);

        // === Type Standard.Base.B ===

        let expected = Documentation::Type(Rc::new(b_type()));
        let name = Rc::new(QualifiedName::from_text("Standard.Base.B").unwrap());
        assert_docs(&db, name, expected);
    }

    fn mock_db() -> SuggestionDatabase {
        mock_suggestion_database! {
            #[with_doc_section(doc_section!("Documentation of module."))]
            Standard.Base {
                #[with_doc_section(doc_section!("Documentation of type A."))]
                #[with_doc_section(doc_section!("Consists of multiple sections."))]
                #[with_doc_section(doc_section!(> "Example", "Example of type A usage."))]
                type A {
                    #[with_doc_section(doc_section!("Documentation of constructor A.Foo."))]
                    Foo (a);
                    #[with_doc_section(doc_section!("Documentation of constructor A.Bar."))]
                    Bar (b);

                    #[with_doc_section(doc_section!("Documentation of method A.baz."))]
                    #[with_doc_section(doc_section!(@ "Tag", "Tag body."))]
                    fn baz() -> Standard.Base.B;
                }

                #[with_doc_section(doc_section!("Documentation of type B."))]
                type B {
                    #[with_doc_section(doc_section!("Documentation of constructor B.New."))]
                    #[with_doc_section(doc_section!(@ "AnotherTag", "Tag body."))]
                    #[with_doc_section(doc_section!(! "Important", "Important note."))]
                    #[with_doc_section(doc_section!(> "Example", "Example of constructor B.New usage."))]
                    New;
                }

                #[with_doc_section(doc_section!("Documentation of module method."))]
                #[with_doc_section(doc_section!(@ "Deprecated", ""))]
                #[with_doc_section(doc_section!(> "Example", "Example of module method usage."))]
                fn module_method() -> Standard.Base.A;
            }
        }
    }

    fn module_docs() -> ModuleDocumentation {
        ModuleDocumentation {
            name:     QualifiedName::from_text("Standard.Base").unwrap().into(),
            tags:     default(),
            synopsis: Synopsis::from_doc_sections([doc_section!("Documentation of module.")]),
            types:    Types { list: SortedVec::new([a_type(), b_type()]) },
            methods:  Functions { list: SortedVec::new([module_method_function()]) }.into(),
            examples: default(),
        }
    }

    fn module_method_function() -> Function {
        Function {
            name:      QualifiedName::from_text("Standard.Base.module_method").unwrap().into(),
            tags:      Tags {
                list: SortedVec::new([Tag {
                    name: "Deprecated".to_im_string(),
                    body: "".to_im_string(),
                }]),
            },
            arguments: default(),
            synopsis:  Synopsis::from_doc_sections([doc_section!(
                "Documentation of module method."
            )]),
            examples:  Examples::from_doc_sections([doc_section!(
                > "Example", "Example of module method usage."
            )]),
        }
    }

    fn a_type() -> TypeDocumentation {
        TypeDocumentation {
            name:         QualifiedName::from_text("Standard.Base.A").unwrap().into(),
            arguments:    default(),
            tags:         default(),
            synopsis:     Synopsis::from_doc_sections([
                doc_section!("Documentation of type A."),
                doc_section!("Consists of multiple sections."),
            ]),
            constructors: Functions {
                list: SortedVec::new(vec![a_bar_constructor(), a_foo_constructor()]),
            }
            .into(),
            methods:      Methods(Functions { list: vec![a_baz_method()].into() }),
            examples:     Examples::from_doc_sections([doc_section!(
                > "Example", "Example of type A usage."
            )]),
        }
    }

    fn a_foo_constructor() -> Function {
        Function {
            name:      QualifiedName::from_text("Standard.Base.A.Foo").unwrap().into(),
            tags:      default(),
            arguments: vec![Argument::new("a", "Standard.Base.Any")].into(),
            synopsis:  Synopsis::from_doc_sections([doc_section!(
                "Documentation of constructor A.Foo."
            )]),
            examples:  default(),
        }
    }

    fn a_bar_constructor() -> Function {
        Function {
            name:      QualifiedName::from_text("Standard.Base.A.Bar").unwrap().into(),
            tags:      default(),
            arguments: vec![Argument::new("b", "Standard.Base.Any")].into(),
            synopsis:  Synopsis::from_doc_sections([doc_section!(
                "Documentation of constructor A.Bar."
            )]),
            examples:  default(),
        }
    }

    fn a_baz_method() -> Function {
        Function {
            name:      QualifiedName::from_text("Standard.Base.A.baz").unwrap().into(),
            tags:      Tags { list: vec![Tag::new("Tag", "Tag body.")].into() },
            arguments: default(),
            synopsis:  Synopsis::from_doc_sections([doc_section!(
                "Documentation of method A.baz."
            )]),
            examples:  default(),
        }
    }

    fn b_type() -> TypeDocumentation {
        TypeDocumentation {
            name:         QualifiedName::from_text("Standard.Base.B").unwrap().into(),
            arguments:    default(),
            tags:         default(),
            synopsis:     Synopsis::from_doc_sections([doc_section!("Documentation of type B.")]),
            constructors: Functions { list: SortedVec::new(vec![b_new_constructor()]) }.into(),
            methods:      default(),
            examples:     default(),
        }
    }

    fn b_new_constructor() -> Function {
        Function {
            name:      QualifiedName::from_text("Standard.Base.B.New").unwrap().into(),
            tags:      Tags { list: vec![Tag::new("AnotherTag", "Tag body.")].into() },
            arguments: default(),
            synopsis:  Synopsis::from_doc_sections([
                doc_section!("Documentation of constructor B.New."),
                doc_section!(!"Important", "Important note."),
            ]),
            examples:  Examples::from_doc_sections([doc_section!(
                > "Example", "Example of constructor B.New usage."
            )]),
        }
    }
}
