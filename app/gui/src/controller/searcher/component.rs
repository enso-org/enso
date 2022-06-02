//! A module containing definition of [`Component`] and its [`List`]
//!
//! Component is a language entity displayed in the Component Browser.

use crate::prelude::*;

use crate::model::suggestion_database;


// ==============
// === Export ===
// ==============

pub mod builder;
pub mod group;

pub use group::Group;



// ====================
// === Type Aliases ===
// ====================

/// A component identifier.
pub type Id = suggestion_database::entry::Id;
/// Information how the component matches the filtering pattern.
pub type MatchInfo = controller::searcher::action::MatchInfo;


// =================
// === Component ===
// =================

/// A structure describing a language entity which may be picked in the Component Browser panel to
/// create a new node or change existing one.
///
/// The components are usually stored in [`List`], which may be filtered; the single component keeps
/// then information how it matches the current filtering pattern.
///
/// The component corresponds to some Suggestion Database Entry, and the entry will be used to
/// properly insert code into the program.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct Component {
    pub id:         Immutable<Id>,
    pub suggestion: Rc<suggestion_database::Entry>,
    pub match_info: Rc<RefCell<MatchInfo>>,
}

impl Component {
    /// Construct a new component.
    ///
    /// The matching info will be filled for an empty pattern.
    pub fn new(id: Id, suggestion: Rc<suggestion_database::Entry>) -> Self {
        Self { id: Immutable(id), suggestion, match_info: default() }
    }

    /// The label which should be displayed in the Component Browser.
    pub fn label(&self) -> &str {
        &self.suggestion.name
    }

    /// Checks if component is filtered out.
    pub fn is_filtered_out(&self) -> bool {
        matches!(*self.match_info.borrow(), MatchInfo::DoesNotMatch)
    }

    /// Checks if the component can be entered in Component Browser.
    ///
    /// Currently, only modules can be entered, and then the Browser should display content and
    /// submodules of the entered module.
    pub fn can_be_entered(&self) -> bool {
        self.suggestion.kind == suggestion_database::entry::Kind::Module
    }

    /// Update matching info.
    ///
    /// It should be called each time the filtering pattern changes.
    pub fn update_matching_info(&self, pattern: impl Str) {
        let label = self.label();
        let matches = fuzzly::matches(label, pattern.as_ref());
        let subsequence = matches.and_option_from(|| {
            let metric = fuzzly::metric::default();
            fuzzly::find_best_subsequence(label, pattern, metric)
        });
        *self.match_info.borrow_mut() = match subsequence {
            Some(subsequence) => MatchInfo::Matches { subsequence },
            None => MatchInfo::DoesNotMatch,
        };
    }
}



// ============
// === List ===
// ============

// === ModuleGroups ===

/// The Component groups associated with a module: the group containing the module's content, and
/// groups with direct submodules' content.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct ModuleGroups {
    pub content:    Group,
    pub submodules: group::List,
}


// === List ===

/// The Component List.
///
/// The List is used to provide information to Component Browser Panel View: thus the components
/// are arranged in appropriate groups, and the groups are ordered accordingly. You may check the
/// [design doc](https://github.com/enso-org/design/blob/main/epics/component-browser/design.md#overview)
/// for information how the Component Browser works.
///
/// The components and their structure are immutable, only the filtering may change in created List.
/// If there is need to change/extend the component list, a new one should be created using
/// [`builder::List`].
#[derive(Clone, CloneRef, Debug, Default)]
pub struct List {
    all_components:        Rc<Vec<Component>>,
    top_modules:           group::List,
    top_modules_flattened: group::List,
    module_groups:         Rc<HashMap<Id, ModuleGroups>>,
    filtered:              Rc<Cell<bool>>,
}

impl List {
    /// Create a list containing all entities available in the [`model::SuggestionDatabase`].
    pub fn build_list_from_all_db_entries(suggestion_db: &Rc<model::SuggestionDatabase>) -> List {
        let mut builder = builder::List::new(suggestion_db.clone_ref());
        builder.extend(suggestion_db.keys());
        builder.build()
    }

    /// Return the list of top modules, which should be displayed in Component Browser.
    ///
    /// If the list is filtered, all top modules will be flattened.
    pub fn top_modules(&self) -> &group::List {
        if self.filtered.get() {
            &self.top_modules_flattened
        } else {
            &self.top_modules
        }
    }

    /// Get the list of given component submodules. Returns [`None`] if given component is not
    /// a module.
    pub fn submodules_of(&self, component: Id) -> Option<&group::List> {
        self.module_groups.get(&component).map(|mg| &mg.submodules)
    }

    /// Update matching info in all components according to the new filtering pattern.
    pub fn update_filtering(&self, pattern: impl AsRef<str>) {
        let pattern = pattern.as_ref();
        for component in &*self.all_components {
            component.update_matching_info(pattern)
        }
        for group in self.all_groups() {
            group.update_sorting(pattern);
        }
        self.filtered.set(!pattern.is_empty());
    }

    fn all_groups(&self) -> impl Iterator<Item = &Group> {
        let normal = self.module_groups.values().map(|mg| &mg.content);
        let flattened = self.top_modules_flattened.iter();
        normal.chain(flattened)
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
pub(crate) mod tests {
    use super::*;

    use crate::model::suggestion_database::entry::Kind;

    use double_representation::module;
    use engine_protocol::language_server;


    // === Helpers ===

    pub fn mock_module(name: &str) -> model::suggestion_database::Entry {
        let ls_entry = language_server::SuggestionEntry::Module {
            module:             name.to_owned(),
            documentation:      default(),
            documentation_html: default(),
            reexport:           default(),
        };
        model::suggestion_database::Entry::from_ls_entry(ls_entry).unwrap()
    }

    pub fn mock_function(
        module: &module::QualifiedName,
        name: &str,
    ) -> model::suggestion_database::Entry {
        model::suggestion_database::Entry {
            kind:               Kind::Function,
            module:             module.clone(),
            name:               name.to_owned(),
            arguments:          vec![],
            return_type:        "Standard.Builtin.Integer".to_string(),
            documentation_html: None,
            self_type:          None,
            scope:              model::suggestion_database::entry::Scope::Everywhere,
        }
    }


    // === Filtering Component List ===

    #[test]
    fn filtering_component_list() {
        let logger = Logger::new("test::update_list_after_filtering_pattern_change");
        let top_module = mock_module("test.Test.TopModule");
        let sub_module = mock_module("test.Test.TopModule.SubModule");
        let fun1 = mock_function(&top_module.module, "fun1");
        let funx2 = mock_function(&sub_module.module, "funx1");
        let all_entries = [top_module, sub_module, fun1, funx2];
        let suggestion_db = model::SuggestionDatabase::new_empty(logger);
        for (id, entry) in all_entries.into_iter().enumerate() {
            suggestion_db.put_entry(id, entry)
        }
        let mut builder = builder::List::new(Rc::new(suggestion_db));
        builder.extend(0..4);
        let list = builder.build();
        let get_entries_ids =
            || list.top_modules()[0].entries.borrow().iter().map(|c| *c.id).collect_vec();

        list.update_filtering("fu");
        let expected_ids = vec![2, 3, 1];
        assert_eq!(get_entries_ids(), expected_ids);
        assert!(list.top_modules()[0].visible.get());

        list.update_filtering("x");
        let expected_ids = vec![3, 2, 1];
        assert_eq!(get_entries_ids(), expected_ids);
        assert!(list.top_modules()[0].visible.get());

        list.update_filtering("Sub");
        let expected_ids = vec![1, 3, 2];
        assert_eq!(get_entries_ids(), expected_ids);
        assert!(list.top_modules()[0].visible.get());

        list.update_filtering("y");
        assert!(!list.top_modules()[0].visible.get());
    }
}
