//! A module containing the hard-coded definitions displayed in Searcher. The main function to use
//! is [`add_hardcoded_entries_to_list`] which adds the entries to given [`ListBuilder`].

use crate::prelude::*;

use crate::controller::searcher::action;
use crate::controller::searcher::action::ListBuilder;
use crate::model::module::MethodId;

use double_representation::module;
use double_representation::tp;



// =============
// === Icons ===
// =============

/// A structure serving as a type for [`ICONS`] constant, containing a set of hardcoded icon names.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug, Default)]
pub struct Icons {
    pub search_result: ImString,
    pub data_science:  ImString,
    pub input_output:  ImString,
    pub text:          ImString,
    pub number_input:  ImString,
    pub text_input:    ImString,
    pub data_input:    ImString,
    pub libraries:     ImString,
    pub default:       ImString,
}

thread_local! {
    /// A set of hardcoded icon names, to be used when creating hardcoded categories and actions.
    pub static ICONS:Icons = Icons {
        search_result : ImString::new("search_result"),
        data_science  : ImString::new("data_science"),
        input_output  : ImString::new("io"),
        text          : ImString::new("text"),
        number_input  : ImString::new("number_input"),
        text_input    : ImString::new("text_input"),
        data_input    : ImString::new("data_input"),
        libraries     : ImString::new("libraries"),
        default       : ImString::new("default"),
    };
}



// ===================
// === Definitions ===
// ===================

// === RootCategory ===

/// The hardcoded root category.
///
/// The structure is used solely for defining hierarchy of hard-coded suggestions. Based in this
/// hierarchy, the [`add_hardcoded_entries_to_list`] will add analogous [`action::RootCategory`]
/// to the built list.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct RootCategory {
    pub name:       &'static str,
    pub icon:       ImString,
    pub categories: Vec<Subcategory>,
}


// === Category ===

/// The hardcoded second-tier category.
///
/// The structure is used solely for defining hierarchy of hard-coded suggestions. Based in this
/// hierarchy, the [`add_hardcoded_entries_to_list`] will add analogous [`action::Category`]
/// to the built list.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub struct Subcategory {
    pub name:        &'static str,
    pub icon:        ImString,
    pub suggestions: Vec<Rc<Suggestion>>,
}


// === Suggestion ===

/// The hardcoded suggestion.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Suggestion {
    /// The name displayed in the Searcher.
    pub name:               &'static str,
    /// The code inserted when picking suggestion.
    pub code:               &'static str,
    /// The type of expected `self` argument.
    pub this_arg:           Option<tp::QualifiedName>,
    /// The list of argument types which may be applied to the code returned by this suggestion.
    pub argument_types:     Vec<tp::QualifiedName>,
    /// The type returned by the suggestion's code.
    pub return_type:        Option<tp::QualifiedName>,
    /// An import required by the suggestion.
    pub imports:            Vec<module::QualifiedName>,
    /// The documentation bound to the suggestion.
    pub documentation_html: Option<&'static str>,
    /// The id of the method called by the suggestion.
    pub method_id:          Option<MethodId>,
    /// The name of the icon bound to this entry.
    pub icon:               ImString,
}

impl Suggestion {
    fn new(name: &'static str, code: &'static str, icon: &ImString) -> Self {
        let icon = icon.clone_ref();
        Self { name, code, icon, ..default() }
    }

    fn with_this_arg(mut self, this_arg: impl TryInto<tp::QualifiedName, Error: Debug>) -> Self {
        self.this_arg = Some(this_arg.try_into().unwrap());
        self
    }

    fn with_argument_types<Iter>(mut self, argument_types: Iter) -> Self
    where
        Iter: IntoIterator,
        Iter::Item: TryInto<tp::QualifiedName, Error: Debug>, {
        let conv_results = argument_types.into_iter().map(|arg| arg.try_into());
        let result = conv_results.collect::<Result<Vec<tp::QualifiedName>, _>>();
        self.argument_types = result.unwrap();
        self
    }

    fn with_return_type(
        mut self,
        return_type: impl TryInto<tp::QualifiedName, Error: Debug>,
    ) -> Self {
        self.return_type = Some(return_type.try_into().unwrap());
        self
    }

    fn with_import_added(
        mut self,
        import: impl TryInto<module::QualifiedName, Error: Debug>,
    ) -> Self {
        self.imports.push(import.try_into().unwrap());
        self
    }

    fn marked_as_method_call(
        mut self,
        name: &'static str,
        module: impl TryInto<module::QualifiedName, Error: Debug>,
    ) -> Self {
        self.method_id = Some(MethodId {
            module:          module.try_into().unwrap(),
            defined_on_type: self.this_arg.as_ref().unwrap().clone(),
            name:            name.to_owned(),
        });
        self
    }

    fn marked_as_module_method_call(
        mut self,
        name: &'static str,
        module: impl TryInto<module::QualifiedName, Error: Debug>,
    ) -> Self {
        let module = module.try_into().unwrap();
        self.method_id = Some(MethodId {
            module:          module.clone(),
            defined_on_type: module.into(),
            name:            name.to_owned(),
        });
        self
    }
}



// ======================================
// === The Hardcoded Suggestions List ===
// ======================================

// The constant must be thread local because of using Rc inside. It should not affect the
// application much, because we are in a single thread anyway.
thread_local! {


    /// The suggestions constant.
    pub static SUGGESTIONS:Vec<RootCategory> = ICONS.with(|icons| vec![
        RootCategory {
            name       : "Data Science",
            icon       : icons.data_science.clone_ref(),
            categories : vec![
                Subcategory {
                    name        : "Input / Output",
                    icon        : icons.input_output.clone_ref(),
                    suggestions : vec![
                        Rc::new(
                            Suggestion::new("Text Input","\"\"",&icons.text_input)
                            .with_return_type("Standard.Base.Data.Text.Text")
                        ),
                        Rc::new(
                            Suggestion::new("Number Input","0",&icons.number_input)
                            .with_return_type("Standard.Base.Data.Numbers.Number")
                        ),
                    ]
                },
                Subcategory {
                    name        : "Text",
                    icon        : icons.text.clone_ref(),
                    suggestions : vec![
                        Rc::new(
                            Suggestion::new("Text Length","length",&icons.default)
                            .with_this_arg("Standard.Base.Data.Text.Text")
                            .with_return_type("Standard.Base.Data.Numbers.Integer")
                            .marked_as_method_call("length","Standard.Base.Data.Text.Extensions")
                        )
                    ]
                }
            ]
        },
        RootCategory {
            name       : "Network",
            icon       : icons.default.clone_ref(),
            categories : vec![
                Subcategory {
                    name : "HTTP",
                    icon : icons.default.clone_ref(),
                    suggestions : vec![
                        Rc::new(
                            Suggestion::new("Fetch Data", "Http.fetch",&icons.default)
                            .with_return_type("Standard.Base.Network.Http.Body.Body")
                            .with_argument_types(vec![
                                "Standard.Base.Data.Text.Text",
                                "Vector.Vector",
                            ])
                            .with_import_added("Standard.Base.Network.Http")
                            .marked_as_module_method_call("fetch","Standard.Base.Network.Http")
                        ),
                        Rc::new(
                            Suggestion::new("GET Request", "Http.get",&icons.default)
                            .with_return_type("Standard.Base.Network.Http.Response.Response")
                            .with_import_added("Standard.Base.Network.Http")
                            .marked_as_module_method_call("get","Standard.Base.Network.Http")
                        )
                    ]
                }
            ]
        }
    ]);
}

/// Extend the list built by given [`ListBuilder`] with the categories and actions hardcoded
/// in [`SUGGESTIONS`] constant.
pub fn add_hardcoded_entries_to_list(
    list: &mut ListBuilder,
    this_type: Option<&tp::QualifiedName>,
    return_types: Option<&HashSet<tp::QualifiedName>>,
) {
    SUGGESTIONS.with(|hardcoded| {
        for hc_root_category in hardcoded {
            let icon = hc_root_category.icon.clone_ref();
            let mut root_cat = list.add_root_category(hc_root_category.name, icon);
            for hc_category in &hc_root_category.categories {
                let icon = hc_root_category.icon.clone_ref();
                let category = root_cat.add_category(hc_category.name, icon);
                category.extend(hc_category.suggestions.iter().cloned().filter_map(|suggestion| {
                    let this_type_matches = if let Some(this_type) = this_type {
                        suggestion.this_arg.contains(this_type)
                    } else {
                        true
                    };
                    let return_type_matches = if let Some(return_types) = return_types {
                        suggestion
                            .return_type
                            .as_ref()
                            .map_or(false, |rt| return_types.contains(rt))
                    } else {
                        true
                    };
                    let filtered_in = this_type_matches && return_type_matches;
                    filtered_in.as_some_from(|| {
                        action::Action::Suggestion(action::Suggestion::Hardcoded(suggestion))
                    })
                }));
            }
        }
    });
}
