//! HTML generator for documentation.

use horrorshow::prelude::*;

use enso_suggestion_database::documentation_ir::Documentation;
use enso_suggestion_database::documentation_ir::EntryDocumentation;
use enso_suggestion_database::documentation_ir::ModuleDocumentation;
use enso_suggestion_database::documentation_ir::TypeDocumentation;
use enso_suggestion_database::entry::Argument;
use horrorshow::box_html;
use horrorshow::owned_html;



// =============
// === Icons ===
// =============

/// We use SVG icons imported as text.
type Icon = &'static str;
const ICON_TYPE: &str = include_str!("../assets/icon-type.svg");
const ICON_METHODS: &str = include_str!("../assets/icon-methods.svg");

/// Render entry documentation to HTML code with Tailwind CSS styles.
pub fn render(docs: EntryDocumentation) -> String {
    match docs {
        EntryDocumentation::Placeholder(_) => String::from("Temporary placeholder"),
        EntryDocumentation::Docs(docs) => render_documentation(docs),
    }
}

fn render_documentation(docs: Documentation) -> String {
    match docs {
        Documentation::Module(module_docs) => render_module_documentation(&module_docs),
        Documentation::Type(type_docs) => render_type_documentation(&type_docs),
        _ => String::from("Not implemented"),
    }
}

/// Render documentation of a type.
///
/// Consists of the following parts:
/// - Type name.
/// - TODO: synopsis and constructors (https://www.pivotaltracker.com/story/show/184024148).
/// - Methods TODO(https://www.pivotaltracker.com/story/show/184024167) only header at the moment.
/// - Examples TODO(https://www.pivotaltracker.com/story/show/184024198) only header at the moment.
fn render_type_documentation(type_docs: &TypeDocumentation) -> String {
    let TypeDocumentation { name, arguments, constructors, methods, .. } = type_docs;

    owned_html! {
        div(class="docs") {
            : header(ICON_TYPE, type_header(name.name(), type_arguments(arguments)));
            p(class="text-base") {
                : "Type docs"
            }
            ul(class="list-disc") {
                @ for constructor in constructors.iter() {
                    li(class="text-base") {
                        : constructor.name.name()
                    }
                }
            }
            : header(ICON_METHODS, methods_header());
            ul(class="list-disc") {
                @ for method in methods.iter() {
                    li(class="text-base") {
                        : method.name.name()
                    }
                }
            }
            : header(ICON_METHODS, examples_header());
        }
    }
    .to_string()
}

/// Render documentation of a module.
///
/// Consists of the following parts:
/// - Module name
/// - TODO: synopsis (https://www.pivotaltracker.com/story/show/184024148).
/// - Types TODO(https://www.pivotaltracker.com/story/show/184024179) only header at the moment.
/// - Functions TODO(https://www.pivotaltracker.com/story/show/184024167) only header at the moment.
/// - Examples TODO(https://www.pivotaltracker.com/story/show/184024198) only header at the moment.
fn render_module_documentation(module_docs: &ModuleDocumentation) -> String {
    let ModuleDocumentation { name, types, methods, .. } = module_docs;

    owned_html! {
        div(class="docs") {
            : header(ICON_TYPE, module_header(name.name()));
            p(class="text-base") {
                : "Module docs"
            }
            : header(ICON_METHODS, types_header());
            ul(class="list-disc") {
                @ for ty in types.iter() {
                    li(class="text-base") {
                        : ty.name.name()
                    }
                }
            }
            : header(ICON_METHODS, functions_header());
            ul(class="list-disc") {
                @ for method in methods.iter() {
                    li(class="text-base") {
                        : method.name.name()
                    }
                }
            }
            : header(ICON_METHODS, examples_header());
        }
    }
    .to_string()
}

/// Generic header. Contains an icon on the left followed by an arbitrary content.
fn header(icon: Icon, content: impl Render) -> impl Render {
    owned_html! {
        div(class="flex flex-row items-center my-2") {
            div(class="pt-0.5 scale-110") {
                : Raw(icon);
            }
            div(class="ml-2") {
                : &content;
            }
        }
    }
}

/// List of arguments of the type or function.
fn type_arguments<'a>(arguments: &'a [Argument]) -> Box<dyn Render + 'a> {
    box_html! {
        span(class="text-arguments ml-2") {
            @ for arg in arguments {
                : type_argument(arg);
                : " ";
            }
        }
    }
}

/// A single argument of the type or function. May contain default value.
fn type_argument(argument: &Argument) -> impl Render {
    let Argument { name, default_value, .. } = argument;
    let text = if let Some(default_value) = default_value {
        format!("{} = {}", name, default_value)
    } else {
        name.to_string()
    };
    owned_html! {
        span {
            : &text;
        }
    }
}

/// A header for the type documentation.
fn type_header<'a>(name: &'a str, arguments: impl Render + 'a) -> Box<dyn Render + 'a> {
    box_html! {
        span(class="text-2xl font-bold") {
            span(class="text-type") {
                : name
            }
            span(class="text-arguments") {
                : &arguments
            }
        }
    }
}

/// A header for the module documentation.
fn module_header<'a>(name: &'a str) -> Box<dyn Render + 'a> {
    box_html! {
        h1(class="text-2xl font-bold text-module") {
            : name
        }
    }
}

/// A header for the "Methods" section.
fn methods_header() -> impl Render {
    owned_html! {
        h1(class="text-xl font-semibold text-methods") {
            : "Methods"
        }
    }
}

/// A header for the "Functions" section.
fn functions_header() -> impl Render {
    owned_html! {
        h1(class="text-xl font-semibold text-methods") {
            : "Functions"
        }
    }
}

/// A header for the "Types" section.
fn types_header() -> impl Render {
    owned_html! {
        h1(class="text-xl font-semibold text-types") {
            : "Types"
        }
    }
}

/// A header for the "Examples" section.
fn examples_header() -> impl Render {
    owned_html! {
        h1(class="text-xl font-semibold text-examples") {
            : "Examples"
        }
    }
}
