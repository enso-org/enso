//! HTML generator for documentation.

use enso_prelude::*;
use horrorshow::prelude::*;

use enso_suggestion_database::documentation_ir::Constructors;
use enso_suggestion_database::documentation_ir::Documentation;
use enso_suggestion_database::documentation_ir::EntryDocumentation;
use enso_suggestion_database::documentation_ir::Function;
use enso_suggestion_database::documentation_ir::ModuleDocumentation;
use enso_suggestion_database::documentation_ir::Placeholder;
use enso_suggestion_database::documentation_ir::Synopsis;
use enso_suggestion_database::documentation_ir::TypeDocumentation;
use enso_suggestion_database::engine_protocol::language_server::DocSection;
use enso_suggestion_database::engine_protocol::language_server::Mark;
use enso_suggestion_database::entry::Argument;
use horrorshow::box_html;
use horrorshow::labels;
use horrorshow::owned_html;



// =============
// === Icons ===
// =============

/// We use SVG icons imported as text.
type Icon = &'static str;
const ICON_TYPE: &str = include_str!("../assets/icon-type.svg");
const ICON_METHODS: &str = include_str!("../assets/icon-methods.svg");
const ICON_EXAMPLES: &str = include_str!("../assets/icon-examples.svg");
/// A value for `viewBox` attribute of the SVG icon. Depends on the size of the icon exported from
/// Figma.
const ICON_VIEWBOX: &str = "0 0 32 32";
const ICON_SVG_XMLNS: &str = "http://www.w3.org/2000/svg";

/// A single icon used in headers. `content` is an SVG code of the icon's content _without_ the
/// surrounding `<svg>` tags.
fn svg_icon(content: &'static str) -> impl Render {
    let class = "w-5 h-5 fill-none flex-shrink-0 mt-0.5";
    owned_html! {
        svg(class=class, viewBox=ICON_VIEWBOX, xmlns=ICON_SVG_XMLNS) {
            :Raw(content)
        }
    }
}



// ==============
// === Render ===
// ==============

/// Render entry documentation to HTML code with Tailwind CSS styles.
pub fn render(docs: EntryDocumentation) -> String {
    match docs {
        EntryDocumentation::Placeholder(placeholder) => match placeholder {
            Placeholder::Function { name } => format!("Function {name}"),
            Placeholder::Local { name } => format!("Local {name}"),
            Placeholder::NoDocumentation => "No documentation found".into(),
        },
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

// === Types ===

/// Render documentation of a type.
///
/// Consists of the following parts:
/// - Type name.
/// - Synopsis and a list of constructors.
/// - Methods TODO(https://www.pivotaltracker.com/story/show/184024167).
/// - Examples TODO(https://www.pivotaltracker.com/story/show/184024198).
fn render_type_documentation(type_docs: &TypeDocumentation) -> String {
    let TypeDocumentation { name, arguments, constructors, methods, synopsis, .. } = type_docs;

    let content = owned_html! {
        : header(ICON_TYPE, type_header(name.name(), arguments_list(arguments)));
        : type_synopsis(synopsis, constructors);
        : header(ICON_METHODS, methods_header());
        ul(class="list-disc list-inside") {
            @ for method in methods.iter() {
                li(class="text-base") {
                    : method.name.name()
                }
            }
        }
        : header(ICON_EXAMPLES, examples_header());
    };
    docs_content(content).into_string().unwrap()
}

/// A header for the type documentation.
fn type_header<'a>(name: &'a str, arguments: impl Render + 'a) -> Box<dyn Render + 'a> {
    box_html! {
        span(class="text-2xl font-bold") {
            span(class="text-type") { : name }
            span(class="text-arguments") { : &arguments }
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

/// A synopsis of the type. Contains a list of constructors, if it is not empty.
fn type_synopsis<'a>(
    synopsis: &'a Synopsis,
    constructors: &'a Constructors,
) -> Box<dyn Render + 'a> {
    box_html! {
        div(class="pl-7") {
            @ for p in synopsis.iter() {
                : paragraph(p);
            }
        }
        @ if !constructors.is_empty() {
            p(class="pl-7") {
                : "Constructors:"
            }
        }
        ul(class="pl-7 list-disc list-outside marker:text-type") {
            @ for method in constructors.iter() {
                li {
                    span(class="text-type font-bold") {
                        : method.name.name();
                        : arguments_list(&method.arguments);
                    }
                    : constructor_docs(method);
                }
            }
        }
    }
}

/// Documentation of a single constructor. If the first [`DocSection`] is of type
/// [`DocSection::Paragraph`], it is rendered on the first line, after the list of arguments. All
/// other sections are rendered as separate paragraphs below.
fn constructor_docs<'a>(constructor: &'a Function) -> Box<dyn Render + 'a> {
    let (first, rest) = match &constructor.synopsis.as_ref()[..] {
        [DocSection::Paragraph { body }, rest @ ..] => (Some(body), rest),
        [_, rest @ ..] => (None, rest),
        [] => (None, default()),
    };
    box_html! {
        @ if let Some(first) = first {
            span { : ", "; : first; }
        }
        @ for p in rest {
            : paragraph(p);
        }
    }
}


// === Modules ===

/// Render documentation of a module.
///
/// Consists of the following parts:
/// - Module name
/// - Synopsis.
/// - Types TODO(https://www.pivotaltracker.com/story/show/184024179).
/// - Functions TODO(https://www.pivotaltracker.com/story/show/184024167).
/// - Examples TODO(https://www.pivotaltracker.com/story/show/184024198).
fn render_module_documentation(module_docs: &ModuleDocumentation) -> String {
    let ModuleDocumentation { name, types, methods, synopsis, .. } = module_docs;

    let content = owned_html! {
        : header(ICON_TYPE, module_header(name.name()));
        : module_synopsis(synopsis);
        : header(ICON_METHODS, types_header());
        ul(class="list-disc list-inside") {
            @ for ty in types.iter() {
                li(class="text-base") {
                    : ty.name.name()
                }
            }
        }
        : header(ICON_METHODS, functions_header());
        ul(class="list-disc list-inside") {
            @ for method in methods.iter() {
                li(class="text-base") {
                    : method.name.name()
                }
            }
        }
        : header(ICON_EXAMPLES, examples_header());
    };
    docs_content(content).into_string().unwrap()
}

/// A header for the module documentation.
fn module_header<'a>(name: &'a str) -> Box<dyn Render + 'a> {
    box_html! {
        h1(class="text-2xl font-bold text-module") {
            : name
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

/// A synopsis of the module.
fn module_synopsis<'a>(synopsis: &'a Synopsis) -> Box<dyn Render + 'a> {
    box_html! {
        div(class="synopsis pl-7") {
            @ for p in synopsis.iter() {
                : paragraph(p);
            }
        }
    }
}



// =======================
// === Common elements ===
// =======================

/// A container for the whole documentation. Has a small paddings, sets the font using `enso-docs`
/// class.
fn docs_content(content: impl Render) -> impl Render {
    owned_html! {
        div(class="enso-docs text-base pl-4 pr-2") {
            : &content;
        }
    }
}

/// Generic header. Contains an icon on the left followed by an arbitrary content.
fn header(icon: Icon, content: impl Render) -> impl Render {
    owned_html! {
        div(class="flex flex-row items-center my-2") {
            : svg_icon(icon);
            div(class="ml-2") {
                : &content;
            }
        }
    }
}

/// List of arguments of the type or function.
fn arguments_list<'a>(arguments: &'a [Argument]) -> Box<dyn Render + 'a> {
    box_html! {
        span {
            @ for arg in arguments {
                : single_argument(arg);
            }
        }
    }
}

/// A single argument of the type or function. May contain default value.
fn single_argument(argument: &Argument) -> impl Render {
    let Argument { name, default_value, .. } = argument;
    let text = if let Some(default_value) = default_value {
        format!("{} = {},", name, default_value)
    } else {
        name.to_string()
    };
    owned_html! {
        span { : " "; : &text; }
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

/// Render a single [`DocSection`] as a paragraph of text. Does not work for [`DocSection::Marked`]
/// with [`Mark::Example`] and for [`DocSection::Tag`].
fn paragraph<'a>(doc_section: &'a DocSection) -> Box<dyn Render + 'a> {
    match doc_section {
        DocSection::Keyed { key, body } => {
            box_html! {
                p { : key; : ": "; }
                : Raw(body);
            }
        }
        DocSection::Paragraph { body } => {
            box_html! {
                p { : Raw(body); }
            }
        }
        DocSection::Marked { mark, header, body } => {
            let background_color = match mark {
                Mark::Important => "bg-important",
                Mark::Info => "bg-info",
                _ => "",
            };
            let mark = match mark {
                Mark::Important => String::from("!"),
                Mark::Info => String::from("ℹ"),
                _ => String::from("Unexpected mark."),
            };
            box_html! {
                div(class=labels!(background_color, "rounded-lg", "p-2", "my-2")) {
                    p(class="text-lg") {
                        span(class="font-bold") { : &mark; }
                        : " "; : header;
                    }
                    p { : Raw(body); }
                }
            }
        }
        _ => box_html! {
            p { : "Unexpected doc section type." }
        },
    }
}
