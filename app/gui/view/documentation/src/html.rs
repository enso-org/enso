//! HTML generator for documentation.

use enso_prelude::*;
use horrorshow::prelude::*;

use double_representation::name::QualifiedName;
use enso_doc_parser::DocSection;
use enso_doc_parser::Mark;
use enso_profiler as profiler;
use enso_profiler::profile;
use enso_suggestion_database::documentation_ir::BuiltinDocumentation;
use enso_suggestion_database::documentation_ir::Documentation;
use enso_suggestion_database::documentation_ir::EntryDocumentation;
use enso_suggestion_database::documentation_ir::Examples;
use enso_suggestion_database::documentation_ir::Function;
use enso_suggestion_database::documentation_ir::LocalDocumentation;
use enso_suggestion_database::documentation_ir::ModuleDocumentation;
use enso_suggestion_database::documentation_ir::Synopsis;
use enso_suggestion_database::documentation_ir::Tag;
use enso_suggestion_database::documentation_ir::TypeDocumentation;
use enso_suggestion_database::documentation_ir::Types;
use enso_suggestion_database::entry::Argument;
use horrorshow::box_html;
use horrorshow::labels;
use horrorshow::owned_html;



// =============
// === Icons ===
// =============

/// We use SVG icons imported as text.
type Icon = &'static str;
const ICON_METHODS: &str = include_str!("../assets/icon-methods.svg");
const ICON_EXAMPLES: &str = include_str!("../assets/icon-examples.svg");
const ICON_INFO: &str = include_str!("../assets/icon-info.svg");
const ICON_IMPORTANT: &str = include_str!("../assets/icon-important.svg");

/// A single icon used in headers. `content` is an SVG code of the icon.
fn svg_icon(content: &'static str, class: &'static str) -> impl Render {
    owned_html! {
        div(class=class) {
            :Raw(content)
        }
    }
}



// ==============
// === Render ===
// ==============

/// Render entry documentation to HTML code.
#[profile(Detail)]
pub fn render(docs: &EntryDocumentation) -> String {
    let html = match docs {
        EntryDocumentation::Placeholder => String::from("No documentation available."),
        EntryDocumentation::Docs(docs) => render_documentation(docs.clone_ref()),
    };
    match validate_utf8(&html) {
        Ok(_) => html,
        Err(_) => {
            error!("Internal error. Generated HTML is not valid utf-8. This is bug #5813.");
            String::from("Failed to load documentation.")
        }
    }
}

#[profile(Debug)]
fn validate_utf8(s: &str) -> Result<&str, std::str::Utf8Error> {
    let bytes = s.as_bytes();
    std::str::from_utf8(bytes)
}

fn render_documentation(docs: Documentation) -> String {
    match docs {
        Documentation::Module(module_docs) => render_module_documentation(&module_docs),
        Documentation::Type { docs, .. } => render_type_documentation(&docs),
        Documentation::Function(docs) => render_function_documentation(&docs),
        Documentation::Local(docs) => render_local_documentation(&docs),
        Documentation::Constructor { docs, .. } => render_function_documentation(&docs),
        Documentation::Method { docs, .. } => render_function_documentation(&docs),
        Documentation::ModuleMethod { docs, .. } => render_function_documentation(&docs),
        Documentation::Builtin(builtin_docs) => render_builtin_documentation(&builtin_docs),
    }
}


// === Types ===

/// Render documentation of a type.
///
/// Consists of the following parts:
/// - Type name.
/// - Synopsis and a list of constructors.
/// - Methods.
/// - Examples.
fn render_type_documentation(docs: &TypeDocumentation) -> String {
    let constructors_exist = !docs.constructors.is_empty();
    let methods_exist = !docs.methods.is_empty();
    let examples_exist = !docs.examples.is_empty();
    let synopsis = &docs.synopsis;
    let constructors = &docs.constructors;
    let synopsis = section_content(type_synopsis(synopsis));
    let constructors = section_content(list_of_functions(constructors));
    let methods = section_content(list_of_functions(&docs.methods));
    let examples = section_content(list_of_examples(&docs.examples));
    let tags = section_content(list_of_tags(&docs.tags));

    let content = owned_html! {
        : &tags;
        : &synopsis;
        @ if constructors_exist {
            : constructors_header();
            : &constructors;
        }
        @ if methods_exist {
            : methods_header();
            : &methods;
        }
        @ if examples_exist {
            : examples_header();
            : &examples;
        }
    };
    docs_content(content).into_string().unwrap()
}

fn constructors_header() -> impl Render {
    header(ICON_METHODS, "Constructors", "section-header methods-header")
}

fn methods_header() -> impl Render {
    header(ICON_METHODS, "Methods", "section-header methods-header")
}

fn examples_header() -> impl Render {
    header(ICON_EXAMPLES, "Examples", "section-header examples-header")
}

fn types_header() -> impl Render {
    header(ICON_METHODS, "Types", "section-header types-header")
}

/// A synopsis of the type. Contains a list of constructors, if it is not empty.
fn type_synopsis<'a>(synopsis: &'a Synopsis) -> Box<dyn Render + 'a> {
    box_html! {
        @ for p in synopsis.iter() {
            : paragraph(p);
        }
    }
}

/// A list of methods defined for the type.
fn list_of_functions<'a>(functions: &'a [Function]) -> Box<dyn Render + 'a> {
    box_html! {
        ul(class="unordered-list") {
            @ for f in functions.iter() {
                : single_function(f);
            }
        }
    }
}

/// A documentation for a single method in the list.
/// If the first [`DocSection`] is of type [`DocSection::Paragraph`], it is rendered on the first
/// line, after the list of arguments.
fn single_function<'a>(function: &'a Function) -> Box<dyn Render + 'a> {
    let first = match &function.synopsis.as_ref()[..] {
        [DocSection::Paragraph { body }, ..] => Some(body),
        _ => None,
    };
    box_html! {
        li(class="method-item") {
            a(id=anchor_name(&function.name), class="link method") {
                span(class="entry-name") { : function.name.name(); }
                span(class="arguments") { : arguments_list(&function.arguments); }
            }
            @ if let Some(first) = first {
                : ": "; : Raw(first);
            }
        }
    }
}


// === Modules ===

/// Render documentation of a module.
///
/// Consists of the following parts:
/// - Module name
/// - Synopsis.
/// - Types.
/// - Functions.
/// - Examples.
fn render_module_documentation(docs: &ModuleDocumentation) -> String {
    let types_exist = !docs.types.is_empty();
    let methods_exist = !docs.methods.is_empty();
    let examples_exist = !docs.examples.is_empty();
    let synopsis = section_content(module_synopsis(&docs.synopsis));
    let types = section_content(list_of_types(&docs.types));
    let methods = section_content(list_of_functions(&docs.methods));
    let examples = section_content(list_of_examples(&docs.examples));
    let tags = section_content(list_of_tags(&docs.tags));
    let content = owned_html! {
        : &tags;
        : &synopsis;
        @ if types_exist {
            : types_header();
            : &types;
        }
        @ if methods_exist {
            : methods_header();
            : &methods;
        }
        @ if examples_exist {
            : examples_header();
            : &examples;
        }
    };
    docs_content(content).into_string().unwrap()
}

/// A list of types defined in the module.
fn list_of_types<'a>(types: &'a Types) -> Box<dyn Render + 'a> {
    box_html! {
        ul(class="unordered-list") {
            @ for type_ in types.iter() {
                : single_type(type_);
            }
        }
    }
}

/// A single type in the list.
fn single_type<'a>(type_: &'a TypeDocumentation) -> Box<dyn Render + 'a> {
    box_html! {
        li(class="type-item") {
            a(id=anchor_name(&type_.name), class="link type") {
                span(class="entry-name") { : type_.name.name(); }
                span(class="arguments") { : arguments_list(&type_.arguments); }
            }
        }
    }
}

/// List of examples for the entity.
fn list_of_examples<'a>(examples: &'a Examples) -> Box<dyn Render + 'a> {
    box_html! {
        @ for example in examples.iter() {
            div(class="example-container") {
                : Raw(example_from_doc_section(example));
            }
        }
    }
}

/// Build an HTML code of the example from the documentation section. Engine already provides as
/// with a preformatted HTML code, but we need to modify some tags in order to properly style it.
fn example_from_doc_section(doc_section: &DocSection) -> String {
    match doc_section {
        DocSection::Marked { mark: Mark::Example, body, .. } =>
            body.replace("<pre>", "<div class=\"example\">").replace("</pre>", "</div>"),
        _ => String::from("Invalid example"),
    }
}

/// A synopsis of the module.
fn module_synopsis<'a>(synopsis: &'a Synopsis) -> Box<dyn Render + 'a> {
    box_html! {
        @ for p in synopsis.iter() {
            : paragraph(p);
        }
    }
}


// === Functions ===

/// Render documentation of a function.
fn render_function_documentation(docs: &Function) -> String {
    let Function { synopsis, tags, .. } = docs;

    let examples_exist = !docs.examples.is_empty();
    let synopsis = section_content(function_synopsis(synopsis));
    let tags = section_content(list_of_tags(tags));
    let examples = section_content(list_of_examples(&docs.examples));
    let content = owned_html! {
        : &tags;
        : &synopsis;
        @ if examples_exist {
            : examples_header();
            : &examples;
        }
    };
    docs_content(content).into_string().unwrap()
}

/// A synopsis of the function.
fn function_synopsis<'a>(synopsis: &'a Synopsis) -> Box<dyn Render + 'a> {
    box_html! {
        @ for p in synopsis.iter() {
            : paragraph(p);
        }
    }
}


// === Locals ===

/// Render documentation of a function.
fn render_local_documentation(docs: &LocalDocumentation) -> String {
    let LocalDocumentation { synopsis, tags, .. } = docs;

    let examples_exist = !docs.examples.is_empty();
    let synopsis = section_content(local_synopsis(synopsis));
    let tags = section_content(list_of_tags(tags));
    let examples = section_content(list_of_examples(&docs.examples));

    let content = owned_html! {
        : &tags;
        : &synopsis;
        @ if examples_exist {
            : examples_header();
            : &examples;
        }
    };
    docs_content(content).into_string().unwrap()
}

/// A synopsis of the local.
fn local_synopsis<'a>(synopsis: &'a Synopsis) -> Box<dyn Render + 'a> {
    box_html! {
        @ for p in synopsis.iter() {
            : paragraph(p);
        }
    }
}


// === Builtin entries ===

/// Render documentation for built-in entries.
///
/// Consists of only a synopsis.
fn render_builtin_documentation(docs: &BuiltinDocumentation) -> String {
    let synopsis = section_content(module_synopsis(&docs.synopsis));
    let content = owned_html! {
        : &synopsis;
    };
    docs_content(content).into_string().unwrap()
}



// =======================
// === Common elements ===
// =======================

/// A container for the whole documentation. Has a small paddings, sets the font using `enso-docs`
/// class.
fn docs_content(content: impl Render) -> impl Render {
    owned_html! {
        div(class="enso-docs") {
            : &content;
        }
    }
}

fn section_content(content: impl Render) -> impl Render {
    owned_html! {
        div(class="section-content") {
            : &content;
        }
    }
}

/// Generic header. Contains an icon on the left followed by an arbitrary content.
fn header(icon: Icon, content: impl Render, class: &'static str) -> impl Render {
    owned_html! {
        div(class=labels!("header-container", class)) {
            : svg_icon(icon, "header-icon");
            div(class="header-text") {
                : &content;
            }
        }
    }
}

/// List of arguments of the type or function.
fn arguments_list<'a>(arguments: &'a [Argument]) -> Box<dyn Render + 'a> {
    box_html! {
        @ for arg in arguments {
            : " ";
            : single_argument(arg);
        }
    }
}

/// A single argument of the type or function. May contain default value.
fn single_argument(argument: &Argument) -> String {
    let Argument { name, default_value, .. } = argument;
    if let Some(default_value) = default_value {
        format!("{name} = {default_value},")
    } else {
        name.to_string()
    }
}

/// Render a single [`DocSection`] as a paragraph of text. Does not work for [`DocSection::Marked`]
/// with [`Mark::Example`] and for [`DocSection::Tag`].
fn paragraph<'a>(doc_section: &'a DocSection) -> Box<dyn Render + 'a> {
    match doc_section {
        DocSection::Keyed { key, body } => {
            box_html! {
                p(class="paragraph") { : Raw(key); : ": "; }
                : Raw(body);
            }
        }
        DocSection::Paragraph { body } => {
            box_html! {
                p(class="paragraph") { : Raw(body); }
            }
        }
        DocSection::Marked { mark, header, body } => {
            let background_color = match mark {
                Mark::Important => "background-important",
                Mark::Info => "background-info",
                _ => "",
            };
            let mark: Box<dyn Render> = match mark {
                Mark::Important =>
                    Box::new(svg_icon(ICON_IMPORTANT, "marked-icon marked-icon-important")),
                Mark::Info => Box::new(svg_icon(ICON_INFO, "marked-icon marked-icon-info")),
                _ => Box::new(String::from("Unexpected mark.")),
            };
            box_html! {
                div(class=labels!(background_color, "marked-container")) {
                    div(class="marked-header") {
                        : &mark;
                        : " "; : header;
                    }
                    p(class="paragraph") { : Raw(body); }
                }
            }
        }
        DocSection::List { items } => {
            box_html! {
                ul(class="unordered-list") {
                    @for item in items {
                        li { : Raw(&item); }
                    }
                }
            }
        }
        DocSection::Arguments { args } => {
            box_html! {
                ul(class="unordered-list") {
                    @for arg in args {
                        li {
                            span(class="argument") { : &arg.name; }
                            : ": "; : Raw(&arg.description);
                        }
                    }
                }
            }
        }
        _ => box_html! {
            p(class="paragraph") { : "Unexpected doc section type." }
        },
    }
}

/// A list of tags.
fn list_of_tags<'a>(tags: &'a [Tag]) -> Box<dyn Render + 'a> {
    box_html! {
        div(class="tags-container") {
            @ for tag in tags {
                : single_tag(tag);
            }
        }
    }
}

/// A single tag in the list.
fn single_tag<'a>(tag: &'a Tag) -> Box<dyn Render + 'a> {
    box_html! {
        div(class="tag") {
            : &*tag.name;
            @if !tag.body.is_empty() {
                : "=";
                : &*tag.body;
            }
        }
    }
}

/// Anchor name for the provided qualified name. It is used to set the unique `id` attribute for the
/// generated HTML elements.
pub fn anchor_name(name: &QualifiedName) -> String {
    name.to_string().replace('.', "_").to_lowercase()
}
