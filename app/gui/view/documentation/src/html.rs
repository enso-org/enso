//! HTML generator for documentation.

use enso_prelude::*;
use horrorshow::prelude::*;

use double_representation::name::QualifiedName;
use enso_doc_parser::DocSection;
use enso_doc_parser::Mark;
use enso_profiler as profiler;
use enso_profiler::profile;
use enso_suggestion_database::documentation_ir::BuiltinDocumentation;
use enso_suggestion_database::documentation_ir::Constructors;
use enso_suggestion_database::documentation_ir::Documentation;
use enso_suggestion_database::documentation_ir::EntryDocumentation;
use enso_suggestion_database::documentation_ir::Examples;
use enso_suggestion_database::documentation_ir::Function;
use enso_suggestion_database::documentation_ir::FunctionDocumentation;
use enso_suggestion_database::documentation_ir::LocalDocumentation;
use enso_suggestion_database::documentation_ir::ModuleDocumentation;
use enso_suggestion_database::documentation_ir::Placeholder;
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
    let class = "w-[12px] h-[12px] fill-none flex-shrink-0 mt-0.5";
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
#[profile(Detail)]
pub fn render(docs: EntryDocumentation) -> String {
    match docs {
        EntryDocumentation::Placeholder(placeholder) => match placeholder {
            Placeholder::NoDocumentation => String::from("No documentation available."),
            Placeholder::VirtualComponentGroup { name } =>
                render_virtual_component_group_docs(name),
        },
        EntryDocumentation::Docs(docs) => render_documentation(docs),
    }
}

fn render_documentation(docs: Documentation) -> String {
    match docs {
        Documentation::Module(module_docs) => render_module_documentation(&module_docs, None),
        Documentation::Type(type_docs) => render_type_documentation(&type_docs, None),
        Documentation::Function(docs) => render_function_documentation(&docs),
        Documentation::Local(docs) => render_local_documentation(&docs),
        Documentation::Constructor { type_docs, name } =>
            render_type_documentation(&type_docs, Some(&name)),
        Documentation::Method { type_docs, name } =>
            render_type_documentation(&type_docs, Some(&name)),
        Documentation::ModuleMethod { module_docs, name } =>
            render_module_documentation(&module_docs, Some(&name)),
        Documentation::Builtin(builtin_docs) => render_builtin_documentation(&builtin_docs),
    }
}

/// Render the documentation of the virtual component group. Includes the name of the group.
fn render_virtual_component_group_docs(name: ImString) -> String {
    let content = owned_html! {
        h1(class="text-2xl font-bold") {
            : &*name
        }
    };
    docs_content(content).into_string().unwrap()
}


// === Types ===

/// Render documentation of a type.
///
/// Consists of the following parts:
/// - Type name.
/// - Synopsis and a list of constructors.
/// - Methods.
/// - Examples.
fn render_type_documentation(
    docs: &TypeDocumentation,
    function_name: Option<&QualifiedName>,
) -> String {
    let methods_exist = !docs.methods.is_empty();
    let examples_exist = !docs.examples.is_empty();
    let name = &docs.name;
    let arguments = &docs.arguments;
    let synopsis = &docs.synopsis;
    let constructors = &docs.constructors;
    let synopsis = section_content(type_synopsis(synopsis, constructors, function_name));
    let methods = section_content(list_of_functions(&docs.methods, function_name));
    let examples = section_content(list_of_examples(&docs.examples));
    let tags = section_content(list_of_tags(&docs.tags));

    let content = owned_html! {
        : header(ICON_TYPE, type_header(name.name(), arguments_list(arguments)));
        : &tags;
        : &synopsis;
        @ if methods_exist {
            : header(ICON_METHODS, methods_header());
            : &methods;
        }
        @ if examples_exist {
            : header(ICON_EXAMPLES, examples_header());
            : &examples;
        }
    };
    docs_content(content).into_string().unwrap()
}

/// A header for the type documentation.
fn type_header<'a>(name: &'a str, arguments: impl Render + 'a) -> Box<dyn Render + 'a> {
    box_html! {
        span(class="text-2xl font-bold text-typeName") {
            span { : name }
            span(class="opacity-34") { : &arguments }
        }
    }
}

/// A header for the "Methods" section.
fn methods_header() -> impl Render {
    owned_html! {
        h1(class="text-xl font-semibold text-methodsHeader") {
            : "Methods"
        }
    }
}

/// A synopsis of the type. Contains a list of constructors, if it is not empty.
fn type_synopsis<'a>(
    synopsis: &'a Synopsis,
    constructors: &'a Constructors,
    function_name: Option<&'a QualifiedName>,
) -> Box<dyn Render + 'a> {
    box_html! {
        @ for p in synopsis.iter() {
            : paragraph(p);
        }
        @ if !constructors.is_empty() {
            p {
                : "Constructors:"
            }
        }
        ul(class="list-disc list-outside marker:text-typeName") {
            @ for method in constructors.iter() {
                : single_constructor(method, function_name);
            }
        }
    }
}

/// A documentation for a single constructor in the list.
fn single_constructor<'a>(
    method: &'a Function,
    function_name: Option<&'a QualifiedName>,
) -> Box<dyn Render + 'a> {
    let highlight = function_name.map(|n| n == &*method.name).unwrap_or(false);
    box_html! {
        li(id=anchor_name(&method.name)) {
            span(class=labels!("text-typeName", "font-bold", "bg-yellow-100" => highlight)) {
                span(class="opacity-85") {
                    : method.name.name();
                }
                span(class="opacity-34") { : arguments_list(&method.arguments); }
            }
            : function_docs(method);
        }
    }
}

/// A list of methods defined for the type.
fn list_of_functions<'a>(
    functions: &'a [Function],
    function_name: Option<&'a QualifiedName>,
) -> Box<dyn Render + 'a> {
    box_html! {
        ul(class="list-disc list-inside") {
            @ for f in functions.iter() {
                : single_function(f, function_name);
            }
        }
    }
}

/// A documentation for a single method in the list.
fn single_function<'a>(
    function: &'a Function,
    function_name: Option<&'a QualifiedName>,
) -> Box<dyn Render + 'a> {
    let highlight = function_name.map(|n| n == &*function.name).unwrap_or(false);
    box_html! {
        li(id=anchor_name(&function.name)) {
            span(class=labels!("text-methodName", "font-semibold", "bg-yellow-100" => highlight)) {
                span(class="opacity-85") {
                    : function.name.name();
                }
                span(class="opacity-34") { : arguments_list(&function.arguments); }
            }
            : function_docs(function);
        }
    }
}

/// Synopsis of a function. If the first [`DocSection`] is of type
/// [`DocSection::Paragraph`], it is rendered on the first line, after the list of arguments. All
/// other sections are rendered as separate paragraphs below. Examples for the function are rendered
/// below the main part of the documentation in a separate subsection.
fn function_docs<'a>(constructor: &'a Function) -> Box<dyn Render + 'a> {
    let (first, rest) = match &constructor.synopsis.as_ref()[..] {
        [DocSection::Paragraph { body }, rest @ ..] => (Some(body), rest),
        [_, rest @ ..] => (None, rest),
        [] => (None, default()),
    };
    let tags = list_of_tags(&constructor.tags);
    box_html! {
        @ if let Some(first) = first {
            span { : ", "; : Raw(first); }
        }
        : &tags;
        @ for p in rest {
            : paragraph(p);
        }
        @ if !constructor.examples.is_empty() {
            h2(class="font-semibold") { : "Examples" }
            : list_of_examples(&constructor.examples);
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
fn render_module_documentation(
    docs: &ModuleDocumentation,
    function_name: Option<&QualifiedName>,
) -> String {
    let types_exist = !docs.types.is_empty();
    let methods_exist = !docs.methods.is_empty();
    let examples_exist = !docs.examples.is_empty();
    let name = &docs.name;
    let synopsis = section_content(module_synopsis(&docs.synopsis));
    let types = section_content(list_of_types(&docs.types));
    let methods = section_content(list_of_functions(&docs.methods, function_name));
    let examples = section_content(list_of_examples(&docs.examples));
    let tags = section_content(list_of_tags(&docs.tags));
    let content = owned_html! {
        : header(ICON_TYPE, module_header(name.name()));
        : &tags;
        : &synopsis;
        @ if types_exist {
            : header(ICON_METHODS, types_header());
            : &types;
        }
        @ if methods_exist {
            : header(ICON_METHODS, functions_header());
            : &methods;
        }
        @ if examples_exist {
            : header(ICON_EXAMPLES, examples_header());
            : &examples;
        }
    };
    docs_content(content).into_string().unwrap()
}

/// A list of types defined in the module.
fn list_of_types<'a>(types: &'a Types) -> Box<dyn Render + 'a> {
    box_html! {
        ul(class="list-disc list-inside") {
            @ for type_ in types.iter() {
                : single_type(type_);
            }
        }
    }
}

/// A single type in the list.
fn single_type<'a>(type_: &'a TypeDocumentation) -> Box<dyn Render + 'a> {
    box_html! {
        li(id=anchor_name(&type_.name), class="text-typeName font-semibold") {
            span(class="opacity-85") {
                : type_.name.name();
            }
            span(class="opacity-34") { : arguments_list(&type_.arguments); }
        }
    }
}

/// List of examples for the entity.
fn list_of_examples<'a>(examples: &'a Examples) -> Box<dyn Render + 'a> {
    box_html! {
        @ for example in examples.iter() {
            div(class="bg-exampleBackground rounded p-3 mb-1") {
                : Raw(example_from_doc_section(example));
            }
        }
    }
}

/// Build an HTML code of the example from the documentation section. Engine already provides as
/// with a preformatted HTML code, but we need to modify some tags in order to properly style it.
fn example_from_doc_section(doc_section: &DocSection) -> String {
    match doc_section {
        DocSection::Marked { mark: Mark::Example, body, .. } => body
            .replace("<pre>", "<div class=\"whitespace-pre overflow-x-auto py-2\">")
            .replace("</pre>", "</div>"),
        _ => String::from("Invalid example"),
    }
}

/// A header for the module documentation.
fn module_header<'a>(name: &'a str) -> Box<dyn Render + 'a> {
    box_html! {
        h1(class="text-2xl font-bold text-moduleName") {
            : name
        }
    }
}

/// A header for the "Functions" section.
fn functions_header() -> impl Render {
    owned_html! {
        h1(class="text-xl font-semibold text-methodsHeader") {
            : "Functions"
        }
    }
}

/// A header for the "Types" section.
fn types_header() -> impl Render {
    owned_html! {
        h1(class="text-xl font-semibold text-typesHeader") {
            : "Types"
        }
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
fn render_function_documentation(docs: &FunctionDocumentation) -> String {
    let FunctionDocumentation { name, arguments, synopsis, tags, .. } = docs;

    let examples_exist = !docs.examples.is_empty();
    let synopsis = section_content(function_synopsis(synopsis));
    let tags = section_content(list_of_tags(tags));
    let examples = section_content(list_of_examples(&docs.examples));
    let content = owned_html! {
        : header(ICON_TYPE, function_header(name.name(), arguments_list(arguments)));
        : &tags;
        : &synopsis;
        @ if examples_exist {
            : header(ICON_EXAMPLES, examples_header());
            : &examples;
        }
    };
    docs_content(content).into_string().unwrap()
}

/// A header for the function documentation.
fn function_header<'a>(name: &'a str, arguments: impl Render + 'a) -> Box<dyn Render + 'a> {
    box_html! {
        span(class="text-2xl font-bold text-typeName") {
            span { : name }
            span(class="opacity-34") { : &arguments }
        }
    }
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
    let LocalDocumentation { name, synopsis, return_type, tags, .. } = docs;

    let examples_exist = !docs.examples.is_empty();
    let synopsis = section_content(local_synopsis(synopsis));
    let tags = section_content(list_of_tags(tags));
    let examples = section_content(list_of_examples(&docs.examples));

    let content = owned_html! {
        : header(ICON_TYPE, local_header(name.name(), return_type.name()));
        : &tags;
        : &synopsis;
        @ if examples_exist {
            : header(ICON_EXAMPLES, examples_header());
            : &examples;
        }
    };
    docs_content(content).into_string().unwrap()
}

/// A header for the local documentation.
fn local_header<'a>(name: &'a str, return_type: &'a str) -> Box<dyn Render + 'a> {
    box_html! {
        span(class="text-2xl font-bold") {
            span(class="text-type") { : name }
            : " ";
            span(class="text-arguments") { : return_type }
        }
    }
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
        div(class="enso-docs text-docsText text-base bg-docsBackground pl-4 pr-2") {
            : &content;
        }
    }
}

fn section_content(content: impl Render) -> impl Render {
    owned_html! {
        div(class="pl-5") {
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
        format!("{name} = {default_value},")
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
        h1(class="text-xl font-semibold text-examplesHeader") {
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
                p { : Raw(key); : ": "; }
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
                Mark::Important => "bg-importantBackground",
                Mark::Info => "bg-infoBackground",
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

/// A list of tags.
fn list_of_tags<'a>(tags: &'a [Tag]) -> Box<dyn Render + 'a> {
    box_html! {
        div(class="flex flex-row flex-wrap") {
            @ for tag in tags {
                : single_tag(tag);
            }
        }
    }
}

/// A single tag in the list.
fn single_tag<'a>(tag: &'a Tag) -> Box<dyn Render + 'a> {
    box_html! {
        div(class="bg-tagBackground rounded-lg px-2 py-1 mr-2 mb-1") {
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



// ===============
// === Caption ===
// ===============

/// "Hovered item preview" caption on top of the documentation panel.
pub fn caption_html() -> String {
    owned_html! {
        div(class="bg-captionBackground rounded-t-[14px] w-full h-full flex \
                   items-center justify-center") {
            div(class="text-base text-white") {
                : "Hovered item preview. Press the right mouse button to lock it.";
            }
        }
    }
    .into_string()
    .unwrap()
}
