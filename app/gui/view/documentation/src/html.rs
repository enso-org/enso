use enso_prelude::*;
use enso_suggestion_database::documentation_ir::Documentation;
use enso_suggestion_database::documentation_ir::EntryDocumentation;
use enso_suggestion_database::documentation_ir::TypeDocumentation;
use xmlem::Document;
use xmlem::Element;

const ICON_TYPE: &str = include_str!("../assets/icon-type.svg");

fn append_element(
    document: &mut Document,
    root: &Element,
    name: impl Str,
    class: impl Str,
) -> Element {
    root.append_new_element(document, (name.as_ref(), [("class", class.as_ref())]))
}

pub fn render(docs: EntryDocumentation) -> String {
    let mut result = xmlem::Document::new("div");
    let root = result.root();
    match docs {
        EntryDocumentation::Placeholder(_) => return String::from("Placeholder"),
        EntryDocumentation::Docs(docs) => match docs {
            Documentation::Module(_) => {
                append_element(&mut result, &root, "h1", "text-xl")
                    .append_text(&mut result, "Module");
                append_element(&mut result, &root, "p", "text-base")
                    .append_text(&mut result, "Module docs");
            }
            Documentation::Type(type_docs) => {
                let TypeDocumentation {
                    name,
                    arguments,
                    tags,
                    synopsis,
                    constructors,
                    methods,
                    examples,
                } = &*type_docs;

                let header_div = append_element(&mut result, &root, "div", "flex flex-row");
                // let icon = Document::from_str("<svg></svg>").unwrap();
                // header_div.append_element(&mut result, icon.root());

                let header = append_element(&mut result, &header_div, "span", "text-xl");
                append_element(&mut result, &header, "span", "text-fuchsia-600")
                    .append_text(&mut result, name.name());
                let arguments = arguments.iter().map(|arg| arg.name.clone()).join(", ");
                append_element(&mut result, &header, "span", "text-fuchsia-300")
                    .append_text(&mut result, &arguments);

                append_element(&mut result, &root, "p", "text-base")
                    .append_text(&mut result, "Type docs");
                let list = append_element(&mut result, &root, "ul", "list-disc");
                for constructor in constructors.iter() {
                    append_element(&mut result, &list, "li", "text-base")
                        .append_text(&mut result, &constructor.name.name());
                }

                append_element(&mut result, &root, "h1", "text-xl text-blue-600")
                    .append_text(&mut result, "Methods");
                let list = append_element(&mut result, &root, "ul", "list-disc");
                for method in methods.iter() {
                    append_element(&mut result, &list, "li", "text-base")
                        .append_text(&mut result, &method.name.name());
                }


                append_element(&mut result, &root, "h1", "text-xl text-green-600")
                    .append_text(&mut result, "Examples");
            }
            Documentation::Constructor { .. } => {}
            Documentation::Method { .. } => {}
            Documentation::ModuleMethod { .. } => {}
            Documentation::Function { .. } => {}
            Documentation::Local { .. } => {}
        },
    }
    root.display(&result)
}
