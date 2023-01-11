use enso_prelude::*;
use horrorshow::html;
use horrorshow::owned_html;
use horrorshow::prelude::*;

use enso_suggestion_database::documentation_ir::Documentation;
use enso_suggestion_database::documentation_ir::EntryDocumentation;
use enso_suggestion_database::documentation_ir::TypeDocumentation;

const ICON_TYPE: &str = include_str!("../assets/icon-type.svg");
const ICON_LIB: &str = include_str!("../assets/icon-lib.svg");

fn header(icon: &'static str, content: impl Render) -> impl Render {
    owned_html! {
        div(class="flex flex-row items-center") {
            : Raw(icon);
            div(class="ml-2") {
                : &content;
            }
        }
    }
}

pub fn render(docs: EntryDocumentation) -> String {
    match docs {
        EntryDocumentation::Placeholder(_) => String::from("Placeholder"),
        EntryDocumentation::Docs(docs) => match docs {
            Documentation::Module(_) => owned_html! {
                : "Module docs";
            }
            .to_string(),
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

                let arguments =
                    format!(" {}", arguments.iter().map(|arg| arg.name.clone()).join(", "));
                let type_header = || {
                    html! {
                        span(class="text-2xl font-bold") {
                            span(class="text-fuchsia-600") {
                                : name.name()
                            }
                            span(class="text-fuchsia-300") {
                                : &arguments
                            }
                        }
                    }
                };
                let methods_header = || {
                    owned_html! {
                        h1(class="text-xl font-semibold text-blue-600") {
                            : "Methods"
                        }
                    }
                };
                let examples_header = || {
                    owned_html! {
                        h1(class="text-xl font-semibold text-green-600") {
                            : "Examples"
                        }
                    }
                };
                owned_html! {
                    div(class="docs") {
                        : header(ICON_TYPE, type_header());
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
                        : header(ICON_LIB, methods_header());
                        ul(class="list-disc") {
                            @ for method in methods.iter() {
                                li(class="text-base") {
                                    : method.name.name()
                                }
                            }
                        }
                        : header(ICON_LIB, examples_header());
                    }
                }
                .to_string()
            }
            _ => String::from("Not implemented"),
        },
    }
}
