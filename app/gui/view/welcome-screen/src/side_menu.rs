//! Side menu for Welcome Screen.
//!
//! Side menu contains a list of available projects and a "new project" button.

use ensogl::prelude::*;

use crate::ClickClosure;

use ensogl::system::web;
use ensogl::system::web::NodeInserter;
use web_sys::Element;



// ================
// === SideMenu ===
// ================

/// Side menu for Welcome Screen. Contains a list of available projects and a "new project" button.
#[derive(Clone, CloneRef, Debug)]
pub struct SideMenu {
    logger:             Logger,
    pub root_dom:       Element,
    new_project_button: Element,
    projects_list:      Element,
    closures:           Rc<RefCell<Vec<ClickClosure>>>,
}

impl SideMenu {
    /// Constructor.
    pub fn new(logger: &Logger) -> Self {
        let logger = Logger::new_sub(logger, "SideMenu");
        let root_dom = web::create_element("aside");
        root_dom.set_class_name(crate::css_class::SIDE_MENU);
        let header = Self::create_header("Your projects");
        root_dom.append_or_warn(&header, &logger);
        let projects_list = Self::create_projects_list();
        root_dom.append_or_warn(&projects_list, &logger);
        let new_project_button = Self::create_new_project_button(&logger, &projects_list);

        Self { logger, root_dom, projects_list, new_project_button, closures: default() }
    }

    fn create_header(text: &str) -> Element {
        let header = web::create_element("h2");
        header.set_text_content(Some(text));
        header
    }

    fn create_projects_list() -> Element {
        web::create_element("ul")
    }

    fn create_new_project_button(logger: &Logger, projects_list: &Element) -> Element {
        let button = web::create_element("li");
        button.set_id(crate::css_id::NEW_PROJECT);
        button.set_inner_html(r#"<img src="/assets/new-project.svg" />Create a new project"#);
        projects_list.append_or_warn(&button, logger);
        button
    }
}
