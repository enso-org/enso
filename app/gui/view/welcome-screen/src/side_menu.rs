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
        root_dom.set_class_name("side-menu");


        // === Header ===

        let header = {
            let header = web::create_element("h2");
            header.set_text_content(Some("Your projects"));
            header
        };
        root_dom.append_or_warn(&header, &logger);


        // === Projects list ===

        let projects_list = {
            let projects_list = web::create_element("ul");
            projects_list.set_id("projects-list");
            projects_list
        };


        // === New project button ===

        let new_project_button = {
            let button = web::create_element("li");
            button.set_id("projects-list-new-project");
            button.set_inner_html(r#"<img src="/assets/new-project.svg" />Create a new project"#);
            projects_list.append_or_warn(&button, &logger);
            button
        };
        root_dom.append_or_warn(&projects_list, &logger);

        Self { logger, root_dom, projects_list, new_project_button, closures: default() }
    }
}
