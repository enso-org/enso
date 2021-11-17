//! Side menu for Welcome Screen.
//!
//! Side menu contains a list of available projects and "new project" button.

use ensogl::prelude::*;

use crate::ClickClosure;
use crate::Frp;

use ensogl::system::web;
use ensogl::system::web::NodeInserter;
use web_sys::Element;



// ================
// === SideMenu ===
// ================

/// Side menu for Welcome Screen. Contains a list of available projects and "new project" button.
#[derive(Clone, CloneRef, Debug)]
pub struct SideMenu {
    logger:             Logger,
    /// Root DOM element of the side menu.
    pub root:           Element,
    new_project_button: Element,
    projects_list:      Element,
    closures:           Rc<RefCell<Vec<ClickClosure>>>,
}

impl SideMenu {
    /// Constructor.
    // TODO [vitvakatu]: _frp argument is not used now, but will be in the next tasks.
    pub fn new(logger: &Logger, _frp: &Frp) -> Self {
        let logger = Logger::new_sub(logger, "SideMenu");
        let root = web::create_element("aside");
        root.set_class_name("side-menu");


        // === Header ===

        let header = {
            let header = web::create_element("h2");
            header.set_text_content(Some("Your projects"));
            header
        };
        root.append_or_warn(&header, &logger);


        // === Projects list ===

        let projects_list = {
            let projects_list = web::create_element("ul");
            projects_list.set_id("projects-list");
            projects_list
        };


        // === New project button ===

        let new_project_button = {
            let new_project_button = web::create_element("li");
            new_project_button.set_id("projects-list-new-project");
            new_project_button
                .set_inner_html(r#"<img src="/assets/new-project.svg" />Create a new project"#);
            projects_list.append_or_warn(&new_project_button, &logger);
            new_project_button
        };
        root.append_or_warn(&projects_list, &logger);

        Self { logger, root, projects_list, new_project_button, closures: default() }
    }
}
