//! Side menu for Welcome Screen.
//!
//! Side menu contains a list of available projects and a "new project" button.

use ensogl::prelude::*;

use crate::ClickableElement;

use enso_frp as frp;
use ensogl::system::web;
use ensogl::system::web::traits::*;



// =============
// === Model ===
// =============

#[derive(Clone, CloneRef, Debug)]
pub struct Model {
    logger:             Logger,
    pub root_dom:       web::Element,
    new_project_button: ClickableElement,
    projects_list_dom:  web::Element,
    projects:           Rc<RefCell<Vec<ClickableElement>>>,
}

impl Model {
    /// Constructor.
    pub fn new(logger: Logger) -> Self {
        let root_dom = web::document.create_element_or_panic("aside");
        root_dom.set_class_name(crate::css_class::SIDE_MENU);
        let header = Self::create_header("Your projects");
        root_dom.append_or_warn(&header);
        let projects_list_dom = Self::create_projects_list();
        root_dom.append_or_warn(&projects_list_dom);
        let new_project_button = Self::create_new_project_button(&projects_list_dom);
        let projects = default();

        Self { logger, root_dom, projects_list_dom, projects, new_project_button }
    }

    pub fn set_projects_list(&self, projects: &[String], open_project: &frp::Any<String>) {
        self.clear_projects_list();
        for name in projects {
            self.add_projects_list_entry(name, open_project);
        }
    }

    fn clear_projects_list(&self) {
        for entry in self.projects.borrow_mut().iter() {
            entry.element.remove();
        }
        self.projects.borrow_mut().clear();
    }

    fn add_projects_list_entry(&self, name: &str, open_project: &frp::Any<String>) {
        let entry = Self::create_project_list_entry(name);
        let network = &entry.network;
        frp::extend! { network
            open_project <+ entry.click.constant(name.to_owned());
        }
        let new_project_button = &self.new_project_button;
        self.projects_list_dom.insert_before_or_warn(&entry, new_project_button);
        self.projects.borrow_mut().push(entry);
    }

    fn create_new_project_button(projects_list: &web::Element) -> ClickableElement {
        let element = web::document.create_element_or_panic("li");
        element.set_id(crate::css_id::NEW_PROJECT);
        element.set_inner_html(r#"<img src="/assets/new-project.svg" />Create a new project"#);
        projects_list.append_or_warn(&element);
        ClickableElement::new(element)
    }

    fn create_header(text: &str) -> web::Element {
        let header = web::document.create_element_or_panic("h2");
        header.set_text_content(Some(text));
        header
    }

    fn create_projects_list() -> web::Element {
        web::document.create_element_or_panic("ul")
    }

    fn create_project_list_entry(project_name: &str) -> ClickableElement {
        let element = web::document.create_element_or_panic("li");
        element.set_inner_html(&format!(r#"<img src="assets/project.svg"/> {}"#, project_name));
        ClickableElement::new(element)
    }
}



// ===========
// === FRP ===
// ===========

ensogl::define_endpoints! {
    Input {
        // Set displayed list of projects.
        set_projects_list(Vec<String>),
    }
    Output {
        // New project button was clicked.
        new_project(),
        // Project with `name` was selected from the projects list.
        open_project(String),
    }
}



// ================
// === SideMenu ===
// ================

/// Side menu for Welcome Screen. Contains a list of available projects and a "new project" button.
#[derive(Debug, Clone, CloneRef)]
pub struct SideMenu {
    pub model: Model,
    pub frp:   Frp,
}

impl Deref for SideMenu {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}

impl SideMenu {
    pub fn new(logger: &Logger) -> Self {
        let logger = Logger::new_sub(logger, "SideMenu");
        let frp = Frp::new();
        let model = Model::new(logger);

        let network = &frp.network;
        frp::extend! { network
            let open_project = &frp.output.source.open_project;
            eval frp.set_projects_list([model, open_project] (list) model.set_projects_list(list, &open_project));

            frp.output.source.new_project <+ model.new_project_button.click;
        }

        Self { frp, model }
    }
}
