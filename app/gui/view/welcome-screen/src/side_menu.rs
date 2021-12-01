//! Side menu for Welcome Screen.
//!
//! Side menu contains a list of available projects and a "new project" button.

use ensogl::prelude::*;

use crate::ClickClosure;
use crate::Frp;

use ensogl::system::web;
use ensogl::system::web::NodeInserter;
use wasm_bindgen::closure::Closure;
use wasm_bindgen::JsCast;
use web_sys::Element;
use web_sys::MouseEvent;



// ================
// === SideMenu ===
// ================

/// Side menu for Welcome Screen. Contains a list of available projects and a "new project" button.
#[derive(Clone, CloneRef, Debug)]
pub struct SideMenu {
    logger:             Logger,
    pub root_dom:       Element,
    frp:                Frp,
    new_project_button: Element,
    projects_list:      Element,
    closures:           Rc<RefCell<Vec<ClickClosure>>>,
}

impl SideMenu {
    /// Constructor.
    pub fn new(logger: &Logger, frp: Frp) -> Self {
        let logger = Logger::new_sub(logger, "SideMenu");
        let root_dom = web::create_element("aside");
        root_dom.set_class_name(crate::css_class::SIDE_MENU);
        let header = Self::create_header("Your projects");
        root_dom.append_or_warn(&header, &logger);
        let projects_list = Self::create_projects_list();
        root_dom.append_or_warn(&projects_list, &logger);
        let new_project_button = Self::create_new_project_button(&logger, &projects_list);
        let closures = default();

        let menu = Self { logger, root_dom, frp, projects_list, new_project_button, closures };
        menu.setup_new_project_event_listener();
        menu
    }

    pub fn set_projects_list(&self, projects: &[String]) {
        self.clear_projects_list();
        let new_button = &self.new_project_button;
        for project in projects {
            let node = Self::create_project_list_entry(project);
            self.setup_open_project_event_listener(&node, project);
            self.projects_list.insert_before_or_warn(&node, new_button, &self.logger);
        }
    }

    fn clear_projects_list(&self) {
        let children = self.projects_list.children();
        let items = (0..children.length()).flat_map(|i| children.item(i));
        let not_new_project_button = |item: &Element| *item != self.new_project_button;
        let items_to_remove: Vec<_> = items.filter(not_new_project_button).collect();
        for item in items_to_remove {
            item.remove();
        }
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

    fn setup_new_project_event_listener(&self) {
        let frp = self.frp.clone_ref();
        let closure = Box::new(move |_event: MouseEvent| {
            frp.create_project.emit(None);
        });
        let closure: ClickClosure = Closure::wrap(closure);
        let callback = closure.as_ref().unchecked_ref();
        if self.new_project_button.add_event_listener_with_callback("click", callback).is_err() {
            error!(self.logger, "Could not add create project event listener.");
        }
        self.closures.borrow_mut().push(closure);
    }

    fn setup_open_project_event_listener(&self, entry: &Element, project_name: &str) {
        let frp = self.frp.clone_ref();
        let project = project_name.to_owned();
        let closure = Box::new(move |_event: MouseEvent| {
            frp.open_project.emit(project.clone());
        });
        let closure: ClickClosure = Closure::wrap(closure);
        let callback = closure.as_ref().unchecked_ref();
        if entry.add_event_listener_with_callback("click", callback).is_err() {
            error!(self.logger, "Could not add open project event listener for {project_name}.");
        }
        self.closures.borrow_mut().push(closure);
    }

    fn create_project_list_entry(project_name: &str) -> Element {
        let node = web::create_element("li");
        node.set_inner_html(&format!(r#"<img src="assets/project.svg"/> {}"#, project_name));
        node
    }
}
