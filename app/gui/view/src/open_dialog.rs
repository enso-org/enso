//! A module with [`OpenDialog`] component.
pub mod project_list;

use crate::prelude::*;

use enso_frp as frp;
use ensogl::application::Application;
use ensogl::display;
use ensogl::display::shape::StyleWatchFrp;
use ensogl_gui_component::file_browser::FileBrowser;
use ensogl_hardcoded_theme as theme;



// ==================
// === OpenDialog ===
// ==================

/// An Open Dialog GUI component.
///
/// This is component bounding together projects-to-open list and the file browser. It does not
/// provide frp endpoints by its own: you should just go directly to the `project_list` or
/// `file_browser` field.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct OpenDialog {
    logger:           Logger,
    network:          frp::Network,
    pub project_list: project_list::ProjectList,
    pub file_browser: FileBrowser,
    display_object:   display::object::Instance,
    style_watch:      StyleWatchFrp,
}

impl OpenDialog {
    /// Create Open Dialog component.
    pub fn new(app: &Application) -> Self {
        let logger = Logger::new("OpenDialog");
        let network = frp::Network::new("OpenDialog");
        let style_watch = StyleWatchFrp::new(&app.display.scene().style_sheet);
        let project_list = project_list::ProjectList::new(app);
        let file_browser = FileBrowser::new();
        // Once FileBrowser will be implemented as component, it should be instantiated this way:
        //let file_browser   = app.new_view::<FileBrowser>();

        let display_object = display::object::Instance::new(&logger);

        display_object.add_child(&project_list);
        display_object.add_child(&file_browser);
        app.display.scene().layers.panel.add_exclusive(&display_object);

        use theme::application as theme_app;
        let project_list_width = style_watch.get_number(theme_app::project_list::width);
        let file_browser_width = style_watch.get_number(theme_app::file_browser::width);
        let gap_between_panels = style_watch.get_number(theme_app::open_dialog::gap_between_panels);
        frp::extend! { network
            init  <- source::<()>();
            width <- all_with4(&project_list_width,&file_browser_width,&gap_between_panels,&init,
                |pw,fw,g,()| *pw + *g + *fw
            );
            project_list_x <- all_with(&width,&project_list_width,|w,pw| - *w / 2.0 + *pw / 2.0);
            file_browser_x <- all_with(&width,&file_browser_width, |w,fw| w / 2.0 - *fw / 2.0);

            eval project_list_x ((x) project_list.set_position_x(*x));
            eval file_browser_x ((x) file_browser.set_position_x(*x));
        }
        init.emit(());
        Self { logger, network, project_list, file_browser, display_object, style_watch }
    }
}

impl display::Object for OpenDialog {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
