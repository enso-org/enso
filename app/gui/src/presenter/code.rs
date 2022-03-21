//! The module containing [`Code`] presenter. See [`crate::presenter`] documentation to know more
//! about presenters in general.

use crate::prelude::*;

use crate::controller::text::Notification;
use crate::executor::global::spawn_stream_handler;

use enso_frp as frp;
use ide_view as view;



// =============
// === Model ===
// =============

#[derive(Debug)]
struct Model {
    logger:     Logger,
    controller: controller::Text,
    view:       view::code_editor::View,
}

impl Model {
    fn new(controller: controller::Text, view: view::code_editor::View) -> Self {
        let logger = Logger::new("presenter::code");
        Self { logger, controller, view }
    }

    fn apply_change_from_view(&self, change: &enso_text::Change) {
        let converted = enso_text::Change { range: change.range, text: change.text.to_string() };
        if let Err(err) = self.controller.apply_text_change(converted) {
            error!(self.logger, "Error while applying text change: {err}");
        }
    }

    #[profile(Detail)]
    async fn emit_event_with_controller_code(&self, endpoint: &frp::Source<ImString>) {
        match self.controller.read_content().await {
            Ok(code) => endpoint.emit(ImString::new(code)),
            Err(err) => {
                error!(self.logger, "Error while updating code editor: {err}")
            }
        }
    }

    fn save_module(&self, content: String) {
        let logger = self.logger.clone_ref();
        let controller = self.controller.clone_ref();
        executor::global::spawn(async move {
            if let Err(err) = controller.store_content(content).await {
                error!(logger, "Error while saving module: {err}");
            }
        })
    }
}



// ============
// === Code ===
// ============

/// The presenter synchronizing Code Editor view with the module code in controllers.
#[derive(Debug)]
pub struct Code {
    _network: frp::Network,
    model:    Rc<Model>,
}

impl Code {
    /// Constructor. The returned structure works right away and does not need any initialization.
    #[profile(Task)]
    pub fn new(controller: controller::Text, project_view: &view::project::View) -> Self {
        let network = frp::Network::new("presenter::code");
        let view = project_view.code_editor().clone_ref();
        let model = Rc::new(Model::new(controller, view));

        let text_area = model.view.text_area();
        frp::extend! { network
            code_in_controller <- source::<ImString>();
            desynchronized <- all_with(&code_in_controller, &text_area.content, |controller, view|
                *controller != view.to_string()
            );
            text_area.set_content <+ code_in_controller.gate(&desynchronized).map(|s| s.to_string());

            maybe_change_to_apply <= text_area.changed;
            change_to_apply <- maybe_change_to_apply.gate(&desynchronized);
            eval change_to_apply ((change) model.apply_change_from_view(change));

            code_to_save <- code_in_controller.sample(&project_view.save_module);
            eval code_to_save ((code) model.save_module(code.to_string()));
        }



        Self { _network: network, model }
            .display_initial_code(code_in_controller.clone_ref())
            .setup_notification_handler(code_in_controller)
    }

    fn setup_notification_handler(self, code_in_controller: frp::Source<ImString>) -> Self {
        let weak = Rc::downgrade(&self.model);
        let notifications = self.model.controller.subscribe();
        spawn_stream_handler(weak, notifications, move |notification, model| {
            let endpoint_clone = code_in_controller.clone_ref();
            let model_clone = model.clone_ref();
            async move {
                match notification {
                    Notification::Invalidate =>
                        model_clone.emit_event_with_controller_code(&endpoint_clone).await,
                }
            }
        });
        self
    }

    #[profile(Detail)]
    fn display_initial_code(self, code_in_controller: frp::Source<ImString>) -> Self {
        let model = self.model.clone_ref();
        executor::global::spawn(async move {
            model.emit_event_with_controller_code(&code_in_controller).await
        });
        self
    }
}
