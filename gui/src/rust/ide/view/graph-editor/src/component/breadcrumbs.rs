//! This module provides a view for breadcrumbs, enabling us to know which node the graph being
//! edited belongs to and navigating through them.

use crate::prelude::*;

pub mod breadcrumb;
pub mod project_name;

pub use breadcrumb::Breadcrumb;
pub use project_name::ProjectName;

use crate::LocalCall;

use enso_frp as frp;
use enso_protocol::language_server::MethodPointer;
use ensogl::application::Application;
use ensogl::data::color;
use ensogl::display::camera::Camera2d;
use ensogl::display::object::ObjectOps;
use ensogl::display::shape::*;
use ensogl::display;
use ensogl::gui::component;
use ensogl::gui::cursor;
use logger::Logger;
use std::cmp::Ordering;



// =================
// === Constants ===
// =================

// FIXME[dg] hardcoded literal for glyph of height 12.0. Copied from port.rs
const GLYPH_WIDTH       : f32 = 7.224_609_4;
const VERTICAL_MARGIN   : f32 = GLYPH_WIDTH;
const HORIZONTAL_MARGIN : f32 = GLYPH_WIDTH;
const TEXT_SIZE         : f32 = 12.0;



// ========================
// === RelativePosition ===
// ========================

#[derive(Debug,Clone,Copy)]
enum RelativePosition {
    LEFT,
    RIGHT
}



// ==================
// === Background ===
// ==================

mod background {
    use super::*;

    ensogl::define_shape_system! {
        () {
            let gray     = 34.0/255.0;
            let bg_color = color::Rgba::new(gray,gray,gray,1.0);
            Plane().fill(bg_color).into()
        }
    }
}



// ===========
// === Frp ===
// ===========

ensogl::define_endpoints! {
    Input {
        /// Pushes a new breadcrumb after the selected breadcrumb. If the pushed breadcrumb already
        /// exists as the next one of the stack, it's just selected. If the next breadcrumb isn't the
        /// breadcrumb being pushed, any existing breadcrumb following the currently selected breadcrumb
        /// is removed from the panel.
        push_breadcrumb             (Option<LocalCall>),
        /// Pops the selected breadcrumb.
        pop_breadcrumb              (),
        /// Signalizes a mouse press happened outside the breadcrumb panel. It's used to finish project
        /// renaming, committing the name in text field.
        outside_press               (),
        /// Signalizes we want to cancel project name renaming, bringing back the project name before
        /// editing.
        cancel_project_name_editing (),
        /// Signalizes we want to start editing the project name. Adds a cursor to the text edit
        /// field at the mouse position.
        start_project_name_editing (),
        /// Sets the project name.
        project_name                (String),
        /// Select the breadcrumb by its index.
        select_breadcrumb           (usize),
        /// Same as `FrpInputs::push_breadcrumb`, but pushes a hardcoded breadcrumb if
        /// `Option<LocalCall>` is `None`.
        debug_push_breadcrumb       (Option<LocalCall>),
        /// Pops the breadcrumb view without sending signals to the controller.
        debug_pop_breadcrumb        (),
        /// Selects the breadcrumb by its index without sending signals to the controller.
        debug_select_breadcrumb     (usize),
        /// Indicates the IDE is in text edit mode.
        ide_text_edit_mode          (bool)
    }
    Output {
        /// Signalizes when a new breadcrumb is pushed.
        breadcrumb_push   (Option<LocalCall>),
        /// Signalizes when a breadcrumb is popped.
        breadcrumb_pop    (),
        /// Signalizes when project name is changed.
        project_name      (String),
        /// Signalizes when a breadcrumb is selected, returning a tuple with the amount of breadcrumbs
        /// to be popped, in case the selection happens on the left of the currently selected
        /// breadcrumb, or else a vector of existing breadcrumbs to be pushed.
        breadcrumb_select ((usize,Vec<Option<LocalCall>>)),
        /// Indicates the pointer style that should be shown based on the interactions with the
        /// breadcrumb.
        pointer_style      (cursor::Style),
        /// Indicates whether the cursor hovers over the project name.
        project_name_hovered (bool),
        /// Indicates whether the project name was clicked.
        project_mouse_down (),
    }
}


// ========================
// === BreadcrumbsModel ===
// ========================

/// Breadcrumbs panel model.
#[derive(Debug,Clone,CloneRef)]
pub struct BreadcrumbsModel {
    logger                : Logger,
    /// The breadcrumbs panel display object.
    display_object        : display::object::Instance,
    background            : component::ShapeView<background::Shape>,
    project_name          : ProjectName,
    /// A container for all the breadcrumbs after project name. This contained and all its
    /// breadcrumbs are moved when project name component is resized.
    breadcrumbs_container : display::object::Instance,
    app                   : Application,
    breadcrumbs           : Rc<RefCell<Vec<Breadcrumb>>>,
    frp_inputs            : FrpInputs,
    current_index         : Rc<Cell<usize>>,
    camera                : Camera2d,
}

impl BreadcrumbsModel {
    /// Constructor.
    pub fn new(app:Application, frp:&Frp) -> Self {
        let scene                 = app.display.scene();
        let project_name          = app.new_view();
        let logger                = Logger::new("Breadcrumbs");
        let display_object        = display::object::Instance::new(&logger);
        let breadcrumbs_container = display::object::Instance::new(&logger);
        let scene                 = scene.clone_ref();
        let breadcrumbs           = default();
        let frp_inputs            = frp.input.clone_ref();
        let current_index         = default();
        let camera                = scene.camera().clone_ref();
        let background            = component::ShapeView::<background::Shape>::new(&logger,&scene);

        Self{logger,display_object,app,breadcrumbs,project_name,breadcrumbs_container,
            frp_inputs,current_index,camera,background}.init()
    }

    fn init(self) -> Self {
        self.add_child(&self.project_name);
        self.add_child(&self.breadcrumbs_container);
        self.project_name.set_position_x(HORIZONTAL_MARGIN);
        self
    }

    fn camera_changed(&self) {
        let camera     = &self.camera;
        let screen     = camera.screen();
        let x_position = -screen.width/2.0;
        let y_position = screen.height/2.0;
        self.set_position(Vector3(x_position.round(),y_position.round(),0.0));
    }

    fn width(&self) -> f32 {
        self.breadcrumbs.borrow().iter().map(|breadcrumb| breadcrumb.width()).sum()
    }

    fn relayout_for_project_name_width(&self, width:f32) {
        self.breadcrumbs_container.set_position_x((HORIZONTAL_MARGIN+width).round());
    }

    fn get_breadcrumb(&self, index:usize) -> Option<Breadcrumb> {
        (index > 0).as_option().and_then(|_|
            self.breadcrumbs.borrow_mut().get(index - 1).map(|breadcrumb| breadcrumb.clone_ref())
        )
    }

    /// Selects the breadcrumb identified by its `index` and returns `(popped_count,local_calls)`,
    /// where `popped_count` is the number of breadcrumbs in the right side of `index` that needs to
    /// be popped or a list of `LocalCall`s identifying the breadcrumbs we need to push.
    fn select_breadcrumb(&self, index:usize) -> (usize,Vec<Option<LocalCall>>) {
        debug!(self.logger,"Selecting breadcrumb #{index}.");
        let current_index = self.current_index.get();
        match index.cmp(&current_index) {
            Ordering::Less    => (current_index - index, default()),
            Ordering::Equal   => default(),
            Ordering::Greater => {
                let mut local_calls = Vec::new();
                for index in current_index..index {
                    let info = self.breadcrumbs.borrow().get(index).map(|breadcrumb| {
                        let definition = breadcrumb.info.method_pointer.clone();
                        let call       = breadcrumb.info.expression_id;
                        LocalCall{call,definition}
                    }).as_ref().cloned();
                    if info.is_some() {
                        local_calls.push(info);
                    } else {
                        error!(self.logger, "LocalCall info is not present.");
                        self.remove_breadcrumbs_history_beginning_from(index);
                        break;
                    }
                }
                (default(),local_calls)
            },
        }
    }

    /// Pushes a breadcrumb and returns the index of the previously selected breadcrumb and the
    /// index of the newly selected one in the form of (old,new).
    fn push_breadcrumb(&self, local_call:&Option<LocalCall>) -> Option<(usize,usize)> {
        local_call.as_ref().map(|local_call| {
            let method_pointer = &local_call.definition;
            let expression_id  = &local_call.call;
            let old_index      = self.current_index.get();
            let new_index      = old_index + 1;

            let breadcrumb_exists =
                self.breadcrumbs.borrow_mut().get(old_index).contains_if(|breadcrumb| {
                    breadcrumb.info.expression_id == *expression_id
                });

            if breadcrumb_exists {
                debug!(self.logger, "Entering an existing {method_pointer.name} breadcrumb.");
            } else {
                debug!(self.logger, "Creating a new {method_pointer.name} breadcrumb.");
                self.remove_breadcrumbs_history_beginning_from(self.current_index.get());
                let breadcrumb       = Breadcrumb::new(&self.app, method_pointer, expression_id);
                let network          = &breadcrumb.frp.network;
                let breadcrumb_index = new_index;
                let frp_inputs       = &self.frp_inputs;

                frp::extend! { network
                    eval_ breadcrumb.frp.outputs.clicked(
                        frp_inputs.select_breadcrumb.emit(breadcrumb_index);
                    );
                }

                debug!(self.logger, "Pushing {breadcrumb.info.method_pointer.name} breadcrumb.");
                breadcrumb.set_position_x(self.width().round());
                self.breadcrumbs_container.add_child(&breadcrumb);
                self.breadcrumbs.borrow_mut().push(breadcrumb);
            }
            self.current_index.set(new_index);
            (old_index,new_index)
        })
    }

    /// Selects the breadcrumb, without signalizing the controller, identified by its `index` and
    /// returns `(popped_count,local_calls)`, where `popped_count` is the number of breadcrumbs in
    /// the right side of `index` that needs to be popped or a list of `LocalCall`s identifying the
    /// breadcrumbs we need to push.
    fn debug_select_breadcrumb(&self,index:usize) -> (usize,Vec<Option<LocalCall>>) {
        self.select_breadcrumb(index)
    }

    /// Pushes a breadcrumb, without signalizing the controller, and returns the index of the
    /// previously selected breadcrumb and the index of the newly selected one in the form of
    /// `(old,new)`.
    fn debug_push_breadcrumb(&self, local_call:&Option<LocalCall>) -> Option<(usize,usize)> {
        let is_new_breadcrumb = local_call.is_none();
        let local_call        = local_call.clone().or_else(|| {
            let defined_on_type = default();
            let module          = default();
            let name            = "Hardcoded".to_string();
            let method_pointer  = MethodPointer{defined_on_type,module,name};
            let definition      = method_pointer.into();
            let call            = uuid::Uuid::new_v4();
            Some(LocalCall{definition,call})
        });
        let result = self.push_breadcrumb(&local_call);

        if is_new_breadcrumb {
            result.as_ref().map(|(_, new_index)| self.get_breadcrumb(*new_index).map(|breadcrumb| {

                let new_index = *new_index;
                let network   = &breadcrumb.frp.network;
                let frp_inputs       = &self.frp_inputs;
                frp::extend! { network
                    eval_ breadcrumb.frp.outputs.clicked(
                        frp_inputs.debug_select_breadcrumb.emit(new_index);
                    );
                }
            }));
        }
        result
    }

    /// Pops a breadcrumb, without signalizing the controller, and returns the index of the
    /// previously selected breadcrumb and the index of the newly selected one in the form of
    /// `(old,new)`.
    fn debug_pop_breadcrumb(&self) -> Option<(usize,usize)> {
        self.pop_breadcrumb()
    }

    /// Pops a breadcrumb and returns the index of the previously selected breadcrumb and the
    /// index of the newly selected one in the form of (old,new).
    fn pop_breadcrumb(&self) -> Option<(usize,usize)> {
        debug!(self.logger, "Popping {self.current_index.get()}");
        (self.current_index.get()>0).as_option().map(|_| {
            debug!(self.logger, "Popping breadcrumb view.");
            let old_index = self.current_index.get();
            let new_index = old_index - 1;
            self.current_index.set(new_index);
            (old_index,new_index)
        })
    }

    fn remove_breadcrumbs_history_beginning_from(&self, index:usize) {
        for breadcrumb in self.breadcrumbs.borrow_mut().split_off(index) {
            debug!(self.logger, "Removing {breadcrumb.info.method_pointer.name}.");
            breadcrumb.unset_parent();
        }
    }
}

impl display::Object for BreadcrumbsModel {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}



// ===================
// === Breadcrumbs ===
// ===================

/// The breadcrumbs panel's view used for visualizing the breadcrumbs and navigating them.
#[derive(Debug,Clone,CloneRef)]
#[allow(missing_docs)]
pub struct Breadcrumbs {
    model   : Rc<BreadcrumbsModel>,
    frp : Frp
}

impl Breadcrumbs {
    /// Constructor.
    pub fn new(app:Application) -> Self {
        let scene   = app.display.scene().clone_ref();
        let frp     = Frp::new();
        let model   = Rc::new(BreadcrumbsModel::new(app,&frp));
        let network = &frp.network;

        // === Breadcrumb selection ===

        frp::extend! { network

            // === Selecting ===

            _breadcrumb_selection <- frp.select_breadcrumb.map(f!([model,frp](index)
                frp.source.breadcrumb_select.emit(model.select_breadcrumb(*index));
            ));


            // === Stack Operations ===

            push_indices <= frp.push_breadcrumb.map(f!((local_call)
                model.push_breadcrumb(local_call))
            );
            pop_indices <= frp.pop_breadcrumb.map(f_!(model.pop_breadcrumb()));
            debug_push_indices <= frp.input.debug_push_breadcrumb.map(f!((local_call)
                model.debug_push_breadcrumb(local_call)
            ));
            debug_pop_indices <= frp.input.debug_pop_breadcrumb.map(f_!(model.debug_pop_breadcrumb()));

            indices <- any4(&push_indices,&pop_indices,&debug_push_indices,&debug_pop_indices);
            old_breadcrumb <- indices.map(f!([model] (indices) {
                (Some(*indices),model.get_breadcrumb(indices.0))
            }));
            new_breadcrumb <- indices.map(f!((indices) model.get_breadcrumb(indices.1)));
            eval old_breadcrumb([model] ((indices,breadcrumb)) {
                if let Some(breadcrumb) = breadcrumb.as_ref() {
                    indices.map(|indices| breadcrumb.frp.deselect.emit((indices.0,indices.1)));
                } else {
                    model.project_name.frp.deselect.emit(());
                }
            });
            eval new_breadcrumb([model] (breadcrumb) {
                if let Some(breadcrumb) = breadcrumb.as_ref() {
                    breadcrumb.frp.select.emit(());
                    breadcrumb.frp.fade_in.emit(());
                } else {
                    model.project_name.frp.select.emit(());
                }
            });


            // === Project Name ===

            eval frp.input.project_name((name) model.project_name.set_name.emit(name));
            frp.source.project_name <+ model.project_name.output.name;

            eval_ frp.input.start_project_name_editing( model.project_name.start_editing.emit(()) );
            eval frp.ide_text_edit_mode((value) model.project_name.ide_text_edit_mode.emit(value) );

            frp.source.project_name_hovered <+ model.project_name.is_hovered;
            frp.source.project_mouse_down   <+ model.project_name.mouse_down;

            // === GUI Update ===

            eval model.project_name.frp.output.width((width)
                model.relayout_for_project_name_width(*width)
            );


            // === User Interaction ===

            eval_ model.project_name.frp.output.mouse_down(frp.select_breadcrumb.emit(0));
            eval_ frp.cancel_project_name_editing(model.project_name.frp.cancel_editing.emit(()));
            eval_ frp.outside_press(model.project_name.frp.outside_press.emit(()));
            
            popped_count <= frp.output.breadcrumb_select.map(|selected| (0..selected.0).collect_vec());
            local_calls  <= frp.output.breadcrumb_select.map(|selected| selected.1.clone());
            eval_ popped_count(frp.source.breadcrumb_pop.emit(()));
            eval local_calls((local_call) frp.source.breadcrumb_push.emit(local_call));


            // === Select ===

            selected_project_name <- model.project_name.frp.output.mouse_down.map(f_!([model]
                model.debug_select_breadcrumb(0))
            );
            selected_breadcrumb   <- frp.input.debug_select_breadcrumb.map(f!((index)
                model.debug_select_breadcrumb(*index))
            );
            selected <- any(&selected_project_name,&selected_breadcrumb);

            popped_count <= selected.map(|selected| (0..selected.0).collect_vec());
            local_calls  <= selected.map(|selected| selected.1.clone());
            eval_ popped_count(frp.input.debug_pop_breadcrumb.emit(()));
            eval local_calls((local_call) frp.input.debug_push_breadcrumb.emit(local_call));


            // === Relayout ===

            eval_ scene.frp.camera_changed(model.camera_changed());

            eval model.project_name.frp.output.width ((width) {
                model.relayout_for_project_name_width(*width)
            });


            // === Pointer style ===

            frp.source.pointer_style <+ model.project_name.frp.output.pointer_style;

        }

        Self{frp,model}
    }
}

impl display::Object for Breadcrumbs {
    fn display_object(&self) -> &display::object::Instance {
        &self.model.display_object
    }
}

impl Deref for Breadcrumbs {
    type Target = Frp;
    fn deref(&self) -> &Self::Target {
        &self.frp
    }
}
