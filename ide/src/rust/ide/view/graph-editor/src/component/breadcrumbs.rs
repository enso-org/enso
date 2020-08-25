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
use ensogl::data::color;
use ensogl::display;
use ensogl::display::camera::Camera2d;
use ensogl::display::object::ObjectOps;
use ensogl::display::scene::Scene;
use ensogl::display::shape::*;
use ensogl::display::shape::text::text_field::FocusManager;
use ensogl::gui::component;
use logger::AnyLogger;
use logger::enabled::Logger;
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



// =================
// === FrpInputs ===
// =================

/// Breadcrumbs panel frp network inputs.
#[derive(Debug,Clone,CloneRef)]
pub struct FrpInputs {
    /// Pushes a new breadcrumb after the selected breadcrumb. If the pushed breadcrumb already
    /// exists as the next one of the stack, it's just selected. If the next breadcrumb isn't the
    /// breadcrumb being pushed, any existing breadcrumb following the currently selected breadcrumb
    /// is removed from the panel.
    pub push_breadcrumb             : frp::Source<Option<LocalCall>>,
    /// Pops the selected breadcrumb.
    pub pop_breadcrumb              : frp::Source,
    /// Signalizes a mouse press happened outside the breadcrumb panel. It's used to finish project
    /// renaming, committing the name in text field.
    pub outside_press               : frp::Source,
    /// Signalizes we want to cancel project name renaming, bringing back the project name before
    /// editing.
    pub cancel_project_name_editing : frp::Source,
    /// Sets the project name.
    pub project_name                : frp::Source<String>,
    /// Select the breadcrumb by its index.
    pub select_breadcrumb           : frp::Source<usize>
}

impl FrpInputs {
    /// Constructor.
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! {network
            push_breadcrumb             <- source();
            pop_breadcrumb              <- source();
            outside_press               <- source();
            cancel_project_name_editing <- source();
            project_name                <- source();
            select_breadcrumb           <- source();
        }
        Self{push_breadcrumb,pop_breadcrumb,outside_press,cancel_project_name_editing,project_name,
            select_breadcrumb}
    }
}

/// Breadcrumbs panel frp network debug inputs. Used to test the breadcrumbs panel view without
/// a controller.
#[derive(Debug,Clone,CloneRef)]
pub struct DebugFrpInputs {
    /// Same as `FrpInputs::push_breadcrumb`, but pushes a hardcoded breadcrumb if
    /// `Option<LocalCall>` is `None`.
    pub push_breadcrumb   : frp::Source<Option<LocalCall>>,
    /// Pops the breadcrumb view without sending signals to the controller.
    pub pop_breadcrumb    : frp::Source,
    /// Selects the breadcrumb by its index without sending signals to the controller.
    pub select_breadcrumb : frp::Source<usize>
}

impl DebugFrpInputs {
    /// Constructor.
    pub fn new(network:&frp::Network) -> Self {
        frp::extend!{ network
            push_breadcrumb   <- source();
            pop_breadcrumb    <- source();
            select_breadcrumb <- source();
        }
        Self{push_breadcrumb,pop_breadcrumb,select_breadcrumb}
    }
}


// ==================
// === FrpOutputs ===
// ==================

/// Breadcrumbs panel frp network outputs.
#[derive(Debug,Clone,CloneRef)]
pub struct FrpOutputs {
    /// Signalizes when a new breadcrumb is pushed.
    pub breadcrumb_push   : frp::Source<Option<LocalCall>>,
    /// Signalizes when a breadcrumb is popped.
    pub breadcrumb_pop    : frp::Source,
    /// Signalizes when project name is changed.
    pub project_name      : frp::Any<String>,
    /// Signalizes when a breadcrumb is selected, returning a tuple with the amount of breadcrumbs
    /// to be popped, in case the selection happens on the left of the currently selected
    /// breadcrumb, or else a vector of existing breadcrumbs to be pushed.
    pub breadcrumb_select : frp::Source<(usize,Vec<Option<LocalCall>>)>
}

impl FrpOutputs {
    /// Constructor.
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! {network
            breadcrumb_push   <- source();
            breadcrumb_pop    <- source();
            project_name      <- any(...);
            breadcrumb_select <- source();
        }
        Self{breadcrumb_push,breadcrumb_pop,project_name,breadcrumb_select}
    }
}



// ===========
// === Frp ===
// ===========

/// A breadcrumbs panel frp structure with its endpoints and network representation.
#[derive(Debug,Clone,CloneRef)]
#[allow(missing_docs)]
pub struct Frp {
    pub inputs  : FrpInputs,
    pub outputs : FrpOutputs,
    /// Frp debug inputs, used for testing the view without the controller.
    pub debug   : DebugFrpInputs,
    pub network : frp::Network,
}

impl Deref for Frp {
    type Target = FrpInputs;
    fn deref(&self) -> &Self::Target {
        &self.inputs
    }
}

impl Frp {
    /// Constructor.
    pub fn new() -> Self {
        let network = frp::Network::new();
        let inputs  = FrpInputs::new(&network);
        let outputs = FrpOutputs::new(&network);
        let debug   = DebugFrpInputs::new(&network);
        Self{network,inputs,outputs,debug}
    }
}

impl Default for Frp {
    fn default() -> Self {
        Self::new()
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
    scene                 : Scene,
    breadcrumbs           : Rc<RefCell<Vec<Breadcrumb>>>,
    frp_inputs            : FrpInputs,
    frp_debug             : DebugFrpInputs,
    current_index         : Rc<Cell<usize>>,
    camera                : Camera2d,
}

impl BreadcrumbsModel {
    /// Constructor.
    pub fn new<'t,S:Into<&'t Scene>>(scene:S, frp:&Frp, focus_manager:&FocusManager) -> Self {
        let scene                 = scene.into();
        let project_name          = ProjectName::new(scene,focus_manager);
        let logger                = Logger::new("Breadcrumbs");
        let display_object        = display::object::Instance::new(&logger);
        let breadcrumbs_container = display::object::Instance::new(&logger);
        let scene                 = scene.clone_ref();
        let breadcrumbs           = default();
        let frp_inputs            = frp.inputs.clone_ref();
        let frp_debug             = frp.debug.clone_ref();
        let current_index         = default();
        let camera                = scene.camera().clone_ref();
        let background            = component::ShapeView::<background::Shape>::new(&logger,&scene);

        Self{logger,display_object,scene,breadcrumbs,project_name,breadcrumbs_container,
            frp_inputs,current_index,frp_debug,camera,background}.init()
    }

    fn init(self) -> Self {
        self.add_child(&self.project_name);
        self.add_child(&self.breadcrumbs_container);
        self.project_name.set_position_x(HORIZONTAL_MARGIN);
        self.relayout_for_project_name_width(self.project_name.width());
        self.project_name.frp.select.emit(());
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
                let breadcrumb       = Breadcrumb::new(&self.scene, method_pointer, expression_id);
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
                let frp_debug = &self.frp_debug;
                frp::extend! { network
                    eval_ breadcrumb.frp.outputs.clicked(
                        frp_debug.select_breadcrumb.emit(new_index);
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
#[derive(Debug,Clone,CloneRef,Shrinkwrap)]
#[allow(missing_docs)]
pub struct Breadcrumbs {
    #[shrinkwrap(main_field)]
    model   : Rc<BreadcrumbsModel>,
    pub frp : Frp
}

impl Breadcrumbs {
    /// Constructor.
    pub fn new<'t,S:Into<&'t Scene>>(scene:S, focus_manager:&FocusManager) -> Self {
        let scene   = scene.into();
        let frp     = Frp::new();
        let model   = Rc::new(BreadcrumbsModel::new(scene,&frp,focus_manager));
        let network = &frp.network;

        // === Breadcrumb selection ===

        frp::extend! { network

            // === Selecting ===

            _breadcrumb_selection <- frp.select_breadcrumb.map(f!([model,frp](index)
                frp.outputs.breadcrumb_select.emit(model.select_breadcrumb(*index));
            ));


            // === Stack Operations ===

            push_indices <= frp.push_breadcrumb.map(f!((local_call)
                model.push_breadcrumb(local_call))
            );
            pop_indices <= frp.pop_breadcrumb.map(f_!(model.pop_breadcrumb()));
            debug_push_indices <= frp.debug.push_breadcrumb.map(f!((local_call)
                model.debug_push_breadcrumb(local_call)
            ));
            debug_pop_indices <= frp.debug.pop_breadcrumb.map(f_!(model.debug_pop_breadcrumb()));

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

            eval frp.project_name((name) model.project_name.frp.name.emit(name));
            frp.outputs.project_name <+ model.project_name.frp.outputs.name;


            // === GUI Update ===

            eval model.project_name.frp.outputs.width((width) {
                model.relayout_for_project_name_width(*width)
            });


            // === User Interaction ===

            eval_ model.project_name.frp.outputs.mouse_down(frp.select_breadcrumb.emit(0));
            eval_ frp.cancel_project_name_editing(model.project_name.frp.cancel_editing.emit(()));
            eval_ frp.outside_press(model.project_name.frp.outside_press.emit(()));
            
            popped_count <= frp.outputs.breadcrumb_select.map(|selected| (0..selected.0).collect_vec());
            local_calls  <= frp.outputs.breadcrumb_select.map(|selected| selected.1.clone());
            eval_ popped_count(frp.outputs.breadcrumb_pop.emit(()));
            eval local_calls((local_call) frp.outputs.breadcrumb_push.emit(local_call));


            // === Select ===

            selected_project_name <- model.project_name.frp.outputs.mouse_down.map(f_!([model]
                model.debug_select_breadcrumb(0))
            );
            selected_breadcrumb   <- frp.debug.select_breadcrumb.map(f!((index)
                model.debug_select_breadcrumb(*index))
            );
            selected <- any(&selected_project_name,&selected_breadcrumb);

            popped_count <= selected.map(|selected| (0..selected.0).collect_vec());
            local_calls  <= selected.map(|selected| selected.1.clone());
            eval_ popped_count(frp.debug.pop_breadcrumb.emit(()));
            eval local_calls((local_call) frp.debug.push_breadcrumb.emit(local_call));


            // === Relayout ===

            eval_ scene.frp.camera_changed(model.camera_changed());
        }

        Self{frp,model}
    }
}

impl display::Object for Breadcrumbs {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}
