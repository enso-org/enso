//! This module defines the `Container` struct and related functionality.

use crate::prelude::*;

use crate::frp;
use crate::visualization::*;

use ensogl::display::Scene;
use ensogl::display::traits::*;
use ensogl::display;


// ===========
// === FRP ===
// ===========

/// Event system of the `Container`.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct ContainerFrp {
    pub network           : frp::Network,
    pub set_visibility    : frp::Source<bool>,
    pub toggle_visibility : frp::Source,
    pub set_visualization : frp::Source<Option<Visualization>>,
    pub set_data          : frp::Source<Option<Data>>,
}

impl Default for ContainerFrp {
    fn default() -> Self {
        frp::new_network! { visualization_events
            def set_visibility    = source();
            def toggle_visibility = source();
            def set_visualization = source();
            def set_data          = source();
        };
        let network = visualization_events;
        Self {network,set_visibility,set_visualization,toggle_visibility,set_data }
    }
}



// ================================
// === Visualizations Container ===
// ================================

/// Container that wraps a `Visualization` for rendering and interaction in the GUI.
///
/// The API to interact with the visualization is exposed through the `ContainerFrp`.
#[derive(Clone,CloneRef,Debug,Shrinkwrap)]
#[allow(missing_docs)]
pub struct Container {
    // The internals are split into two structs: `ContainerData` and `ContainerFrp`. The
    // `ContainerData` contains the actual data and logic for the `Container`. The `ContainerFrp`
    // contains the FRP api and network. This split is required to avoid creating cycles in the FRP
    // network: the FRP network holds `Rc`s to the `ContainerData` and thus must not live in the
    // same struct.

    #[shrinkwrap(main_field)]
        data : Rc<ContainerData>,
    pub frp  : ContainerFrp,
}

/// Internal data of a `Container`.
#[derive(Debug,Clone)]
#[allow(missing_docs)]
pub struct ContainerData {
    logger         : Logger,
    display_object : display::object::Instance,
    size           : Cell<Vector2<f32>>,
    visualization  : RefCell<Option<Visualization>>,
}

impl ContainerData {
    /// Set whether the visualization should be visible or not.
    pub fn set_visibility(&self, is_visible:bool) {
        if let Some(vis) = self.visualization.borrow().as_ref() {
            if is_visible {
                self.add_child(vis);
            } else {
                vis.unset_parent();
            }
        }
    }

    /// Indicates whether the visualization is visible.
    pub fn is_visible(&self) -> bool {
        if let Some(vis) = self.visualization.borrow().as_ref() {
            vis.has_parent()
        } else {
            false
        }
    }

    /// Toggle visibility.
    fn toggle_visibility(&self) {
        self.set_visibility(!self.is_visible())
    }

    /// Update the content properties with the values from the `ContainerData`.
    ///
    /// Needs to called when a visualization has been set.
    fn init_visualization_properties(&self) {
        let size         = self.size.get();
        if let Some(vis) = self.visualization.borrow().as_ref() {
            vis.set_size(size);
        };
        self.set_visibility(true);
    }

    /// Set the visualization shown in this container..
    fn set_visualization(&self, visualization:Visualization) {
        self.add_child(&visualization);
        self.visualization.replace(Some(visualization));
        self.init_visualization_properties();
    }
}

impl display::Object for ContainerData {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}


impl Container {
    /// Constructor.
    pub fn new(scene:&Scene) -> Self {
        let logger         = Logger::new("visualization");
        let visualization  = default();
        let size           = Cell::new(Vector2::new(200.0, 200.0));
        let display_object = display::object::Instance::new(&logger);
        let data           = ContainerData {logger,visualization,size,display_object};
        let data           = Rc::new(data);
        data.set_visualization(Registry::default_visualisation(&scene));
        data.set_visibility(false);
        let frp            = default();
        Self {data,frp} . init_frp()
    }

    fn init_frp(self) -> Self {
        let frp     = &self.frp;
        let network = &self.frp.network;

        frp::extend! { network

            let container_data = &self.data;

            def _f_hide = frp.set_visibility.map(f!([container_data](is_visible) {
                container_data.set_visibility(*is_visible);
            }));

            def _f_toggle = frp.toggle_visibility.map(f!([container_data](_) {
                container_data.toggle_visibility()
            }));

            def _f_set_vis = frp.set_visualization.map(f!([container_data](visualization) {
                if let Some(visualization) = visualization.as_ref() {
                    container_data.set_visualization(visualization.clone());
                }
            }));

            def _f_set_data = frp.set_data.map(f!([container_data](data) {
                 container_data.visualization.borrow()
                    .for_each_ref(|vis| vis.frp.set_data.emit(data));
            }));
        }
        self
    }
}

impl display::Object for Container {
    fn display_object(&self) -> &display::object::Instance {
        &self.data.display_object
    }
}
