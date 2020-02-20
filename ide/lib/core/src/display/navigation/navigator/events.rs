use crate::prelude::*;

use crate::control::io::mouse::MouseManager;
use crate::control::io::mouse::event::*;
use crate::control::io::mouse::button;
use crate::control::callback::CallbackHandle;
use crate::system::web::Result;
use crate::system::web::dom::DomContainer;
use crate::system::web::IgnoreContextMenuHandle;
use crate::system::web::ignore_context_menu;
use crate::system::web::dyn_into;

use nalgebra::Vector2;
use nalgebra::zero;
use std::rc::Rc;
use std::cell::RefCell;
use web_sys::EventTarget;



// =================
// === ZoomEvent ===
// =================

pub trait FnZoomEvent = FnMut(ZoomEvent) + 'static;

/// A struct holding zoom event information, such as the focus point and the amount of zoom.
pub struct ZoomEvent {
    pub focus  : Vector2<f32>,
    pub amount : f32
}

impl ZoomEvent {
    fn new(focus:Vector2<f32>, amount:f32, zoom_speed:f32) -> Self {
        let amount = amount * zoom_speed;
        Self { focus, amount }
    }
}



// ================
// === PanEvent ===
// ================

pub trait FnPanEvent = FnMut(PanEvent) + 'static;

/// A struct holding pan event information.
pub struct PanEvent {
    pub movement : Vector2<f32>
}

impl PanEvent {
    fn new(movement:Vector2<f32>) -> Self {
        Self { movement }
    }
}



// ====================
// === MovementType ===
// ====================

#[derive(PartialEq,Clone,Copy,Debug)]
enum MovementType {
    Pan,
    Zoom { focus : Vector2<f32> }
}



// =================================
// === NavigatorEventsProperties ===
// =================================

#[derive(Derivative)]
#[derivative(Debug)]
struct NavigatorEventsProperties {
    zoom_speed          : f32,
    movement_type       : Option<MovementType>,
    last_mouse_position : Vector2<f32>,
    mouse_position      : Vector2<f32>,
    #[derivative(Debug="ignore")]
    pan_callback : Box<dyn FnPanEvent>,
    #[derivative(Debug="ignore")]
    zoom_callback : Box<dyn FnZoomEvent>,
}



// ===========================
// === NavigatorEventsData ===
// ===========================

#[derive(Debug)]
struct NavigatorEventsData {
    properties : RefCell<NavigatorEventsProperties>
}

impl NavigatorEventsData {
    fn new
    ( pan_callback:Box<dyn FnPanEvent>
    , zoom_callback:Box<dyn FnZoomEvent>
    , zoom_speed:f32) -> Rc<Self> {
        let mouse_position      = zero();
        let last_mouse_position = zero();
        let movement_type       = None;
        let properties          = RefCell::new(NavigatorEventsProperties {
            last_mouse_position,
            mouse_position,
            movement_type,
            pan_callback,
            zoom_callback,
            zoom_speed

        });
        Rc::new(Self {properties})
    }

    fn on_zoom(&self, event:ZoomEvent) {
        (&mut self.properties.borrow_mut().zoom_callback)(event);
    }

    fn on_pan(&self, event: PanEvent) {
        (&mut self.properties.borrow_mut().pan_callback)(event);
    }
}


// === Getters ===

impl NavigatorEventsData {
    fn mouse_position(&self) -> Vector2<f32> {
        self.properties.borrow().mouse_position
    }

    fn last_mouse_position(&self) -> Vector2<f32> {
        self.properties.borrow().last_mouse_position
    }

    fn zoom_speed(&self) -> f32 {
        self.properties.borrow().zoom_speed
    }

    fn movement_type(&self) -> Option<MovementType> {
        self.properties.borrow().movement_type
    }
}


// === Setters ===

impl NavigatorEventsData {
    fn set_movement_type(&self, movement_type:Option<MovementType>) {
        self.properties.borrow_mut().movement_type = movement_type;
    }

    fn set_mouse_position(&self, mouse_position:Vector2<f32>) {
        let mut properties             = self.properties.borrow_mut();
        properties.last_mouse_position = properties.mouse_position;
        properties.mouse_position      = mouse_position;
    }
}


// =======================
// === NavigatorEvents ===
// =======================

/// Struct used to handle pan and zoom events from mouse interactions.
#[derive(Derivative)]
#[derivative(Debug)]
pub struct NavigatorEvents {
    data                 : Rc<NavigatorEventsData>,
    mouse_manager        : MouseManager,
    #[derivative(Debug="ignore")]
    mouse_down           : Option<CallbackHandle>,
    #[derivative(Debug="ignore")]
    mouse_up             : Option<CallbackHandle>,
    #[derivative(Debug="ignore")]
    mouse_move           : Option<CallbackHandle>,
    #[derivative(Debug="ignore")]
    mouse_leave          : Option<CallbackHandle>,
    #[derivative(Debug="ignore")]
    disable_context_menu : Option<IgnoreContextMenuHandle>,
    #[derivative(Debug="ignore")]
    wheel_zoom           : Option<CallbackHandle>
}

impl NavigatorEvents {
    pub fn new
    <P,Z>(dom:&DomContainer, pan_callback:P, zoom_callback:Z, zoom_speed:f32) -> Result<Self>
    where P : FnPanEvent, Z : FnZoomEvent {
        let event_target         = dyn_into(dom.dom.clone())?;
        let mouse_manager        = MouseManager::new(&event_target);
        let pan_callback         = Box::new(pan_callback);
        let zoom_callback        = Box::new(zoom_callback);
        let mouse_move           = None;
        let mouse_up             = None;
        let mouse_down           = None;
        let wheel_zoom           = None;
        let disable_context_menu = None;
        let mouse_leave          = None;
        let data = NavigatorEventsData::new(pan_callback,zoom_callback,zoom_speed);
        let mut event_handler = Self {
            data,
            mouse_manager,
            mouse_down,
            mouse_move,
            mouse_up,
            mouse_leave,
            disable_context_menu,
            wheel_zoom
        };

        event_handler.initialize_mouse_events(&event_target)?;
        Ok(event_handler)
    }

    fn initialize_mouse_events(&mut self, target:&EventTarget) -> Result<()> {
        self.disable_context_menu(target)?;
        self.initialize_wheel_zoom()?;
        self.initialize_mouse_start_event()?;
        self.initialize_mouse_move_event()?;
        self.initialize_mouse_end_event()
    }

    fn initialize_wheel_zoom(&mut self) -> Result<()> {
        let data     = Rc::downgrade(&self.data);
        let listener = self.mouse_manager.on_wheel.add(move |event:&OnWheel| {
            event.prevent_default();
            if let Some(data) = data.upgrade() {
                if event.ctrl_key() {
                    let position   = data.mouse_position();
                    let zoom_speed = data.zoom_speed();
                    let amount     = event.delta_y() as f32;
                    let zoom_event = ZoomEvent::new(position, amount, zoom_speed);
                    data.on_zoom(zoom_event);
                } else {
                    let x         =  event.delta_x() as f32;
                    let y         = -event.delta_y() as f32;
                    let movement  = Vector2::new(x,y);
                    let pan_event = PanEvent::new(movement);
                    data.on_pan(pan_event);
                }
            }
        });
        self.wheel_zoom = Some(listener);
        Ok(())
    }

    fn initialize_mouse_start_event(&mut self) -> Result<()> {
        let data     = Rc::downgrade(&self.data);
        let listener = self.mouse_manager.on_down.add(move |event:&OnDown| {
            if let Some(data) = data.upgrade() {
                match event.button() {
                    button::MiddleButton => {
                        data.set_movement_type(Some(MovementType::Pan))
                    },
                    button::SecondaryButton => {
                        let focus = Vector2::new(event.offset_x() as f32, event.offset_y() as f32);
                        data.set_movement_type(Some(MovementType::Zoom { focus }))
                    },
                    _ => ()
                }
            }
        });
        self.mouse_down = Some(listener);
        Ok(())
    }

    fn disable_context_menu(&mut self, target:&EventTarget) -> Result<()> {
        self.disable_context_menu = Some(ignore_context_menu(target)?);
        Ok(())
    }

    fn initialize_mouse_end_event(&mut self) -> Result<()> {
        let data     = Rc::downgrade(&self.data);
        let listener = self.mouse_manager.on_up.add(move |_:&OnUp| {
            if let Some(data) = data.upgrade() {
                data.set_movement_type(None);
            }
        });
        self.mouse_up = Some(listener);

        let data     = Rc::downgrade(&self.data);
        let listener = self.mouse_manager.on_leave.add(move |_:&OnLeave| {
            if let Some(data) = data.upgrade() {
                data.set_movement_type(None);
            }
        });
        self.mouse_leave = Some(listener);
        Ok(())
    }

    fn initialize_mouse_move_event(&mut self) -> Result<()> {
        let data     = Rc::downgrade(&self.data);
        let listener = self.mouse_manager.on_move.add(move |event:&OnMove| {
            if let Some(data) = data.upgrade() {
                let position = Vector2::new(event.offset_x() as f32, event.offset_y() as f32);
                data.set_mouse_position(position);
                let mut movement = data.mouse_position() - data.last_mouse_position();
                movement.x = -movement.x;

                if let Some(movement_type) = data.movement_type() {
                    match movement_type {
                        MovementType::Zoom { focus } => {
                            let zoom_speed = data.zoom_speed();
                            let zoom_event = ZoomEvent::new(focus,movement.y,zoom_speed);
                            data.on_zoom(zoom_event);
                        },
                        MovementType::Pan => {
                            let pan_event = PanEvent::new(movement);
                            data.on_pan(pan_event);
                        }
                    }
                }
            }
        });
        self.mouse_move = Some(listener);
        Ok(())
    }
}
