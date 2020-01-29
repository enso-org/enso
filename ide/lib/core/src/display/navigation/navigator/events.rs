use crate::prelude::*;

use crate::control::io::mouse::MouseManager;
use crate::control::io::mouse::MouseClickEvent;
use crate::control::io::mouse::MouseWheelEvent;
use crate::control::io::mouse::MousePositionEvent;
use crate::control::io::mouse::MouseButton;
use crate::control::io::mouse::WheelEventListener;
use crate::control::io::mouse::MouseEventListener;
use crate::system::web::Result;
use crate::system::web::dom::DomContainer;

use nalgebra::Vector2;
use std::rc::Rc;
use std::cell::RefCell;
use nalgebra::zero;



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

    fn from_mouse_wheel
    (event:MouseWheelEvent, mouse_position:Vector2<f32>, zoom_speed:f32) -> Self {
        let amount = event.movement_y;
        let focus  = mouse_position;
        Self::new(focus, amount, zoom_speed)
    }

    fn from_mouse_move(event:MousePositionEvent, focus:Vector2<f32>, zoom_speed:f32) -> Self {
        let amount = event.position.y - event.previous_position.y;
        Self::new(focus, amount, zoom_speed)
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
    fn from_mouse_move(event:MousePositionEvent) -> Self {
        let mut movement = event.position - event.previous_position;
        movement.x       = -movement.x;
        Self { movement }
    }

    fn from_wheel_event(event:MouseWheelEvent) -> Self {
        let movement = Vector2::new(event.movement_x, -event.movement_y);
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
    movement_type  : Option<MovementType>,
    mouse_position : Vector2<f32>,
    #[derivative(Debug="ignore")]
    pan_callback   : Box<dyn FnPanEvent>,
    #[derivative(Debug="ignore")]
    zoom_callback  : Box<dyn FnZoomEvent>,
    zoom_speed     : f32
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
        let mouse_position = zero();
        let movement_type  = None;
        let properties     = RefCell::new(NavigatorEventsProperties {
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
        self.properties.borrow_mut().mouse_position = mouse_position;
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
    mouse_down           : Option<MouseEventListener>,
    #[derivative(Debug="ignore")]
    mouse_move           : Option<MouseEventListener>,
    #[derivative(Debug="ignore")]
    mouse_up             : Option<MouseEventListener>,
    #[derivative(Debug="ignore")]
    mouse_leave          : Option<MouseEventListener>,
    #[derivative(Debug="ignore")]
    disable_context_menu : Option<MouseEventListener>,
    #[derivative(Debug="ignore")]
    wheel_zoom           : Option<WheelEventListener>
}

impl NavigatorEvents {
    pub fn new
    <P,Z>(dom:&DomContainer, pan_callback:P, zoom_callback:Z, zoom_speed:f32) -> Result<Self>
    where P : FnPanEvent, Z : FnZoomEvent {
        let pan_callback         = Box::new(pan_callback);
        let zoom_callback        = Box::new(zoom_callback);
        let mouse_move           = None;
        let mouse_up             = None;
        let mouse_leave          = None;
        let mouse_down           = None;
        let wheel_zoom           = None;
        let disable_context_menu = None;
        let mouse_manager        = MouseManager::new(dom)?;
        let data          = NavigatorEventsData::new(pan_callback, zoom_callback, zoom_speed);
        let mut event_handler = Self {
            data,
            mouse_manager,
            mouse_down,
            mouse_move,
            mouse_up,
            mouse_leave,
            disable_context_menu,
            wheel_zoom,
        };

        event_handler.initialize_mouse_events()?;
        Ok(event_handler)
    }

    fn initialize_mouse_events(&mut self) -> Result<()> {
        self.disable_context_menu()?;
        self.initialize_wheel_zoom()?;
        self.initialize_mouse_start_event()?;
        self.initialize_mouse_move_event()?;
        self.initialize_mouse_end_event()
    }

    fn initialize_wheel_zoom(&mut self) -> Result<()> {
        let data = Rc::downgrade(&self.data);
        let listener = self.mouse_manager.add_mouse_wheel_callback(move |event:MouseWheelEvent| {
            if let Some(data) = data.upgrade() {
                if event.is_ctrl_pressed {
                    let position   = data.mouse_position();
                    let zoom_speed = data.zoom_speed();
                    let zoom_event = ZoomEvent::from_mouse_wheel(event, position, zoom_speed);
                    data.on_zoom(zoom_event);
                } else {
                    let pan_event = PanEvent::from_wheel_event(event);
                    data.on_pan(pan_event);
                }
            }
        })?;
        self.wheel_zoom = Some(listener);
        Ok(())
    }

    fn initialize_mouse_start_event(&mut self) -> Result<()> {
        let data     = Rc::downgrade(&self.data);
        let listener = self.mouse_manager.add_mouse_down_callback(move |event:MouseClickEvent| {
            if let Some(data) = data.upgrade() {
                match event.button {
                    MouseButton::MIDDLE => {
                        data.set_movement_type(Some(MovementType::Pan))
                    },
                    MouseButton::RIGHT => {
                        let focus = event.position;
                        data.set_movement_type(Some(MovementType::Zoom { focus }))
                    },
                    _ => ()
                }
            }
        })?;
        self.mouse_down = Some(listener);
        Ok(())
    }

    fn disable_context_menu(&mut self) -> Result<()> {
        self.disable_context_menu = Some(self.mouse_manager.disable_context_menu()?);
        Ok(())
    }

    fn initialize_mouse_end_event(&mut self) -> Result<()> {
        let data         = Rc::downgrade(&self.data);
        let closure      = move |_| {
            if let Some(data) = data.upgrade() {
                data.set_movement_type(None);
            }
        };
        let listener     = self.mouse_manager.add_mouse_up_callback(closure)?;
        self.mouse_up    = Some(listener);

        let data         = Rc::downgrade(&self.data);
        let closure      = move |_| {
            if let Some(data) = data.upgrade() {
                data.set_movement_type(None);
            }
        };
        let listener     = self.mouse_manager.add_mouse_leave_callback(closure)?;
        self.mouse_leave = Some(listener);
        Ok(())
    }

    fn initialize_mouse_move_event(&mut self) -> Result<()> {
        let data     = Rc::downgrade(&self.data);
        let listener = self.mouse_manager.add_mouse_move_callback(move |event:MousePositionEvent| {
            if let Some(data) = data.upgrade() {
                data.set_mouse_position(event.position);

                if let Some(movement_type) = data.movement_type() {
                    match movement_type {
                        MovementType::Zoom { focus } => {
                            let zoom_speed = data.zoom_speed();
                            let zoom_event = ZoomEvent::from_mouse_move(event, focus, zoom_speed);
                            data.on_zoom(zoom_event);
                        },
                        MovementType::Pan => {
                            let pan_event = PanEvent::from_mouse_move(event);
                            data.on_pan(pan_event);
                        }
                    }
                }
            }
        })?;
        self.mouse_move = Some(listener);
        Ok(())
    }
}
