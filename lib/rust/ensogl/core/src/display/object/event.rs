//! Events implementation. Events behave in a similar way to JavaScript Events. When an event is
//! emitted, it is propagated in two stages: capturing and bubbling. Each stage is
//! configurable and some events propagation can be cancelled. To learn more about the mechanics,
//! see: https://javascript.info/bubbling-and-capturing.

use crate::prelude::*;

use crate::display::object::instance::Instance;
use crate::display::object::instance::WeakInstance;



// =============
// === State ===
// =============

/// Event state. It can be used to determine whether the event is being propagated, its propagation
/// is cancelled, or that the propagation cannot be cancelled. See docs of this module to learn
/// more.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum State {
    // Event is being propagated, and will continue to be propagated until all registered event
    // handlers have been called. Can be cancelled with [`Event::stop_propagation`].
    Running(Phase),
    // Event is being propagated, and will continue to be propagated until all registered event
    // handlers have been called. Cannot be cancelled.
    RunningNonCancellable(Phase),
    // Event has been cancelled, but the event propagation is still running. If the event were to
    // be resumed, the propagation would continue on its own.
    RunningCancelled(Phase),
    // Event has been cancelled and [`InstanceDef::emit_event_impl`] function has returned. If the
    // event were to be resumed, the propagation will have to be restarted.
    StoppedCancelled(Phase),
    // The event propagation reached the end, all event handlers have been called. Resuming the
    // event will have no effect.
    Finished,
}

impl Default for State {
    fn default() -> Self {
        State::Running(default())
    }
}

/// Current phase of the event propagation. For cancelled events, it's the phase in which the event
/// was cancelled.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Ord, PartialOrd)]
pub enum Phase {
    #[default]
    Capturing,
    Bubbling,
}



// =================
// === SomeEvent ===
// =================

/// Similar to [`Event`] but with a hidden payload type. It is used to construct, configure, and
/// emit new events.
#[allow(missing_docs)]
#[derive(Clone, CloneRef, Debug)]
pub struct SomeEvent {
    pub data:       frp::AnyData,
    state:          Rc<Cell<State>>,
    current_target: Rc<RefCell<Option<WeakInstance>>>,
    /// Indicates whether the event participates in the capturing phase.
    pub captures:   Rc<Cell<bool>>,
    /// Indicates whether the event participates in the bubbling phase.
    pub bubbles:    Rc<Cell<bool>>,
}

impl SomeEvent {
    /// Constructor.
    pub fn new<T: 'static>(target: Option<WeakInstance>, payload: T) -> Self {
        Self::from_event(Event::new(target, payload))
    }

    fn from_event<T: 'static>(event: Event<T>) -> Self {
        let state = event.state.clone_ref();
        let current_target = event.current_target.clone_ref();
        let captures = event.captures.clone_ref();
        let bubbles = event.bubbles.clone_ref();
        Self { data: frp::AnyData::new(event), state, current_target, captures, bubbles }
    }

    /// The [`State]` of the event.
    pub fn state(&self) -> State {
        self.state.get()
    }

    /// Check whether the event was cancelled.
    pub fn is_cancelled(&self) -> bool {
        matches!(self.state(), State::RunningCancelled(_) | State::StoppedCancelled(_))
    }

    /// Enables or disables bubbling for this event.
    pub fn set_bubbling(&self, value: bool) {
        self.bubbles.set(value);
    }

    /// Determine the phase at which the event propagation should continue. This is internal
    /// function and should not be used directly.
    pub(crate) fn begin_propagation(&self) -> Option<(Phase, Option<Instance>)> {
        match self.state.get() {
            State::StoppedCancelled(phase) => {
                let target = self.current_target.borrow().as_ref()?.upgrade()?;
                self.state.set(State::Running(phase));
                Some((phase, Some(target)))
            }
            _ => Some((Phase::Capturing, None)),
        }
    }

    pub(crate) fn enter_phase(&self, phase: Phase) {
        self.state.set(match self.state.get() {
            State::Running(_) => State::Running(phase),
            State::RunningNonCancellable(_) => State::RunningNonCancellable(phase),
            State::RunningCancelled(_) => State::RunningCancelled(phase),
            State::StoppedCancelled(_) => State::StoppedCancelled(phase),
            State::Finished => State::Finished,
        });
    }

    /// Mark the end of the event propagation. This is internal function and should not be used
    /// directly.
    pub(crate) fn finish_propagation(&self) {
        self.state.set(match self.state.get() {
            State::RunningCancelled(phase) => State::StoppedCancelled(phase),
            _ => {
                self.set_current_target(None);
                State::Finished
            }
        });
    }

    /// Set the current target of the event. This is internal function and should not be used
    /// directly.
    pub(crate) fn set_current_target(&self, target: Option<&Instance>) {
        self.current_target.replace(target.map(|t| t.downgrade()));
    }
}

impl Default for SomeEvent {
    fn default() -> Self {
        Self::new::<()>(None, ())
    }
}



// =============
// === Event ===
// =============

/// The [`Event`] interface represents an event which takes place in the EnsoGL display object
/// hierarchy.
///
/// An event can be triggered by the user action e.g. clicking the mouse button or tapping keyboard,
/// or generated by APIs to represent the progress of an asynchronous task. It can also be triggered
/// programmatically, such as by calling the [`display::object::Instance::focus()`] method of an
/// element, or by defining the event, then sending it to a specified target using
/// [`display::object::Instance::event_source::emit(...)`].
///
/// See the JavaScript counterpart of this struct:
/// https://developer.mozilla.org/en-US/docs/Web/API/Event.
#[derive(Derivative, Deref)]
#[derivative(Clone(bound = ""))]
#[derivative(Default(bound = "T: Default"))]
pub struct Event<T> {
    data: Rc<EventData<T>>,
}

impl<T: Debug> Debug for Event<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.data, f)
    }
}

/// Internal representation of [`Event`].
#[allow(missing_docs)]
#[derive(Deref, Derivative)]
#[derivative(Default(bound = "T: Default"))]
pub struct EventData<T> {
    #[deref]
    pub payload:    T,
    target:         Option<WeakInstance>,
    current_target: Rc<RefCell<Option<WeakInstance>>>,
    state:          Rc<Cell<State>>,
    captures:       Rc<Cell<bool>>,
    bubbles:        Rc<Cell<bool>>,
}

impl<T: Debug> Debug for EventData<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Event")
            .field("payload", &self.payload)
            .field("state", &self.state.get())
            .finish()
    }
}

impl<T: 'static> Event<T> {
    fn new(target: Option<WeakInstance>, payload: T) -> Self {
        let state = default();
        let current_target = Rc::new(RefCell::new(target.clone()));
        let captures = Rc::new(Cell::new(true));
        let bubbles = Rc::new(Cell::new(true));
        let data = Rc::new(EventData { payload, target, current_target, state, captures, bubbles });
        Self { data }
    }

    /// Prevents further propagation of the current event in the capturing and bubbling phases. It
    /// also does NOT prevent immediate propagation to other event-handlers.
    ///
    /// See: https://developer.mozilla.org/en-US/docs/Web/API/Event/stopPropagation.
    pub fn stop_propagation(&self) {
        match self.state.get() {
            State::Running(phase) => self.state.set(State::RunningCancelled(phase)),
            State::RunningNonCancellable(_) => warn!("Trying to cancel a non-cancellable event."),
            _ => {}
        }
    }


    /// Emit event again with the same payload after it was cancelled. The event will start its
    /// capturing phase handling again, starting at but not including this instance. If the passed
    /// instance is no longer a part of the parent chain of the event's original target, the event
    /// will start its capturing phase from scratch. If the original target no longer exists, the
    /// event will be discarded.
    pub fn resume_propagation(&self) {
        match self.state.get() {
            State::RunningCancelled(phase) => {
                // When cancelled but not stopped yet, the propagation is still ongoing. We can
                // reset the state back to running and let it continue.
                self.state.set(State::Running(phase));
            }
            State::StoppedCancelled(_) =>
                if let Some(target) = self.target() {
                    target.resume_event(SomeEvent::from_event(self.clone()));
                },
            _ => warn!("Trying to resume propagation of a non-cancelled event."),
        }
    }


    /// A reference to the object onto which the event was dispatched.
    ///
    /// See: https://developer.mozilla.org/en-US/docs/Web/API/Event/target.
    pub fn target(&self) -> Option<Instance> {
        self.data.target.as_ref().and_then(|t| t.upgrade())
    }

    /// The current target for the event, as the event traverses the display object hierarchy. It
    /// always refers to the element to which the event handler has been attached, as opposed to
    /// [`Self::target`], which identifies the element on which the event occurred and which may be
    /// its descendant.
    ///
    /// # Important Note
    /// The value of [`Self::current_target`] is only available while the event is being handled. If
    /// store the event in a variable and read this property later, the value will be [`None`].
    pub fn current_target(&self) -> Option<Instance> {
        self.data.current_target.borrow().as_ref().and_then(|t| t.upgrade())
    }
}



// ====================
// === Basic Events ===
// ====================

/// The [`Focus`] event fires when an element has received focus. The event does not bubble, but the
/// related [`FocusIn`] event that follows does bubble.
///
/// The opposite of [`Focus`] is the [`Blur`] event, which fires when the element has lost focus.
///
/// The [`Focus`] event is not cancelable.
///
/// See: https://developer.mozilla.org/en-US/docs/Web/API/Element/focus_event.
#[derive(Clone, Copy, Debug, Default)]
pub struct Focus;

/// The [`Blur`] event fires when an element has lost focus. The event does not bubble, but the
/// related [`FocusOut`] event that follows does bubble.
///
/// The opposite of [`Blur`] is the [Focus] event, which fires when the element has received focus.
/// The [`Blur`] event is not cancelable.
///
/// See: https://developer.mozilla.org/en-US/docs/Web/API/Element/blur_event.
#[derive(Clone, Copy, Debug, Default)]
pub struct Blur;

/// The [`FocusIn`] event fires when an element has received focus, after the [`Focus`] event. The
/// two events differ in that [`FocusIn`] bubbles, while [`Focus`] does not.
///
/// The opposite of [`FocusIn`] is the [`FocusOut`] event, which fires when the element has lost
/// focus. The [`FocusIn`] event is not cancelable.
///
/// See: https://developer.mozilla.org/en-US/docs/Web/API/Element/focusin_event.
#[derive(Clone, Copy, Debug, Default)]
pub struct FocusIn;

/// The [`FocusOut`] event fires when an element has lost focus, after the [`Blur`] event. The two
/// events differ in that [`FocusOut`] bubbles, while [`Blur`] does not.
///
/// The opposite of [`FocusOut`] is the [`FocusIn`] event, which fires when the element has received
/// focus. The [`FocusOut`] event is not cancelable.
///
/// See: https://developer.mozilla.org/en-US/docs/Web/API/Element/focusout_event.
#[derive(Clone, Copy, Debug, Default)]
pub struct FocusOut;
