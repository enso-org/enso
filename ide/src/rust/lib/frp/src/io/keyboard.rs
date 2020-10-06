//! Keyboard implementation and FRP bindings.

use crate::prelude::*;

use crate as frp;
use ensogl_system_web as web;
use web_sys::KeyboardEvent;
use wasm_bindgen::prelude::*;
use web_sys::HtmlElement;
use wasm_bindgen::JsCast;
use unicode_segmentation::UnicodeSegmentation;
use inflector::Inflector;



// ============
// === Side ===
// ============

/// The key placement enum.
#[derive(Clone,Copy,Debug,Eq,Hash,PartialEq)]
#[allow(missing_docs)]
pub enum Side { Left, Right }

impl Side {
    /// Convert the side to a lowercase string representation.
    pub fn simple_name(self) -> &'static str {
        match self {
            Self::Left  => "left",
            Self::Right => "right"
        }
    }
}



// ===========
// === Key ===
// ===========

macro_rules! define_keys {
    (Side { $($side:ident),* $(,)? } Regular { $($regular:ident),* $(,)? }) => {
        /// A key representation.
        ///
        /// For reference, see the following links:
        /// https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key/Key_Values
        #[derive(Clone,Debug,Eq,Hash,PartialEq)]
        #[allow(missing_docs)]
        pub enum Key {
            $($side(Side),)*
            $($regular,)*
            Character (String),
            Other     (String),
        }


        // === KEY_NAME_MAP ===

        lazy_static! {
            /// A mapping from a name to key instance. Please note that all side-aware keys are
            /// instantiated to the left binding. The correct assignment (left/right) is done in a
            /// separate step.
            static ref KEY_NAME_MAP: HashMap<&'static str,Key> = {
                use Key::*;
                use Side::*;
                let mut m = HashMap::new();
                $(m.insert(stringify!($side), $side(Left));)*
                $(m.insert(stringify!($regular), $regular);)*
                m
            };
        }
    };
}

define_keys! {
    Side    {Alt,AltGr,AltGraph,Control,Meta,Shift}
    Regular {
        ArrowDown,
        ArrowLeft,
        ArrowRight,
        ArrowUp,
        Backspace,
        Delete,
        End,
        Enter,
        Home,
        Insert,
        PageDown,
        PageUp,
        Space,
    }
}

impl Key {
    /// Constructor. The `key` is used to distinguish between keys, while the `code` is used to
    /// check whether it was left or right key in case of side-aware keys. It's important to use the
    /// `key` to distinguish between keys, as it it hardware independent. For example, `alt a` could
    /// result in key `ą` in some keyboard layouts and the code `KeyA`. When layout changes, the
    /// symbol `ą` could be mapped to a different hardware key. Check the following site for more
    /// info: https://keycode.info.
    pub fn new(key:String, code:String) -> Self {
        let label_ref : &str = &key;
        let code_ref  : &str = &code;
        // Space is very special case. It has key value being a character, but we don't want to
        // interpret is as a Key::Character.
        if      key == " "                       { Self::Space          }
        else if key.graphemes(true).count() == 1 { Self::Character(key) }
        else {
            let key = KEY_NAME_MAP.get(label_ref).cloned().unwrap_or(Self::Other(key));
            match (key,code_ref) {
                (Self::Alt      (_), "AltRight")     => Self::Alt      (Side::Right),
                (Self::AltGr    (_), "AltRight")     => Self::AltGr    (Side::Right),
                (Self::AltGraph (_), "AltRight")     => Self::AltGraph (Side::Right),
                (Self::Control  (_), "ControlRight") => Self::Control  (Side::Right),
                (Self::Meta     (_), "MetaRight")    => Self::Meta     (Side::Right),
                (Self::Shift    (_), "ShiftRight")   => Self::Shift    (Side::Right),
                (other,_)                            => other,
            }
        }
    }

    /// When the meta key is down on MacOS, the key up event is not fired for almost every key. This
    /// function checks whether the event will be emitted for a particular key. Please note that
    /// although this is MacOS specific issue, we are simulating this behavior on all platforms to
    /// keep it consistent.
    pub fn can_be_missing_when_meta_is_down(&self) -> bool {
        match self {
            Self::Alt      (_) => false,
            Self::AltGr    (_) => false,
            Self::AltGraph (_) => false,
            Self::Control  (_) => false,
            Self::Meta     (_) => false,
            Self::Shift    (_) => false,
            _                  => true
        }
    }

    /// Simple, kebab-case name of a key.
    pub fn simple_name(&self) -> String {
        let fmt = |side:&Side,repr| format!("{}-{}",repr,side.simple_name());
        match self {
            Self::Alt       (side) => fmt(side,"alt"),
            Self::AltGr     (side) => fmt(side,"alt-graph"),
            Self::AltGraph  (side) => fmt(side,"alt-graph"),
            Self::Control   (side) => fmt(side,"ctrl"),
            Self::Meta      (side) => fmt(side,"meta"),
            Self::Shift     (side) => fmt(side,"shift"),

            Self::ArrowDown        => "arrow-down".into(),
            Self::ArrowLeft        => "arrow-left".into(),
            Self::ArrowRight       => "arrow-right".into(),
            Self::ArrowUp          => "arrow-up".into(),
            Self::Backspace        => "backspace".into(),
            Self::Delete           => "delete".into(),
            Self::End              => "end".into(),
            Self::Enter            => "enter".into(),
            Self::Home             => "home".into(),
            Self::Insert           => "insert".into(),
            Self::PageDown         => "page-down".into(),
            Self::PageUp           => "page-up".into(),
            Self::Space            => "space".into(),

            Self::Character (repr) => repr.into(),
            Self::Other     (repr) => repr.to_kebab_case()
        }
    }
}

impl Default for Key {
    fn default() -> Self {
        Self::Other("".into())
    }
}



// =====================
// === KeyboardModel ===
// =====================

/// Model keeping track of currently pressed keys.
#[derive(Clone,CloneRef,Debug,Default)]
pub struct KeyboardModel {
    set : Rc<RefCell<HashSet<Key>>>,
}

impl KeyboardModel {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Check whether the meta key is currently pressed.
    pub fn is_meta_down(&self) -> bool {
        self.is_down(&Key::Meta(Side::Left)) || self.is_down(&Key::Meta(Side::Right))
    }

    /// Checks whether the provided key is currently pressed.
    pub fn is_down(&self, key:&Key) -> bool {
        self.set.borrow().contains(key)
    }

    /// Simulate press of the provided key.
    pub fn press(&self, key:&Key) {
        self.set.borrow_mut().insert(key.clone());
    }

    /// Simulate release of the provided key.
    pub fn release(&self, key:&Key) {
        self.set.borrow_mut().remove(key);
    }

    /// Release all keys which can become "sticky" when meta key is down. To learn more, refer to
    /// the docs of `can_be_missing_when_meta_is_down`.
    #[allow(clippy::unnecessary_filter_map)] // Allows not cloning the element.
    pub fn release_meta_dependent(&self) -> Vec<Key> {
        let mut to_release = Vec::new();
        let new_set        = self.set.borrow_mut().drain().filter_map(|key| {
            if key.can_be_missing_when_meta_is_down() {
                to_release.push(key);
                None
            } else {
                Some(key)
            }
        }).collect();
        *self.set.borrow_mut() = new_set;
        to_release
    }
}



// ======================
// === KeyboardSource ===
// ======================

/// The source of FRP keyboard inputs (press / release).
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct KeyboardSource {
    pub up   : frp::Source<Key>,
    pub down : frp::Source<Key>,
}

impl KeyboardSource {
    /// Constructor.
    pub fn new(network:&frp::Network) -> Self {
        frp::extend! { network
            down <- source();
            up   <- source();
        }
        Self {up,down}
    }
}



// ================
// === Keyboard ===
// ================

/// Keyboard FRP bindings.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Keyboard {
    model       : KeyboardModel,
    pub network : frp::Network,
    pub source  : KeyboardSource,
    pub down    : frp::Stream<Key>,
    pub up      : frp::Stream<Key>,
}

impl Keyboard {
    /// Constructor.
    pub fn new() -> Self {
        let network = frp::Network::new();
        let model   = KeyboardModel::default();
        let source  = KeyboardSource::new(&network);
        frp::extend! { network
            eval source.down ((key) model.press(key));
            eval source.up   ((key) model.release(key));
            down         <- source.down.map(|t|t.clone());
            meta_down    <- source.down.map(f_!(model.is_meta_down()));
            meta_release <= source.down.gate(&meta_down).map(f_!(model.release_meta_dependent()));
            up           <- any(&source.up,&meta_release);
        }
        Keyboard {network,model,source,down,up}
    }
}

impl Default for Keyboard {
    fn default() -> Self {
        Self::new()
    }
}



// ================
// === Listener ===
// ================

/// Callback for keyboard events.
pub trait ListenerCallback = FnMut(KeyboardEvent) + 'static;

type ListenerClosure = Closure<dyn ListenerCallback>;

/// Keyboard event listener which calls the callback function as long it lives.
#[derive(Debug)]
pub struct Listener {
    logger     : Logger,
    callback   : ListenerClosure,
    element    : HtmlElement,
    event_type : String
}

impl Listener {
    /// Constructor.
    pub fn new<F:ListenerCallback>(logger:impl AnyLogger,event_type:impl Str, f:F) -> Self {
        let closure     = Box::new(f);
        let callback    = ListenerClosure::wrap(closure);
        let element     = web::body();
        let js_function = callback.as_ref().unchecked_ref();
        let logger      = Logger::sub(logger,"Listener");
        let event_type  = event_type.as_ref();
        if element.add_event_listener_with_callback(event_type,js_function).is_err() {
            logger.warning(|| format!("Couldn't add {} event listener.",event_type));
        }
        let event_type = event_type.into();
        Self {callback,element,event_type,logger}
    }

    /// Creates a new key down event listener.
    pub fn new_key_down<F:ListenerCallback>(logger:impl AnyLogger, f:F) -> Self {
        Self::new(logger,"keydown",f)
    }

    /// Creates a new key up event listener.
    pub fn new_key_up<F:ListenerCallback>(logger:impl AnyLogger, f:F) -> Self {
        Self::new(logger,"keyup",f)
    }
}

impl Drop for Listener {
    fn drop(&mut self) {
        let callback = self.callback.as_ref().unchecked_ref();
        if self.element.remove_event_listener_with_callback(&self.event_type, callback).is_err() {
            self.logger.warning("Couldn't remove event listener.");
        }
    }
}

/// A handle of listener emitting events on bound FRP graph.
#[derive(Debug)]
pub struct DomBindings {
    key_down : Listener,
    key_up   : Listener
}

impl DomBindings {
    /// Create new Keyboard and Frp bindings.
    pub fn new(logger:impl AnyLogger, keyboard:&Keyboard) -> Self {
        let key_down = Listener::new_key_down(&logger,f!((event:KeyboardEvent)
            keyboard.source.down.emit(Key::new(event.key(),event.code()))
        ));
        let key_up = Listener::new_key_up(&logger,f!((event:KeyboardEvent)
            keyboard.source.up.emit(Key::new(event.key(),event.code()))
        ));
        Self {key_down,key_up}
    }
}
