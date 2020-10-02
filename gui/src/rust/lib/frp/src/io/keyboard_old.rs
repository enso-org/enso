//! FIXME DEPRECATED DEPRECATED DEPRECATED DEPRECATED DEPRECATED DEPRECATED DEPRECATED DEPRECATED
//! The following implementation uses `key.legacy_keycode` which reports key codes for a very
//! small amount of keys. Please convert your code to the new implementation.

//! FRP keyboard bindings.

use crate::prelude::*;

use crate as frp;
use crate::data::bitfield::BitField256;
use crate::data::bitfield::BitField;
use enso_callback as callback;



// ===========
// === Key ===
// ===========

/// A key representation.
pub use keyboard_types::Key;



// ===============
// === KeyMask ===
// ===============

/// The key bitmask (each bit represents one key). Used for matching key combinations.
#[derive(Clone,Debug,Default,Eq,Hash,PartialEq)]
#[allow(missing_docs)]
pub struct KeyMask {
    pub bits : BitField256
}

impl KeyMask {
    /// Creates Key::Meta + Key::Character.
    pub fn meta_plus(character:char) -> Self {
        Self::from_vec(vec![Key::Meta, Key::Character(character.to_string())])
    }

    /// Creates Key::Control + Key::Character.
    pub fn control_plus(character:char) -> Self {
        Self::from_vec(vec![Key::Control, Key::Character(character.to_string())])
    }

    /// Creates Key::Alt + Key::Character.
    pub fn alt_plus(character:char) -> Self {
        Self::from_vec(vec![Key::Alt, Key::Character(character.to_string())])
    }

    /// Creates KeyMask from Vec<Key>.
    pub fn from_vec(keys:Vec<Key>) -> Self {
        keys.iter().collect()
    }

    /// Check if key bit is on.
    pub fn contains(&self, key:&Key) -> bool {
        self.bits.get_bit(key.legacy_keycode() as usize)
    }

    /// Set the `key` bit with the new state.
    pub fn set(&mut self, key:&Key, state:bool) {
        self.bits.set_bit(key.legacy_keycode() as usize,state);
    }
    // FIXME FIXME  FIXME  FIXME  FIXME  FIXME  FIXME  FIXME  FIXME  FIXME  FIXME  FIXME  FIXME
    // The above code is very bad. It uses `legacy_keycode` which works for a small amount of
    // buttons only. For example, for `meta` key, it returns just `0`. Its accidental that it works
    // now.

    /// Clone the mask and set the `key` bit with the new state.
    pub fn with_set(&self, key:&Key, state:bool) -> Self {
        let mut mask = self.clone();
        mask.set(key,state);
        mask
    }

    /// Handles new key press event and updates the key mask accordingly.
    ///
    /// **WARNING**
    ///
    /// Please note that this function is deeply magical. It checks whether the newly pressed key
    /// was registered as already pressed. This should, of course, never happen, but actually it
    /// happens on MacOS. Currently, no browser emits `keyup` event when a letter key is released
    /// WHILE the `meta` key is being pressed. Thus, if the user actions were
    /// `press meta -> press z -> release z -> press z -> release meta`, JavaScript will emit the
    /// following events: `press meta -> press z -> press z -> release meta`.
    ///
    /// Thus, when we the newly pressed key was registered as already pressed AND the `meta` key was
    /// also registered as pressed, two masks are emitted, one with the `meta` key only, and one
    /// with the `meta` key AND the newly pressed key. In case the `mmeta` key was not registered as
    /// pressed, a warning is emitted because it should never happen. Please note that this does NOT
    /// solve the issue, it only makes common use cases work properly. There is no general solution
    /// to this problem available. For example, user actions
    /// `press meta -> press z -> press x -> release z -> release x -> press x` will be interpreted
    /// as `press meta -> press z -> press x -> release x -> press x`, which clearly is not valid.
    /// Fortunately, this is not needed in most cases. To learn more about this behavior, please
    /// follow the links:
    /// - https://stackoverflow.com/questions/11818637/why-does-javascript-drop-keyup-events-when-the-metakey-is-pressed-on-mac-browser
    /// - https://w3c.github.io/uievents/tools/key-event-viewer.html
    pub fn press(&self, key:&Key) -> Vec<Self> {
        if self.contains(&Key::Meta) {
            let meta_mask = Self::default().with_set(&Key::Meta,true);
            let new_mask  = meta_mask.with_set(key,true);
            vec![meta_mask,new_mask]
        } else {
            let new_mask = self.with_set(key,true);
            vec![new_mask]
        }
    }

    /// Handles new key release event and updates the key mask accordingly.
    ///
    /// **WARNING**
    /// Please note that this function is magical. In case the released key was `meta`, the
    /// resulting key mask will be empty. In order to know why it is designed this way, please refer
    /// to the documentation of the `press` function.
    pub fn release(&self, key:&Key) -> Self {
        if key != &Key::Meta { self.with_set(key,false) } else {
            default()
        }
    }
}

impl<'a> FromIterator<&'a Key> for KeyMask {
    fn from_iter<T: IntoIterator<Item=&'a Key>>(iter:T) -> Self {
        let mut key_mask = KeyMask::default();
        for key in iter { key_mask.set(key,true) }
        key_mask
    }
}

impl From<&[Key]>   for KeyMask { fn from(keys:&[Key])   -> Self {KeyMask::from_iter(keys)} }
impl From<&[Key;0]> for KeyMask { fn from(keys:&[Key;0]) -> Self {KeyMask::from_iter(keys)} }
impl From<&[Key;1]> for KeyMask { fn from(keys:&[Key;1]) -> Self {KeyMask::from_iter(keys)} }
impl From<&[Key;2]> for KeyMask { fn from(keys:&[Key;2]) -> Self {KeyMask::from_iter(keys)} }
impl From<&[Key;3]> for KeyMask { fn from(keys:&[Key;3]) -> Self {KeyMask::from_iter(keys)} }
impl From<&[Key;4]> for KeyMask { fn from(keys:&[Key;4]) -> Self {KeyMask::from_iter(keys)} }
impl From<&[Key;5]> for KeyMask { fn from(keys:&[Key;5]) -> Self {KeyMask::from_iter(keys)} }
impl From<&KeyMask> for KeyMask { fn from(t:&KeyMask)    -> Self {t.clone()} }



// ================
// === Keyboard ===
// ================

/// Keyboard FRP bindings.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Keyboard {
    pub network       : frp::Network,
    pub on_pressed    : frp::Source<Key>,
    pub on_released   : frp::Source<Key>,
    pub on_defocus    : frp::Source,
    pub key_mask      : frp::Stream<KeyMask>,
    pub prev_key_mask : frp::Stream<KeyMask>,
}

impl Default for Keyboard {
    fn default() -> Self {
        frp::new_network! { keyboard
            on_pressed    <- source();
            on_released   <- source();
            on_defocus    <- source();
            key_mask      <- any_mut::<KeyMask>();
            new_mask      <= on_pressed  . map2(&key_mask,|key,mask| mask.press(key));
            key_mask      <+ new_mask;
            key_mask      <+ on_released . map2(&key_mask,|key,mask| mask.release(key));
            key_mask      <+ on_defocus  . map2(&key_mask,|_,_| default());
            prev_key_mask <- key_mask.previous();
        }
        let network  = keyboard;
        let key_mask = key_mask.into();
        Keyboard {network,on_pressed,on_released,on_defocus,key_mask,prev_key_mask}
    }
}



// ===============
// === Actions ===
// ===============

// TODO: Remove Actions and all of its usages. Use the new `app::shortcut` tools.

/// An action defined for specific key combinations. For convenience, the key mask is passed as
/// argument.
pub trait Action = FnMut() + 'static;

/// A mapping between key combinations and actions.
pub type ActionMap = HashMap<KeyMask,callback::SharedRegistryMut>;

/// A structure bound to Keyboard FRP graph, which allows to define actions for specific keystrokes.
#[derive(Clone,CloneRef)]
pub struct Actions {
    action_map : Rc<RefCell<ActionMap>>,
    network    : frp::Network
}

impl Actions {
    /// Create structure without any actions defined yet. It will be listening for events from
    /// passed `Keyboard` structure.
    pub fn new(keyboard:&Keyboard) -> Self {
        let action_map = Rc::new(RefCell::new(HashMap::new()));
        frp::new_network! { keyboard_actions
            def _action = keyboard.key_mask.map(Self::perform_action_fn(action_map.clone_ref()));
        }
        let network = keyboard_actions;
        Actions{action_map,network}
    }

    fn perform_action_fn(action_map:Rc<RefCell<ActionMap>>) -> impl Fn(&KeyMask) {
        move |key_mask| {
            // The action map ref is cloned in order to execute callbacks when not being borrowed.
            let opt_callbacks = action_map.borrow().get(key_mask).map(|t| t.clone_ref());
            if let Some(callbacks) = opt_callbacks {
                callbacks.run_all();
                if callbacks.is_empty() {
                    action_map.borrow_mut().remove(key_mask);
                }
            }
        }
    }

    /// Set action binding for given key mask.
    pub fn add_action_for_key_mask<F:Action>(&self, key_mask:KeyMask, action:F) -> callback::Handle {
        self.action_map.borrow_mut().entry(key_mask).or_insert_with(default).add(action)
    }

    /// Set action binding for given set of keys.
    pub fn add_action<F:Action>(&self, keys:&[Key], action:F) -> callback::Handle {
        self.add_action_for_key_mask(keys.into(),action)
    }
}

impl Debug for Actions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<CallbackRegistry>")
    }
}



// =============
// === Tests ===
// =============

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn key_mask() {
        let keyboard                  = Keyboard::default();
        let expected_key_mask:KeyMask = default();
        frp::new_network! { sampler_network
            def sampler = keyboard.key_mask.sampler();
        }
        assert_eq!(expected_key_mask, sampler.value());
        let key1 = Key::Character("x".to_string());
        let key2 = Key::Control;

        keyboard.on_pressed.emit(key1.clone());
        let expected_key_mask:KeyMask = std::iter::once(&key1).collect();
        assert_eq!(expected_key_mask, sampler.value());

        keyboard.on_pressed.emit(key2.clone());
        let expected_key_mask:KeyMask = [&key1,&key2].iter().cloned().collect();
        assert_eq!(expected_key_mask, sampler.value());

        keyboard.on_released.emit(key1);
        let expected_key_mask:KeyMask = std::iter::once(&key2).collect();
        assert_eq!(expected_key_mask, sampler.value());
    }
}