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

// FIXME: The follwoing implementation uses `key.legacy_keycode` which reports key codes for a very
//        small amount of keys. We need a better mechanism here.

/// The key bitmask (each bit represents one key). Used for matching key combinations.
#[derive(Clone,Copy,Debug,Default,Eq,Hash,PartialEq,Shrinkwrap)]
#[shrinkwrap(mutable)]
pub struct KeyMask(pub BitField256);

impl KeyMask {
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
        let KeyMask(bit_set) = self;
        bit_set.get_bit(key.legacy_keycode() as usize)
    }

    /// Set the `key` bit for new state.
    pub fn set(&mut self, key:&Key, state:bool) {
        let KeyMask(ref mut bit_set) = self;
        bit_set.set_bit(key.legacy_keycode() as usize,state);
    }
}

impl<'a> FromIterator<&'a Key> for KeyMask {
    fn from_iter<T: IntoIterator<Item=&'a Key>>(iter:T) -> Self {
        let mut key_mask = KeyMask::default();
        for key in iter {
            let bit = key.legacy_keycode() as usize;
            key_mask.set_bit(bit,true);
        }
        key_mask
    }
}

impl From<&[Key]>   for KeyMask { fn from(keys:&[Key])   -> Self { KeyMask::from_iter(keys) } }
impl From<&[Key;1]> for KeyMask { fn from(keys:&[Key;1]) -> Self { KeyMask::from_iter(keys) } }
impl From<&[Key;2]> for KeyMask { fn from(keys:&[Key;2]) -> Self { KeyMask::from_iter(keys) } }
impl From<&[Key;3]> for KeyMask { fn from(keys:&[Key;3]) -> Self { KeyMask::from_iter(keys) } }
impl From<&[Key;4]> for KeyMask { fn from(keys:&[Key;4]) -> Self { KeyMask::from_iter(keys) } }
impl From<&[Key;5]> for KeyMask { fn from(keys:&[Key;5]) -> Self { KeyMask::from_iter(keys) } }



// =====================
// === KeyMaskChange ===
// =====================

/// A helper structure used for describing KeyMask changes.
#[derive(Clone,Debug)]
#[allow(missing_docs)]
enum KeyMaskChange { Set(Key), Unset(Key), Clear }

impl KeyMaskChange {
    fn on_pressed(key:&Key) -> Self {
        Self::Set(key.clone())
    }

    /// When we're losing focus we should clear keymask, because we are not sure what keys were
    /// released during being unfocused.
    fn on_defocus() -> Self {
        Self::Clear
    }

    fn on_released (key:&Key) -> Self {
        match key {
            // The very special case: pressing CMD on MacOS makes all the keyup events for letters
            // lost. Therefore for CMD releasing we must clear keymask.
            Key::Meta => Self::Clear,
            other     => Self::Unset(other.clone())
        }
    }

    /// Returns copy of given KeyMask with applied change
    fn updated_mask(&self, mask:&KeyMask) -> KeyMask {
        let mut mask = *mask;
        match self {
            Self::Set   (ref key) => mask.set(key,true),
            Self::Unset (ref key) => mask.set(key,false),
            Self::Clear           => mask = default()
        }
        mask
    }
}

impl Default for KeyMaskChange {
    fn default() -> Self {
        Self::Clear
    }
}



// ================
// === Keyboard ===
// ================

/// Keyboard FRP bindings.
#[derive(Clone,CloneRef,Debug)]
#[allow(missing_docs)]
pub struct Keyboard {
    pub network           : frp::Network,
    pub on_pressed        : frp::Source<Key>,
    pub on_released       : frp::Source<Key>,
    pub on_defocus        : frp::Source,
    pub key_mask          : frp::Stream<KeyMask>,
    pub previous_key_mask : frp::Stream<KeyMask>,
}

impl Default for Keyboard {
    fn default() -> Self {
        frp::new_network! { keyboard
            on_pressed        <- source();
            on_released       <- source();
            on_defocus        <- source();
            change_set        <- on_pressed  . map(KeyMaskChange::on_pressed);
            change_unset      <- on_released . map(KeyMaskChange::on_released);
            change_clear      <- on_defocus  . map(|_| KeyMaskChange::on_defocus());
            change_set_unset  <- [change_set, change_unset];
            change            <- [change_set_unset, change_clear];
            prev_key_mask     <- gather::<KeyMask>();
            key_mask          <- change.map2(&prev_key_mask,KeyMaskChange::updated_mask);
            prev_key_mask     <+ key_mask;
            previous_key_mask <- key_mask.previous();
        }
        let network = keyboard;
        Keyboard {network,on_pressed,on_released,on_defocus,key_mask,previous_key_mask}
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

        keyboard.on_released.emit(key1.clone());
        let expected_key_mask:KeyMask = std::iter::once(&key2).collect();
        assert_eq!(expected_key_mask, sampler.value());
    }
}