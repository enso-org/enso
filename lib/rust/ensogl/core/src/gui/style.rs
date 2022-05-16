//! Definition of the Style macro that creates Style, which can be updated partially.

use crate::prelude::*;



// ==================
// === StyleValue ===
// ==================

/// Defines a value of the cursor style.
#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(missing_docs)]
pub struct StyleValue<T> {
    /// Defines the value of the style. In case it is set to `None`, the default value will be
    /// used. Please note that setting it to `None` has a different effect than not providing
    /// the value in the `Style` at all. If the value is provided it can override the existing
    /// values when used in a semigroup operation.
    pub value: Option<T>,

    /// Defines if the state transition should be used. Sometimes disabling animation is required.
    /// A good example is the implementation of a selection box. When drawing selection box with
    /// the mouse, the user wants to see it in real-time, without it growing over time.
    pub animate: bool,
}

impl<T: Default> Default for StyleValue<T> {
    fn default() -> Self {
        let value = default();
        let animate = true;
        Self { value, animate }
    }
}

impl<T> StyleValue<T> {
    /// Constructor.
    pub fn new(value: T) -> Self {
        let value = Some(value);
        let animate = true;
        Self { value, animate }
    }

    /// Constructor for a default value setter. Please note that this is not made a `Default` impl
    /// on purpose. This method creates a non-empty value setter which sets the target to its
    /// default value. Read `Style` docs to learn more.
    pub fn new_default() -> Self {
        let value = None;
        let animate = true;
        Self { value, animate }
    }

    /// Constructor with disabled animation.
    pub fn new_no_animation(value: T) -> Self {
        let value = Some(value);
        let animate = false;
        Self { value, animate }
    }
}



// =============
// === Style ===
// =============

#[macro_export]
/// Create struct called `Style` that can hold updatable style information.
macro_rules! define_style {( $( $(#$meta:tt)* $field:ident : $field_type:ty),* $(,)? ) => {

    /// Set of cursor style parameters. You can construct this object in FRP network, merge it using
    /// its `Semigroup` instance, and finally pass to the cursor to apply the style. Please note
    /// that cursor does not implement any complex style management (like pushing or popping a style
    /// from a style stack) on purpose, as it is stateful, while it is straightforward to implement
    /// it in FRP.
    #[derive(Debug,Clone,Default,PartialEq)]
    pub struct Style {
        $($(#$meta)? $field : Option<StyleValue<$field_type>>),*
    }

    impl Style {
        /// Create a new style with all fields set to default value. Please note that it is
        /// different than empty style, as this one overrides fields with default values when
        /// used in a semigroup operation.
        pub fn new_with_all_fields_default() -> Self {
            $(let $field = Some(StyleValue::new_default());)*
            Self {$($field),*}
        }

        /// Check whether the style is a default, empty one.
        pub fn is_default(&self) -> bool {
            $(self.$field.is_none())&&*
        }
    }

    impl $crate::prelude::PartialSemigroup<&Style> for Style {
        #[allow(clippy::clone_on_copy)]
        fn concat_mut(&mut self, other:&Self) {
            $(if self.$field . is_none() { self.$field = other.$field . clone() })*
        }
    }

    impl $crate::prelude::PartialSemigroup<Style> for Style {
        fn concat_mut(&mut self, other:Self) {
            self.concat_mut(&other)
        }
    }
};}
