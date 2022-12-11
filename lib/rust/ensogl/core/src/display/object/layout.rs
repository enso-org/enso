//! Display object layout utilities.

use crate::prelude::*;

use unit2::Fraction;
use unit2::Percent;



// ============
// === Unit ===
// ============


/// Unit for display object layout.
#[derive(Clone, Copy, Debug, Display, PartialEq, From)]
pub enum Unit {
    /// Pixel distance.
    Pixels(f32),
    /// Fraction of the unused space, if any. For example, if you set the layout gap to be
    /// `1.fr()`, all the gaps will have the same size to fill all the space in the parent
    /// container.
    Fraction(Fraction),
    /// Percent of the parent size.
    Percent(Percent),
}

impl Unit {
    pub fn as_fraction(self) -> Option<Fraction> {
        match self {
            Self::Fraction(f) => Some(f),
            _ => None,
        }
    }

    /// Resolve the unit to a pixel value.
    pub fn resolve(&self, parent_size: f32, free_space: f32, total_fraction: Fraction) -> f32 {
        match self {
            Unit::Pixels(value) => *value,
            Unit::Percent(value) => value.unchecked_raw() / 100.0 * parent_size,
            Unit::Fraction(value) =>
                value.unchecked_raw() / total_fraction.unchecked_raw() * free_space,
        }
    }

    /// Resolve the unit to fixed pixel value. If the unit was set to be a fraction or percent, it
    /// will result in 0. This is mostly used in layouting algorithm.
    pub fn resolve_const_only(&self) -> f32 {
        match self {
            Unit::Pixels(value) => *value,
            Unit::Fraction(_) => 0.0,
            Unit::Percent(_) => 0.0,
        }
    }

    pub fn resolve_const_only2(&self) -> Option<f32> {
        match self {
            Unit::Pixels(value) => Some(*value),
            _ => None,
        }
    }

    pub fn resolve_const_and_percent(&self, parent_size: f32) -> Option<f32> {
        match self {
            Unit::Pixels(value) => Some(*value),
            Unit::Percent(value) => Some(value.unchecked_raw() / 100.0 * parent_size),
            _ => None,
        }
    }
}

impl Default for Unit {
    fn default() -> Self {
        Self::Pixels(0.0)
    }
}

impl From<i32> for Unit {
    fn from(value: i32) -> Self {
        Self::Pixels(value as f32)
    }
}



// ============
// === Size ===
// ============

/// The resizing of display objects.
#[derive(Clone, Copy, Debug, Default, PartialEq, From)]
pub enum Size {
    /// In this mode, the display object size will be set to the size of its content. In case of
    /// siplay object with no children, their size will be set to 0.
    #[default]
    Hug,
    /// In this mode, the display object size is provided explicitly.
    Fixed(Unit),
}

impl Size {
    /// Checks whether the resizing mode is [`Size::Hug`].
    pub fn is_hug(self) -> bool {
        self == Size::Hug
    }

    /// Checks whether the resizing mode is [`Size::Fixed`].
    pub fn is_fixed(self) -> bool {
        match self {
            Size::Fixed(_) => true,
            _ => false,
        }
    }

    pub fn as_fraction(self) -> Option<Fraction> {
        match self {
            Size::Fixed(unit) => unit.as_fraction(),
            _ => None,
        }
    }
}

impl From<f32> for Size {
    fn from(value: f32) -> Self {
        Self::Fixed(Unit::from(value))
    }
}

impl From<i32> for Size {
    fn from(value: i32) -> Self {
        Self::Fixed(Unit::from(value))
    }
}

impl From<Fraction> for Size {
    fn from(value: Fraction) -> Self {
        Self::Fixed(Unit::from(value))
    }
}

impl From<Percent> for Size {
    fn from(value: Percent) -> Self {
        Self::Fixed(Unit::from(value))
    }
}



// ===================
// === SideSpacing ===
// ===================

/// Data model used for expressing margin and padding.
#[derive(Clone, Copy, Debug, Default, PartialEq)]
#[allow(missing_docs)]
pub struct SideSpacing<T = Unit> {
    pub start: T,
    pub end:   T,
}

impl<T> SideSpacing<T> {
    /// Constructor.
    pub fn new(start: T, end: T) -> Self {
        Self { start, end }
    }

    /// A sum of start and end values.
    pub fn total(self) -> T
    where T: Add<Output = T> {
        self.start + self.end
    }
}

impl SideSpacing<Unit> {
    pub fn resolve(
        self,
        parent_size: f32,
        free_space: f32,
        total_fraction: Fraction,
    ) -> SideSpacing<f32> {
        SideSpacing::new(
            self.start.resolve(parent_size, free_space, total_fraction),
            self.end.resolve(parent_size, free_space, total_fraction),
        )
    }

    pub fn resolve_fixed(self) -> SideSpacing<f32> {
        SideSpacing::new(self.start.resolve_const_only(), self.end.resolve_const_only())
    }
}

impl<T: Copy> From<T> for SideSpacing<T> {
    fn from(value: T) -> Self {
        Self { start: value, end: value }
    }
}

#[macro_export]
macro_rules! with_display_object_side_spacing_matrix {
    ($f:path $([$($args:tt)*])?) => {
        $f! { $([$($args)*])? [[left x start] [right x end] [bottom y start] [top y end]] }
    }
}
