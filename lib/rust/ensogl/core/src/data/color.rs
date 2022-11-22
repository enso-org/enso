//! Generic color management implementation. Implements multiple color spaces, including `Rgb`,
//! `LinearRgb`, `Hsv`, `Hsl`, `Xyz`, `Lab`, `Lch`, and others. Provides conversion utilities and
//! many helpers. It is inspired by different libraries, including Rust Palette. We are not using
//! Palette here because it is buggy (https://github.com/Ogeon/palette/issues/187), uses bounds
//! on structs which makes the bound appear in places they should not, uses too strict bounds on
//! components, does not provide many useful conversions, does not allow for mixing colors in sRGB
//! space (which should not be allowed in general but web browses do it this way), and is
//! implemented in a very complex way. This library is not so generic, uses `f32` everywhere and is
//! much simpler. It also implements all the color spaces, much more conversions, much nicer API
//! to work with and uses macros to minimize a lot of boilerplate of `Palette` code. Moreover, this
//! implementation uses `enso-generics` to allow very generic access to color components.
//!
//! **WARNING**
//! Be extra careful when developing color conversion equations. Many equations were re-scaled to
//! make them more pleasant to work with, however, the equations you will fnd will probably work on
//! different value ranges. Read documentation for each color space very carefully.


// ==============
// === Export ===
// ==============

pub mod animation;
pub mod component;
pub mod data;
pub mod gradient;
pub mod mix;
pub mod space;

pub use self::data::*;
pub use animation::Animation;
pub use component::*;
pub use mix::mix;
pub use space::*;
