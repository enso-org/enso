//! This module defines color structures and associated modifiers.

use crate::prelude::*;

use crate::system::gpu::shader::glsl::Glsl;
use crate::system::gpu::shader::glsl::traits::*;



// ====================
// === ControlPoint ===
// ====================

/// Control point of the gradient. It defines a color at a specific gradient offset. The offset
/// of `0` means the beginning of the gradient. The offset of `1` means its end.
#[derive(Clone,Debug,Default)]
pub struct ControlPoint<Color> {
    /// Offset of the control point in [0..1] range.
    pub offset : f32,
    /// Color of this control point.
    pub color  : Color
}

impl<Color> ControlPoint<Color> {
    /// Constructor.
    pub fn new(offset:f32, color:Color) -> Self {
        Self {offset,color}
    }
}



// ================
// === Gradient ===
// ================

/// A a range of position-dependent colors encoded as control points. Control points do not contain
/// any information about incoming or outgoing slope, so the interpolation between them is linear.
#[derive(Clone,Debug,Derivative)]
#[derivative(Default(bound=""))]
pub struct LinearGradient<Color> {
    control_points : Vec<ControlPoint<Color>>,
}

impl<Color> LinearGradient<Color> {
    /// Constructor.
    pub fn new() -> Self {
        default()
    }

    /// Add a new control point. The offset needs to be in range [0..1].
    pub fn add(mut self, offset:f32, color:Color) -> Self {
        self.control_points.push(ControlPoint::new(offset,color));
        self
    }
}

impls! { [Color] From<&LinearGradient<Color>> for Glsl
where [Color:RefInto<Glsl>] {
    |t| {
        let args = t.control_points.iter().map(|control_point| {
            let offset = control_point.offset.glsl();
            let color  = control_point.color.glsl();
            iformat!("gradient_control_point({offset},{color})")
        }).join(",");
        iformat!("gradient({args})").into()
    }
}}



// ==================
// === SdfSampler ===
// ==================

/// Default start distance of the distance gradient.
pub const DEFAULT_DISTANCE_GRADIENT_MIN_DISTANCE : f32 = 0.0;

/// Default end distance of the distance gradient.
pub const DEFAULT_DISTANCE_GRADIENT_MAX_DISTANCE : f32 = 10.0;

/// A gradient which transforms a linear gradient to a gradient along the signed distance field.
/// The slope parameter modifies how fast the gradient values are changed, allowing for nice,
/// smooth transitions.
#[derive(Clone,Debug)]
pub struct SdfSampler<Gradient> {
    /// The distance from the shape border at which the gradient should start.
    pub min_distance : f32,
    /// The distance from the shape border at which the gradient should finish.
    pub max_distance : f32,
    /// The gradient slope modifier. Defines how fast the gradient values change.
    pub slope : Slope,
    /// The underlying gradient.
    pub gradient : Gradient
}

impl<Gradient> SdfSampler<Gradient> {
    /// Constructs a new gradient with `min_distance` and `max_distance` set to
    /// `DEFAULT_DISTANCE_GRADIENT_MIN_DISTANCE` and `DEFAULT_DISTANCE_GRADIENT_MAX_DISTANCE`
    /// respectively.
    pub fn new(gradient:Gradient) -> Self {
        let min_distance = DEFAULT_DISTANCE_GRADIENT_MIN_DISTANCE;
        let max_distance = DEFAULT_DISTANCE_GRADIENT_MAX_DISTANCE;
        let slope        = Slope::Smooth;
        Self {min_distance,max_distance,slope,gradient}
    }

    /// Constructor setter for the `max_distance` field.
    pub fn max_distance(mut self, t:f32) -> Self {
        self.max_distance = t;
        self
    }

    /// Constructor setter for the `slope` field.
    pub fn slope(mut self, t:Slope) -> Self {
        self.slope = t;
        self
    }
}


// === Instances ===

impl<Gradient> HasContent for SdfSampler<Gradient> {
    type Content = Gradient;
}

impl<Gradient> ContentRef for SdfSampler<Gradient> {
    fn content(&self) -> &Self::Content {
        &self.gradient
    }
}

impls! {[G:RefInto<Glsl>] From< SdfSampler<G>> for Glsl { |g| { (&g).into() } }}
impls! {[G:RefInto<Glsl>] From<&SdfSampler<G>> for Glsl {
    |g| {
        let span   = iformat!("{g.max_distance.glsl()} - {g.min_distance.glsl()}");
        let offset = iformat!("-shape.sdf.distance - {g.min_distance.glsl()}");
        let norm   = iformat!("clamp(({offset}) / ({span}))");
        let t      = match g.slope {
            Slope::Linear        => norm,
            Slope::Smooth        => iformat!("smoothstep(0.0,1.0,{norm})"),
            Slope::Exponent(exp) => iformat!("pow({norm},{exp.glsl()})"),
            Slope::InvExponent(exp) => iformat!("1.0-pow(1.0-{norm},{exp.glsl()})"),
        };
        let expr   = iformat!("sample({g.gradient.glsl()},{t})");
        expr.into()
    }
}}


/// Defines how fast gradient values change.
#[derive(Copy,Clone,Debug)]
pub enum Slope {
    /// Defines a linear gradient.
    Linear,
    /// Perform Hermite interpolation between gradient values. See `GLSL` `smoothstep` for
    /// reference.
    Smooth,
    /// Raises the normalized gradient offset to the given power and uses it as the interpolation
    /// step.
    Exponent(f32),
    /// Raises the normalized gradient offset to the given power and uses it as the interpolation
    /// step.
    InvExponent(f32),
}
