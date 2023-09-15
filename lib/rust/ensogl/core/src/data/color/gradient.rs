//! This module defines color structures and associated modifiers.

use crate::prelude::*;
use crate::system::gpu::shader::glsl::traits::*;

use crate::display::shape::Var;
use crate::system::gpu::shader::glsl::Glsl;



// ====================
// === ControlPoint ===
// ====================

/// Control point of the gradient. It defines a color at a specific gradient offset. The offset
/// of `0` means the beginning of the gradient. The offset of `1` means its end.
#[derive(Clone, Debug, Default)]
pub struct ControlPoint<Color> {
    /// Offset of the control point in [0..1] range.
    pub offset: f32,
    /// Color of this control point.
    pub color:  Color,
}

impl<Color> ControlPoint<Color> {
    /// Constructor.
    pub fn new(offset: f32, color: Color) -> Self {
        Self { offset, color }
    }
}



// ==============
// === Linear ===
// ==============

/// A range of position-dependent colors encoded as control points. Control points do not contain
/// any information about incoming or outgoing slope, so the interpolation between them is linear.
#[derive(Clone, Debug, Derivative)]
#[derivative(Default(bound = ""))]
pub struct Linear<Color> {
    control_points: Vec<ControlPoint<Color>>,
}

impl<Color> Linear<Color> {
    /// Constructor.
    pub fn empty() -> Self {
        default()
    }

    /// Constructor.
    pub fn new(start: impl Into<Color>, end: impl Into<Color>) -> Self {
        let this = Self::empty();
        this.add(0.0, start).add(1.0, end)
    }

    /// Add a new control point. The offset needs to be in range [0..1].
    pub fn add(mut self, offset: f32, color: impl Into<Color>) -> Self {
        self.control_points.push(ControlPoint::new(offset, color.into()));
        self
    }

    /// Convert this gradient to SDF sampler gradient.
    pub fn sdf_sampler(self) -> SdfSampler<Self> {
        SdfSampler::new(self)
    }
}

impls! { [Color] From<&Linear<Color>> for Glsl
where [Color:RefInto<Glsl>] {
    |t| {
        let args = t.control_points.iter().map(|control_point| {
            let offset = control_point.offset.glsl();
            let color  = control_point.color.glsl();
            format!("gradient_control_point({offset},{color})")
        }).join(",");
        format!("gradient({args})").into()
    }
}}



// ==================
// === SdfSampler ===
// ==================

/// Default start distance of the distance gradient.
pub const DEFAULT_DISTANCE_GRADIENT_SPREAD: f32 = 0.0;

/// Default end distance of the distance gradient.
pub const DEFAULT_DISTANCE_GRADIENT_SIZE: f32 = 10.0;

/// A gradient which transforms a linear gradient to a gradient along the signed distance field.
/// The slope parameter modifies how fast the gradient values are changed, allowing for nice,
/// smooth transitions.
#[derive(Clone, Debug)]
pub struct SdfSampler<Gradient> {
    /// The distance from the shape border at which the gradient should start.
    pub spread:   Var<f32>,
    /// The size of the gradient in the SDF space.
    pub size:     Var<f32>,
    /// The gradient slope modifier. Defines how fast the gradient values change.
    pub slope:    Slope,
    /// The underlying gradient.
    pub gradient: Gradient,
}

impl<Gradient> SdfSampler<Gradient> {
    /// Constructs a new gradient with `spread` and `size` set to
    /// `DEFAULT_DISTANCE_GRADIENT_SPREAD` and `DEFAULT_DISTANCE_GRADIENT_SIZE` respectively.
    pub fn new(gradient: Gradient) -> Self {
        let spread = DEFAULT_DISTANCE_GRADIENT_SPREAD.into();
        let size = DEFAULT_DISTANCE_GRADIENT_SIZE.into();
        let slope = Slope::Smooth;
        Self { spread, size, slope, gradient }
    }

    /// Constructor setter for the `spread` field.
    pub fn spread(mut self, t: impl Into<Var<f32>>) -> Self {
        self.spread = t.into();
        self
    }

    /// Constructor setter for the `size` field.
    pub fn size(mut self, t: impl Into<Var<f32>>) -> Self {
        self.size = t.into();
        self
    }

    /// Constructor setter for the `slope` field.
    pub fn slope(mut self, t: Slope) -> Self {
        self.slope = t;
        self
    }
}


// === Instances ===

impl<Gradient> HasItem for SdfSampler<Gradient> {
    type Item = Gradient;
}

impl<Gradient> ItemRef for SdfSampler<Gradient> {
    fn item(&self) -> &Self::Item {
        &self.gradient
    }
}

impls! {[G:RefInto<Glsl>] From< SdfSampler<G>> for Glsl { |g| { (&g).into() } }}
impls! {[G:RefInto<Glsl>] From<&SdfSampler<G>> for Glsl {
    |g| {
        let size   = format!("{}", g.size.glsl());
        let offset = format!("-shape.sdf.distance + {}", g.spread.glsl());
        let norm   = format!("clamp(({offset}) / ({size}))");
        let t      = match &g.slope {
            Slope::Linear        => norm,
            Slope::Smooth        => format!("smoothstep(0.0,1.0,{norm})"),
            Slope::Exponent(exp) => format!("pow({norm},{})", exp.glsl()),
            Slope::InvExponent(exp) => format!("1.0-pow(1.0-{norm},{})", exp.glsl()),
        };
        let expr   = format!("sample({},{t})", g.gradient.glsl());
        expr.into()
    }
}}

macro_rules! define_slope {
    ($($(#$docs:tt)* $name:ident $(($($arg:ident : $arg_type:ty),*))? $fn_name:ident),* $(,)?) => {
        /// Defines how fast gradient values change.
        #[derive(Clone,Debug)]
        pub enum Slope {
            $($(#$docs)* $name $(($($arg_type),*))? ),*
        }

        impl<Gradient> SdfSampler<Gradient> {
            $(
                /// Constructor setter for the `slope` field.
                pub fn $fn_name(self, $($($arg : $arg_type),*)?) -> Self {
                    self.slope(Slope::$name $(($($arg),*))? )
                }
            )*
        }
    };
}

define_slope! {
    /// Defines a linear gradient.
    Linear linear,
    /// Perform Hermite interpolation between gradient values. See `GLSL` `smoothstep` for
    /// reference.
    Smooth smooth,
    /// Raises the normalized gradient offset to the given power and uses it as the interpolation
    /// step.
    Exponent(exp:Var<f32>) exponent,
    /// Raises the normalized gradient offset to the given power and uses it as the interpolation
    /// step.
    InvExponent(exp:Var<f32>) inv_exponent,
}
