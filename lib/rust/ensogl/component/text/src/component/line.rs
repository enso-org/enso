//! Visible line component implementation. Line is a set of visible glyphs.

use crate::prelude::*;
use enso_text::unit::*;
use ensogl_core::display::shape::*;

use crate::buffer::formatting;
use crate::font::glyph::Glyph;

use ensogl_core::display;
use ensogl_core::display::IntoGlsl;
use ensogl_core::Animation;



// ================
// === Ellipsis ===
// ================

const ELLIPSIS_SCALE: f32 = 12.0;
const ELLIPSIS_SHAPE_RADIUS: f32 = 1.0;
const ELLIPSIS_SHAPE_OFFSET: f32 = 2.0;
const ELLIPSIS_TEXT_OFFSET: f32 = 2.0;
const ELLIPSIS_WIDTH: f32 = ELLIPSIS_SHAPE_RADIUS * 2.0 * 3.0 + ELLIPSIS_SHAPE_OFFSET * 2.0;
const ELLIPSIS_ANIMATION_DURATION_MS: f32 = 400.0;
const ELLIPSIS_ANIMATION_OFFSET_MS: f32 = 100.0;

/// Ellipsis, three dots shown when the text is too long.
mod ellipsis {
    use super::*;
    ensogl_core::define_shape_system! {
        (start_time:f32, scale: f32, color_rgb:Vector3<f32>) {
            let time = Var::<f32>::from("input_time");
            let time = time - start_time;
            let radius = (&scale * ELLIPSIS_SHAPE_RADIUS).px();
            let offset = (&scale * (ELLIPSIS_SHAPE_RADIUS * 2.0 + ELLIPSIS_SHAPE_OFFSET)).px();
            let dot1 = Circle(&radius).translate_x(-&offset);
            let dot2 = Circle(&radius);
            let dot3 = Circle(&radius).translate_x(offset);
            let dot1_anim_start = ELLIPSIS_ANIMATION_OFFSET_MS;
            let dot2_anim_start = dot1_anim_start + ELLIPSIS_ANIMATION_OFFSET_MS;
            let dot3_anim_start = dot2_anim_start + ELLIPSIS_ANIMATION_OFFSET_MS;
            let dot1_anim_end = dot1_anim_start + ELLIPSIS_ANIMATION_DURATION_MS;
            let dot2_anim_end = dot2_anim_start + ELLIPSIS_ANIMATION_DURATION_MS;
            let dot3_anim_end = dot3_anim_start + ELLIPSIS_ANIMATION_DURATION_MS;
            let dot1_alpha = time.smoothstep(0.0, dot1_anim_end);
            let dot2_alpha = time.smoothstep(dot2_anim_start, dot2_anim_end);
            let dot3_alpha = time.smoothstep(dot3_anim_start, dot3_anim_end);
            let rgb = color_rgb;
            let color1 = format!("srgba({}.x,{}.y,{}.z,{})", rgb, rgb, rgb, dot1_alpha.glsl());
            let color2 = format!("srgba({}.x,{}.y,{}.z,{})", rgb, rgb, rgb, dot2_alpha.glsl());
            let color3 = format!("srgba({}.x,{}.y,{}.z,{})", rgb, rgb, rgb, dot3_alpha.glsl());
            let dot1 = dot1.fill(color1);
            let dot2 = dot2.fill(color2);
            let dot3 = dot3.fill(color3);
            let shape = dot1 + dot2 + dot3;
            shape.into()
        }
    }
}



// ==================
// === Truncation ===
// ==================

/// Line truncation visual representation, three animated dots displayed if line contains to many
/// glyphs.
#[derive(Debug, Clone, CloneRef, Deref, Default)]
pub struct Truncation {
    rc: Rc<RefCell<Option<TruncationData>>>,
}

impl Truncation {
    fn set_animation_start_time(&self, time: f32) {
        if let Some(data) = &*self.borrow() {
            data.ellipsis.start_time.set(time);
        }
    }
}

/// Internal representation of [`Truncation`].
#[allow(missing_docs)]
#[derive(Debug, Deref, DerefMut)]
pub struct TruncationData {
    #[deref]
    #[deref_mut]
    pub size:     TruncationSize,
    pub ellipsis: ellipsis::View,
}

impl TruncationData {
    /// Constructor.
    fn new(size: impl Into<TruncationSize>, ellipsis: ellipsis::View) -> Self {
        let size = size.into();
        Self { size, ellipsis }
    }

    /// The starting x-axis position.
    pub fn start_x(&self) -> f32 {
        self.ellipsis.position().x - (ELLIPSIS_WIDTH / 2.0) * self.scale
    }

    /// Maximum value of the x-axis position (right border).
    pub fn max_x(&self) -> f32 {
        self.start_x() + self.width_with_text_offset()
    }
}

/// Size properties of [`Truncation`]. Allows to query for the size of the truncation without its
/// creation. This can be used for example to check how much space needs to be left in a line
/// before displaying the truncation.
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, Deref)]
pub struct TruncationSize {
    pub scale: f32,
}

impl TruncationSize {
    /// Constructor.
    pub fn new(scale: f32) -> Self {
        Self { scale }
    }

    /// Get the width of the ellipsis shape. It does not include the offset between the text and the
    /// ellipsis. If you want to include the offset, use `[elli
    pub fn width(&self) -> f32 {
        self.scale * ELLIPSIS_WIDTH
    }

    /// Get the height of the ellipsis shape.
    pub fn height(&self) -> f32 {
        self.scale * ELLIPSIS_SHAPE_RADIUS * 2.0
    }

    /// Get the dimensions of the ellipsis shape.
    pub fn dim(&self) -> Vector2<f32> {
        Vector2(self.width(), self.height())
    }

    /// Get the width of the ellipsis shape including the offset to the preceding text.
    pub fn width_with_text_offset(&self) -> f32 {
        self.scale * ELLIPSIS_TEXT_OFFSET + self.width()
    }

    /// Get the x-axis position of ellipsis after the last glyph.
    pub fn x_after_last_glyph(&self) -> f32 {
        self.scale * (ELLIPSIS_TEXT_OFFSET + ELLIPSIS_WIDTH / 2.0)
    }

    /// Get the y-axis position of ellipsis.
    pub fn y(&self) -> f32 {
        self.scale * (ELLIPSIS_SHAPE_RADIUS / 2.0)
    }
}

impl From<f32> for TruncationSize {
    fn from(scale: f32) -> Self {
        Self::new(scale)
    }
}

impl From<formatting::Size> for TruncationSize {
    fn from(size: formatting::Size) -> Self {
        let scale = size.value / ELLIPSIS_SCALE;
        Self::new(scale)
    }
}


// ===============
// === Metrics ===
// ===============

/// Metrics of the line. It is a combination of metrics of all glyphs in the line. See the following
/// link to learn more about the terms used here:
/// https://en.wikipedia.org/wiki/Ascender_(typography).
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub struct Metrics {
    pub ascender:  f32,
    pub descender: f32,
    pub gap:       f32,
}

impl PartialSemigroup for Metrics {
    fn concat_mut(&mut self, other: Self) {
        self.ascender = self.ascender.max(other.ascender);
        self.descender = self.descender.min(other.descender);
        self.gap = self.gap.max(other.gap);
    }
}



// ===========
// === FRP ===
// ===========

ensogl_core::define_endpoints_2! {
    Input {
        /// Set metrics of this line.
        set_metrics(Metrics),
        /// Set the baseline y-axis position.
        set_baseline(f32),
        skip_baseline_animation(),

        // === Internal API ===

        // Show the truncation ellipsis.
        show_ellipsis(),
    }
    Output {
        metrics(Metrics),
        baseline(f32),
        descent(f32),
    }
}



// ============
// === View ===
// ============

/// Visual line representation. It contains all the visual glyph shapes.
///
/// **Design Notes**
/// The `divs` and `centers` are kept as vectors for performance reasons. Especially, when
/// clicking inside of the text area, it allows us to binary search the place of the mouse
/// pointer.
#[allow(missing_docs)]
#[derive(Debug, Deref)]
#[cfg_attr(not(target_arch = "wasm32"), allow(dead_code))]
pub struct View {
    #[deref]
    pub frp:            Frp,
    pub display_object: display::object::Instance,
    pub glyphs:         VecIndexedBy<Glyph, Column>,
    /// Division points between glyphs. There is always the beginning division point (0.0). If
    /// there are any glyphs, this also contains the last division point, which is the glyph
    /// right hand side + `x_advance`, where `a_advance` is the space to the next glyph place.
    pub divs:           NonEmptyVec<f32>,
    /// Centers between division points. Used for glyph selection with mouse cursor.
    pub centers:        Vec<f32>,
    pub truncation:     Truncation,
    baseline_anim:      Animation<f32>,
}

impl View {
    /// Constructor.
    pub fn new(frame_time: &enso_frp::Stream<f32>) -> Self {
        let frp = Frp::new();
        let network = frp.network();
        let baseline_anim = Animation::new(network);
        let display_object = display::object::Instance::new();
        let glyphs = default();
        let divs = default();
        let centers = default();
        let truncation: Truncation = default();
        let frame_time = frame_time.clone_ref();
        baseline_anim.simulator.update_spring(|s| s * crate::DEBUG_ANIMATION_SPRING_FACTOR);

        frp::extend! {network

            // === Baseline and metrics ===

            baseline_anim.target <+ frp.set_baseline;
            baseline_anim.skip <+ frp.skip_baseline_animation;
            eval baseline_anim.value ((y) display_object.set_position_y(*y));

            new_baseline <- baseline_anim.value.on_change();
            frp.private.output.baseline <+ new_baseline;
            new_descent <- all_with(&new_baseline, &frp.set_metrics,
                |baseline,metrics| baseline + metrics.descender
            );
            frp.private.output.descent <+ new_descent.on_change();
            frp.private.output.metrics <+ frp.set_metrics.on_change();

            // === Truncation ===

            start_time <- frame_time.sample(&frp.show_ellipsis);
            eval start_time ((t) truncation.set_animation_start_time(*t));
        }

        Self { frp, display_object, glyphs, divs, centers, truncation, baseline_anim }
    }

    /// Get glyph for the provided column or create a new one if it does not exist.
    pub fn get_or_create(&mut self, column: Column, cons: impl Fn() -> Glyph) -> &Glyph {
        let missing = column.value as i32 - self.glyphs.len() as i32;
        for _ in 0..=missing {
            self.push_glyph(cons());
        }
        &self.glyphs[column]
    }

    /// Set the truncation of the line to the specified size.
    pub fn set_truncated(&mut self, size: Option<formatting::Size>) {
        if let Some(size) = size {
            let ellipsis = ellipsis::View::new();
            self.add_child(&ellipsis);

            let truncation = TruncationData::new(size, ellipsis);
            let x = self.glyphs.last().map(|g| g.position().x + g.x_advance.get()).unwrap_or(0.0);
            let x = x + truncation.x_after_last_glyph();
            truncation.ellipsis.set_position_xy(Vector2(x, truncation.y()));
            truncation.ellipsis.scale.set(truncation.scale);
            truncation.ellipsis.size.set(truncation.dim());
            let was_truncated = self.truncation.borrow().is_some();
            *self.truncation.borrow_mut() = Some(truncation);
            if !was_truncated {
                self.show_ellipsis();
            }
        } else {
            *self.truncation.borrow_mut() = None;
        }
    }

    /// Line metrics.
    pub fn metrics(&self) -> Metrics {
        self.metrics.value()
    }

    /// Line baseline animation target.
    pub fn baseline(&self) -> f32 {
        self.baseline_anim.target()
    }

    /// Set the division points (offsets between letters). Also updates center points.
    #[cfg_attr(not(target_arch = "wasm32"), allow(dead_code))]
    pub fn set_divs(&mut self, divs: NonEmptyVec<f32>) {
        let div_iter = divs.iter();
        let div_iter_skipped = divs.iter().skip(1);
        self.centers = div_iter.zip(div_iter_skipped).map(|(t, s)| (t + s) / 2.0).collect();
        self.divs = divs;
    }

    /// Finds the div close to the given x-axis position.
    pub fn div_index_close_to(&self, x_pos: f32) -> usize {
        self.centers.binary_search_by(|t| t.partial_cmp(&x_pos).unwrap()).unwrap_both()
    }

    /// Get the division by column.
    pub fn div_by_column(&self, column: Column) -> f32 {
        if column.value < self.divs.len() {
            self.divs[column.value]
        } else {
            // Requested column is bigger then glyph amount. This can happen for example when text
            // is truncated and the cursor is in the truncated area.
            *self.divs.last()
        }
    }

    /// Resize glyph vector and use the provided constructor to create missing glyphs if any.
    pub fn resize_with(&mut self, size: usize, cons: impl Fn() -> Glyph) {
        let display_object = self.display_object().clone_ref();
        self.glyphs.resize_with(size, move || {
            let glyph = cons();
            display_object.add_child(&glyph);
            glyph
        });
    }

    /// Add a new glyph to this line.
    pub fn push_glyph(&mut self, glyph: Glyph) {
        self.add_child(&glyph);
        self.glyphs.push(glyph);
    }
}

impl display::Object for View {
    fn display_object(&self) -> &display::object::Instance {
        &self.display_object
    }
}

impl<'t> IntoIterator for &'t View {
    type Item = &'t Glyph;
    type IntoIter = slice::Iter<'t, Glyph>;
    fn into_iter(self) -> Self::IntoIter {
        self.glyphs.iter()
    }
}
