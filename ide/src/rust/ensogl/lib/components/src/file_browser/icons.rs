//! This module provides all icons used in the file browser.
use ensogl_core::prelude::*;

use ensogl_core::display::object::{ObjectOps, Instance};
use ensogl_core::display::shape::*;
use ensogl_core::data::color;
use ensogl_core::display;



// =================
// === Constants ===
// =================

/// The width of all icons.
pub const ICON_SIZE: f32 = 16.0;

// Due to a rendering error, shapes appear too big when the camera is zoomed in very closely.
// (Documented here: https://github.com/enso-org/ide/issues/1698)
// To compensate for this, we apply `.shrink(SHRINK_AMOUNT)` to all icons in this file. By default,
// `SHRINK_AMOUNT` should be set to 0.0 to make the icons appear right on the default zoom level.
// When examining the icons closely, with a strong zoom, `SHRINK_AMOUNT` should be set to 0.4 to
// make the icons apear right on that zoom level.
const SHRINK_AMOUNT: f32 = 0.0;
// const SHRINK_AMOUNT : f32 = 0.4;

// TODO: Take this value from styles. (https://github.com/enso-org/ide/issues/1694)
fn standard_color() -> color::Rgba {
    color::Rgba(0.5, 0.5, 0.5, 1.0)
}

// TODO: Take this value from styles. (https://github.com/enso-org/ide/issues/1694)
fn focused_color() -> color::Rgba {
    color::Rgba(1.0, 1.0, 1.0, 1.0)
}



// ===================
// === DynamicIcon ===
// ===================

/// Icons to be used in the file browser. They have a normal state and a focused state, in which
/// they may change their color and weight.
pub trait DynamicIcon: display::Object+Debug {

    /// Sets whether this icon is focused, changing it's appearance.
    fn set_focused(&self, focused:bool);
}



// ==============
// === Folder ===
// ==============

/// The shape definition for a folder icon.
pub mod folder {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style,color_rgba:Vector4,stroke_width:f32) {
            let stroke_width : Var<Pixels> = stroke_width.into();

            let base = Rect((15.0.px(),11.0.px()));
            let base = base.corners_radius(1.5.px());
            let base = base.translate((0.0.px(),(-0.5).px()));
            let tab  = Rect((5.5.px(),5.0.px()));
            let tab  = tab.corners_radius(1.5.px());
            let tab  = tab.translate(((-4.75).px(),3.5.px()));

            let outline = base + tab;
            let cut_out = outline.shrink(&stroke_width);

            let middle_line = Rect((15.0.px(),&stroke_width));
            let middle_line = middle_line.translate((0.0.px(),2.5.px() - &stroke_width / 2.0));

            let shape = outline - cut_out + middle_line;
            let shape = shape.fill(color_rgba);
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// A folder icon.
#[derive(Debug)]
pub struct Folder(folder::View);

impl Folder {
    /// Construct a new icon.
    pub fn new() -> Self {
        let shape_view = folder::View::new(Logger::new("file_browser::icon::Folder"));
        shape_view.size.set(Vector2(ICON_SIZE, ICON_SIZE));
        let icon = Folder(shape_view);
        icon.set_focused(false);
        icon
    }
}

impl display::Object for Folder {
    fn display_object(&self) -> &Instance {
        self.0.display_object()
    }
}

impl DynamicIcon for Folder {
    fn set_focused(&self, focused:bool) {
        self.0.stroke_width.set(if focused {1.5} else {1.0});
        self.0.color_rgba.set(if focused {focused_color().into()} else {standard_color().into()})
    }
}

impl Default for Folder {
    fn default() -> Self {
        Self::new()
    }
}



// ============
// === Home ===
// ============

/// The shape definition for a home icon.
pub mod home {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style,color_rgba:Vector4,stroke_width:f32) {
            let stroke_width : Var<Pixels> = stroke_width.into();

            let base = Rect((12.0.px(),8.5.px()));
            let base = base.corners_radiuses(0.0.px(), 0.0.px(), 2.0.px(), 2.0.px());
            let base = base.translate((0.0.px(),(-2.75).px()));

            let inner_radius = 2.0.px() - &stroke_width;

            let cut_out = Rect((12.px()-&stroke_width*2.0,14.0.px()));
            let cut_out = cut_out.corners_radiuses(0.0.px(),0.0.px(),&inner_radius,&inner_radius);
            let cut_out = cut_out.translate_y(&stroke_width);

            let door_inner = Rect((1.0.px(), 3.5.px()));
            let door_inner = door_inner.translate((0.0.px(),(-5.25).px()+&stroke_width));
            let door_outer = door_inner.grow(&stroke_width);
            let door       = door_outer - door_inner;

            let roof_left  = Rect((9.975.px(),&stroke_width));
            let roof_left  = roof_left.translate_y(-&stroke_width/2.0);
            let roof_left  = roof_left.rotate((-40.0f32).to_radians().radians());
            let roof_left  = roof_left.translate(((-3.82).px(),3.5.px()));
            let roof_right = Rect((9.975.px(),&stroke_width));
            let roof_right = roof_right.translate_y(-&stroke_width/2.0);
            let roof_right = roof_right.rotate(40.0f32.to_radians().radians());
            let roof_right = roof_right.translate((3.82.px(),3.5.px()));
            let roof       = roof_left + roof_right;

            let chimney = Rect((&stroke_width,3.0.px()));
            let chimney = chimney.translate((5.5.px()-&stroke_width/2.0, 3.5.px()));

            let shape = base - cut_out + door + roof + chimney;
            let shape = shape.fill(color_rgba);
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// A home icon.
#[derive(Debug)]
pub struct Home(home::View);

impl Home {
    /// Construct a new icon.
    pub fn new() -> Self {
        let shape_view = home::View::new(Logger::new("file_browser::icon::Home"));
        shape_view.size.set(Vector2(ICON_SIZE, ICON_SIZE));
        let icon = Home(shape_view);
        icon.set_focused(false);
        icon
    }
}

impl display::Object for Home {
    fn display_object(&self) -> &Instance {
        self.0.display_object()
    }
}

impl DynamicIcon for Home {
    fn set_focused(&self, focused:bool) {
        self.0.stroke_width.set(if focused {1.5} else {1.0});
        self.0.color_rgba.set(if focused {focused_color().into()} else {standard_color().into()})
    }
}

impl Default for Home {
    fn default() -> Self {
        Self::new()
    }
}



// ============
// === Root ===
// ============

/// The shape definition for a root icon.
pub mod root {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style,color_rgba:Vector4,stroke_width:f32) {
            let stroke_width : Var<Pixels> = stroke_width.into();

            let outer   = Circle(6.5.px());
            let cut_out = outer.shrink(stroke_width);
            let outer   = outer - cut_out;

            let inner = Circle(2.0.px());

            let shape = inner + outer;
            let shape = shape.fill(color_rgba);
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// A root icon.
#[derive(Debug)]
pub struct Root(root::View);

impl Root {
    /// Construct a new icon.
    pub fn new() -> Self {
        let shape_view = root::View::new(Logger::new("file_browser::icon::Root"));
        shape_view.size.set(Vector2(ICON_SIZE, ICON_SIZE));
        let icon = Root(shape_view);
        icon.set_focused(false);
        icon
    }
}

impl display::Object for Root {
    fn display_object(&self) -> &Instance {
        self.0.display_object()
    }
}

impl DynamicIcon for Root {
    fn set_focused(&self, focused:bool) {
        self.0.stroke_width.set(if focused {1.5} else {1.0});
        self.0.color_rgba.set(if focused {focused_color().into()} else {standard_color().into()})
    }
}

impl Default for Root {
    fn default() -> Self {
        Self::new()
    }
}



// ============
// === File ===
// ============

/// The shape definition for a file icon.
pub mod file {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style,color_rgba:Vector4,stroke_width:f32) {
            let block00 = Rect((4.0.px(),3.0.px())).translate(((-5.0).px(),(-4.0).px()));
            let block10 = Rect((4.0.px(),3.0.px())).translate((  0.0.px() ,(-4.0).px()));
            let block20 = Rect((4.0.px(),3.0.px())).translate((  5.0.px( ),(-4.0).px()));

            let block01 = Rect((4.0.px(),3.0.px())).translate(((-5.0).px(),0.0.px()));
            let block11 = Rect((4.0.px(),3.0.px())).translate((  0.0.px() ,0.0.px()));
            let block21 = Rect((4.0.px(),3.0.px())).translate((  5.0.px() ,0.0.px()));

            let block02 = Rect((4.0.px(),3.0.px())).translate(((-5.0).px(), 4.0.px()));
            let block12 = Rect((4.0.px(),3.0.px())).translate((  0.0.px() , 4.0.px()));
            let block22 = Rect((4.0.px(),3.0.px())).translate((  5.0.px() , 4.0.px()));

            let grid = block00 + block10 + block20 + block01 + block11 + block21 + block02 + block12
                + block22;

            let frame = Rect((14.0.px(),11.0.px())).corners_radius(1.5.px());

            let shape = grid * frame;
            let shape = shape.fill(color_rgba);
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// A file icon.
#[derive(Debug)]
pub struct File(file::View);

impl File {
    /// Construct a new icon.
    pub fn new() -> Self {
        let shape_view = file::View::new(Logger::new("file_browser::icon::File"));
        shape_view.size.set(Vector2(ICON_SIZE, ICON_SIZE));
        let icon = File(shape_view);
        icon.set_focused(false);
        icon
    }
}

impl display::Object for File {
    fn display_object(&self) -> &Instance {
        self.0.display_object()
    }
}

impl DynamicIcon for File {
    fn set_focused(&self, focused:bool) {
        self.0.stroke_width.set(if focused {1.5} else {1.0});
        let color: Vector4 = if focused {
            focused_color().into()
        } else {
            color::Rgba(0.475,0.678,0.216,1.0).into()
        };
        self.0.color_rgba.set(color);
    }
}

impl Default for File {
    fn default() -> Self {
        Self::new()
    }
}



// =============
// === Arrow ===
// =============

/// The shape definition for an arrow icon.
pub mod arrow {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style,color_rgba:Vector4,stroke_width:f32) {
            let stroke_width : Var<Pixels> = stroke_width.into();
            let delta_x       = 2.75_f32;
            let delta_y       = 3.0_f32;
            let angle         = delta_y.atan2(delta_x);
            let stroke_length = &stroke_width + (delta_x.pow(2.0) + delta_y.pow(2.0)).sqrt().px();

            let upper = Rect((&stroke_length,&stroke_width));
            let upper = upper.corners_radius(&stroke_width/2.0);
            let upper = upper.rotate(angle.radians());
            let upper = upper.translate_y((delta_y/2.0).px());

            let lower = Rect((&stroke_length,&stroke_width));
            let lower = lower.corners_radius(&stroke_width/2.0);
            let lower = lower.rotate((-angle).radians());
            let lower = lower.translate_y((-delta_y/2.0).px());

            let shape = upper + lower;
            let shape = shape.fill(color_rgba);
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// An arrow icon.
#[derive(Debug)]
pub struct Arrow(arrow::View);

impl Arrow {
    /// Construct a new icon.
    pub fn new() -> Self {
        let shape_view = arrow::View::new(Logger::new("file_browser::icon::Arrow"));
        shape_view.size.set(Vector2(ICON_SIZE, ICON_SIZE));
        let icon = Arrow(shape_view);
        icon.set_focused(false);
        icon
    }
}

impl display::Object for Arrow {
    fn display_object(&self) -> &Instance {
        self.0.display_object()
    }
}

impl DynamicIcon for Arrow {
    fn set_focused(&self, focused:bool) {
        self.0.stroke_width.set(if focused {1.5} else {1.0});
        self.0.color_rgba.set(if focused {focused_color().into()} else {standard_color().into()})
    }
}

impl Default for Arrow {
    fn default() -> Self {
        Self::new()
    }
}



// ===============
// === Project ===
// ===============

/// The shape definition for a project icon.
pub mod project {
    use super::*;

    ensogl_core::define_shape_system! {
        (style:Style,color_rgba:Vector4,stroke_width:f32) {
            let stroke_width : Var<Pixels> = stroke_width.into();

            let left  = Rect((&stroke_width,10.0.px())).translate_x((-5.5).px()+&stroke_width/2.0);
            let right = Rect((&stroke_width,10.0.px())).translate_x(5.5.px()-&stroke_width/2.0);

            let top_ellipse = Ellipse(5.5.px(),1.5.px());
            let top_upper   = &top_ellipse - top_ellipse.translate_y(-&stroke_width);
            let top_lower   = &top_ellipse - top_ellipse.translate_y(&stroke_width);
            let top         = top_upper + top_lower;
            let top         = top.translate_y(5.0.px());

            let bottom_outer = Ellipse(5.5.px(),2.0.px());
            let bottom_outer = bottom_outer.translate_y(0.5.px()-&stroke_width);
            let bottom_inner = Ellipse(4.5.px(),1.5.px());
            let bottom       = bottom_outer - bottom_inner;
            let bottom       = bottom * HalfPlane();
            let bottom       = bottom.translate_y((-5.5).px()+&stroke_width);

            let upper_middle_outer = Ellipse(5.0.px(),1.6666.px());
            let upper_middle_inner = upper_middle_outer.translate_y(&stroke_width/2.0);
            let upper_middle       = upper_middle_outer - upper_middle_inner;
            let upper_middle       = upper_middle.translate_y(1.9166.px());

            let lower_middle_outer = Ellipse(5.0.px(),1.83333.px());
            let lower_middle_inner = lower_middle_outer.translate_y(&stroke_width/2.0);
            let lower_middle       = lower_middle_outer - lower_middle_inner;
            let lower_middle       = lower_middle.translate_y((-1.4166).px());

            let shape = left + right + top + bottom + upper_middle + lower_middle;
            let shape = shape.fill(color_rgba);
            let shape = shape.shrink(SHRINK_AMOUNT.px());
            shape.into()
        }
    }
}

/// A project icon.
#[derive(Debug)]
pub struct Project(project::View);

impl Project {
    /// Construct a new icon.
    pub fn new() -> Self {
        let shape_view = project::View::new(Logger::new("file_browser::icon::Project"));
        shape_view.size.set(Vector2(ICON_SIZE, ICON_SIZE));
        let icon = Project(shape_view);
        icon.set_focused(false);
        icon
    }
}

impl display::Object for Project {
    fn display_object(&self) -> &Instance {
        self.0.display_object()
    }
}

impl DynamicIcon for Project {
    fn set_focused(&self, focused:bool) {
        self.0.stroke_width.set(if focused {1.5} else {1.0});
        self.0.color_rgba.set(if focused {focused_color().into()} else {standard_color().into()})
    }
}

impl Default for Project {
    fn default() -> Self {
        Self::new()
    }
}
