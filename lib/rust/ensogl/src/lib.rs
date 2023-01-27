//! EnsoGL is a blazing fast vector rendering engine. To learn more about its features and
//! architecture design, read the [`README.md`].
//!
//!
//!
//! # Debug Display Modes
//! Development of visual components is hard, especially if they react to asynchronous events or are
//! animated. Thus, EnsoGL provides you with multiple Debug Display Modes, allowing you to see the
//! scene from different perspectives. The modes can be switched by using <kbd>ctrl + alt +
//! 0-9</kbd> shortcuts.
//!
//!
//! ## The Default Mode (<kbd>ctrl + alt + 0</kbd>)
//! This is the resulting picture that you see by default. For the purpose of this documentation, we
//! are using the "Focus Management" demo scene:
//!
//! https://user-images.githubusercontent.com/1623053/203922752-70687520-11c1-4808-b4fc-519ab273a755.mov
//!
//!
//! ## The Default Sprite UV Mode (<kbd>ctrl + alt + 1</kbd>)
//! This mode shows full size of sprites with their UV information encoded mapped to RG channels.
//! Please note that in case of shapes, UV coordinates may exceed the [0, 1] range in order to
//! improve shapes anti-aliasing. The UV space outside the [0, 1] range has a blue tint applied:
//!
//! https://user-images.githubusercontent.com/1623053/203923358-f6785ae8-e958-44c1-8403-0871a430e581.mov
//!
//!
//! ## The Debug Sprite Overview Mode (<kbd>ctrl + alt + 2</kbd>)
//! In this mode all shapes are colored based on their ID-encoding with anti-aliasing disabled.
//!
//! By moving the mouse vertically, you can change the border size around sprites. It allows
//! discovering small, fully transparent, or degenerated sprites easily. By moving the mouse
//! horizontally, you can change the sprite opacity, which allows discovering sprites hidden behind
//! other sprites.
//!
//! By clicking the mouse, you can change the color seed for the ID-encoding. In this mode,
//! mouse interactions with the scene (e.g. clicking) are disabled.
//!
//! https://user-images.githubusercontent.com/1623053/203924084-cb387fa8-bb18-47c9-adf5-7fc5e022432d.mov
//!
//! ## The Debug Sprite Grid Mode (<kbd>ctrl + alt + 3</kbd>)
//! In this mode, a pixel-perfect grid is displayed over shapes colored based on their ID-encoding.
//!
//! The grid is displayed for each sprite using the sprite's local coordinate system. When zooming,
//! the grid density is refined. With the maximum zoom, the grid elements are equal to one virtual
//! pixel on the screen (on high-density screens, such as Retina, the grid elements are equal to
//! several physical pixels).
//!
//! By clicking the mouse, you can change the color seed for the ID-encoding. In this mode, mouse
//! interactions with the scene (e.g. clicking) are disabled.
//!
//! Please note, that rendering artifacts can be observed when zooming in/out the grid (lines can
//! "flicker"). This is because the grid view is implemented using a very simple, non-antialiased
//! equation that does not use EnsoGL's shape system. This is done on purpose. Bugs in the shape
//! renderer should not cause the pixel grid to be rendered incorrectly.
//!
//! https://user-images.githubusercontent.com/1623053/203925350-e741f5f1-c099-4a8e-a0ae-b739c78fc7c6.mov
//!
//!
//! ## The Debug SDF Mode (<kbd>ctrl + alt + 4</kbd>)
//! In this mode, all shapes are rendered as SDF equation iso-lines. They represent the distance
//! from the shape border. They also visualize what will happen to the shape after it is shrinked or
//! expanded.
//!
//! In most cases, expanded shapes have more rounded corners, while shrinked shapes, less rounded
//! ones.
//!
//! https://user-images.githubusercontent.com/1623053/203926029-40f554ed-2418-4a16-97f8-4102beb155cc.mov
//!
//!
//! ## The Debug Shape AA Span Mode (<kbd>ctrl + alt + 5</kbd>)
//! In order to properly display a shape close to the borders of its canvas, the underlying sprite
//! size needs to be increased. For example, when displaying a rectangle, after moving the shape 0.5
//! to the right, and slightly zooming the scene, some pixels on the left and right of the rectangle
//! should be anti-aliased. As the anti-aliasing is performed in the fragment shader, the sprite
//! size needs to be increased to cover this additional anti-aliased area. In this mode this
//! additional anti-aliased area is displayed in red. Please note that the area size changes
//! depending on the zoom level.
//!
//! https://user-images.githubusercontent.com/1623053/203926879-255f401e-ff52-4ae6-8cca-883b16c8e39b.mov
//!
//!
//! ## The Debug Instance ID Mode (<kbd>ctrl + alt + 6</kbd>)
//! Each shape instance has a separate ID which is used to identify the shape after a mouse
//! interaction occurs (such as mouse hover, or mouse click). In this mode, all shapes are colored
//! based on their ID-encoding with anti-aliasing disabled.
//!
//! By clicking the mouse, you can change the color seed for the ID-encoding. In this mode,
//! mouse interactions with the scene (e.g. clicking) are disabled.
//!
//! https://user-images.githubusercontent.com/1623053/203927187-33ee6a81-6251-4dfe-8238-4181ebd289fa.mov
//!
//!
//! ## Invalid Debug Display Modes (<kbd>ctrl + alt + 7-9</kbd>)
//! These modes do not exist yet. If they are selected, a checkerboard pattern will be displayed
//! instead:
//!
//! https://user-images.githubusercontent.com/1623053/203927501-31b73b56-414e-40d2-aec0-cb90dad6a908.mov

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]


// ==============
// === Export ===
// ==============

pub use ensogl_core::*;



/// Data type declarations.
pub mod data {
    pub use ensogl_core::data::*;
    pub use ensogl_text as text;
}

/// Graphical interface related components, like buttons, sliders, or text areas.
pub mod gui {
    pub use ensogl_core::gui::*;
    pub use ensogl_text::component as text;
}
