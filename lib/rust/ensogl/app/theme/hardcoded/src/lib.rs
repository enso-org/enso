//! Builtin themes definition and compile-time generated theme paths (allowing catching improper
//! theme usage during IDE compilation time).

// === Standard Linter Configuration ===
#![deny(non_ascii_idents)]
#![warn(unsafe_code)]
#![allow(clippy::bool_to_int_with_if)]
#![allow(clippy::let_and_return)]
// === Non-Standard Linter Configuration ===
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

use ensogl_core::prelude::ImString;



// ==============
// === Macros ===
// ==============

/// `_define_theme_wrapper_and_literals` helper.
macro_rules! _define_theme_literals {
    ($id:tt $theme:ident [$($path:ident)*]) => {};
    ($id:tt $theme:ident [$($path:ident)*]
        $qual:ident . $($var:ident).+ = $($e:expr),* $(;$($rest:tt)*)?) => {
            _define_theme_literals!{
                $id $theme [$($path)*] $qual { $($var).+ = $($e),* } $($($rest)*)?
            }
    };
    ($id:tt $theme:ident [$($path:ident)*] $var:ident = $($e:expr),* $(;$($rest:tt)*)?) => {
        $theme.set(stringify!($($path.)*$var), _select_theme_expr!{$id $($e),*});
        _define_theme_literals!{$id $theme [$($path)*] $($($rest)*)?}
    };
    ($id:tt $theme:ident [$($path:ident)*] $path_segment:ident {$($t:tt)*} $($rest:tt)*) => {
        _define_theme_literals!{$id $theme [$($path)* $path_segment] $($t)*}
        _define_theme_literals!{$id $theme [$($path)*] $($rest)*}
    };
}

/// `_define_theme_wrapper_and_literals` helper.
macro_rules! _define_theme_modules {
    ([$($path:ident)*]) => {};
    ([$($path:ident)*]
        $qual:ident . $($var:ident).+ = $($e:expr),* $(;$($rest:tt)*)?) => {
            _define_theme_modules!{
                [$($path)*] $qual {$($var).+ = $($e),*} $($($rest)*)?
            }
    };
    ([$($path:ident)*] $var:ident = $($e:expr),* $(;$($rest:tt)*)?) => {
        pub const $var : StaticPath = StaticPath::new(stringify!($($path.)*$var));
        _define_theme_modules!{[$($path)*] $($($rest)*)?}
    };
    ([$($path:ident)*] $path_segment:ident {$($t:tt)*} $($rest:tt)*) => {
        pub mod $path_segment {
            use ensogl_core::display::style::StaticPath;
            pub const HERE : StaticPath = StaticPath::new(stringify!($($path.)*$path_segment));
            _define_theme_modules!{[$($path)* $path_segment] $($t)*}
        }
        _define_theme_modules!{[$($path)*] $($rest)*}
    };
}

/// Select the theme expression by its number.
macro_rules! _select_theme_expr {
    (0 $e0:expr                                         $(,$rest:tt)*) => {
        $e0
    };
    (1 $e0:expr, $e1:expr                               $(,$rest:tt)*) => {
        $e1
    };
    (2 $e0:expr, $e1:expr, $e2:expr                     $(,$rest:tt)*) => {
        $e2
    };
    (3 $e0:expr, $e1:expr, $e2:expr, $e3:expr           $(,$rest:tt)*) => {
        $e3
    };
    (4 $e0:expr, $e1:expr, $e2:expr, $e3:expr, $e4:expr $(,$rest:tt)*) => {
        $e4
    };
}

/// Helper for defining multiple themes as the same time.
macro_rules! _define_themes_wrappers_and_literals {
    ([$($name:ident : $id:tt),*] $body:tt) => {
        /// Builtin themes.
        pub mod builtin {
            use super::*;
            $(_define_theme_wrapper_and_literals! {$name $id $body})*
        }
    }
}

/// Generates code for `StyleManager` from given cascade style definition. It generates module equal
/// to the theme name and there will be function `setup` which creates a theme definition in `app`.
macro_rules! _define_theme_wrapper_and_literals {
    ($name:ident $id:tt {$($t:tt)*}) => {
        #[allow(missing_docs)]
        #[allow(non_snake_case)]
        #[allow(unused_imports)]
        pub mod $name {
            use super::*;
            use ensogl_core::application::Application;
            use ensogl_core::data::color::Lcha;
            use ensogl_core::data::color::Rgb;
            use ensogl_core::data::color::Rgba;
            use ensogl_core::display::style::theme;

            /// Registers the theme in the application.
            pub fn register(theme_manager:impl AsRef<theme::Manager>) {
                let $name = theme::Theme::new();
                _define_theme_literals!{$id $name [] $($t)*}
                theme_manager.as_ref().register(stringify!($name),$name);
            }

            /// Enables the current theme.
            pub fn enable(theme_manager:impl AsRef<theme::Manager>) {
                theme_manager.as_ref().set_enabled(&[stringify!($name)]);
            }
        }
    };
}

/// Used to define default theme. This one aside from generating code for `StyleManager` also
/// creates nested public modules that makes accessing values much better than with bare string
/// literals. It adds the `var` module with string constants, so now, instead of having to get data
/// by string literal - like `style.get("foo.bar.baz",fallback)`, you can do
/// `style.get(theme::foo::bar::baz,fallback)`.
macro_rules! define_themes {
    ($ids:tt $($t:tt)*) => {
        _define_themes_wrappers_and_literals!{$ids { $($t)* }}

        #[allow(non_upper_case_globals)]
        #[allow(missing_docs)]
        #[allow(non_snake_case)]
        pub mod vars {
            use ensogl_core::display::style::StaticPath;
            _define_theme_modules!{[] $($t)*}
        }
        pub use vars::*;
    };
}



// =============
// === Theme ===
// =============

/// Enum holding available themes for ease of access.
#[allow(missing_docs)]
#[derive(Clone, Debug)]
pub enum Theme {
    Light,
    Dark,
    Other(ImString),
}

impl Default for Theme {
    fn default() -> Self {
        Theme::Light
    }
}



// ===========================
// === Light & Dark Themes ===
// ===========================

define_themes! { [light:0, dark:1]
    application {
        // Original RGB values (for reference after fixing color-conversion issues)
        // light: rgb(249,250,251), old-dark: Lcha(0.13,0.014,0.18,1.0), dark: rgb(32,34,36)
        background = Rgba(0.976,0.98,0.984,1.0) , Rgba(0.125,0.133,0.141,1.0);
        tooltip {
            hide_delay_duration_ms = 150.0, 150.0;
            show_delay_duration_ms = 150.0, 150.0;
        }
        component_browser {
            panels_gap = 3.0, 3.0;
            documentation {
                width = 369.0, 369.0;
            }
            searcher {
                list_panel {
                    content_width = 400.0, 400.0;
                    content_height = 398.0, 398.0;
                    content_padding = 3.0, 3.0;
                    content_corner_radius = 15.0, 15.0;
                    content_background_color = Rgba::new(252.0 / 256.0, 254.0 / 255.0, 1.0, 1.0),Rgba::new(252.0 / 256.0, 254.0 / 255.0, 1.0, 1.0);

                    breadcrumbs_crop_left = 9.0, 9.0;
                    breadcrumbs_crop_right = 3.0, 3.0;

                    section_divider_height = 2.0, 2.0;
                    section_heading_size = 16.0, 16.0;
                    section_heading_offset = 50.0, 50.0;
                    section_heading_text_offset = 13.0, 13.0;
                    section_heading_font = "default", "default";
                    section_heading_color = Rgb(0.4510, 0.4510, 0.4510), Rgb(0.4510, 0.4510, 0.4510);
                    section_divider_color = Rgb(0.4510, 0.4510, 0.4510), Rgb(0.4510, 0.4510, 0.4510);

                    menu_height = 35.0, 35.0;
                    menu_divider_color = Rgb(0.7804, 0.7804, 0.7804), Rgb(0.7804, 0.7804, 0.7804);
                    menu_divider_height = 0.5,0.5;

                    navigator_width = 37.0, 37.0;
                    navigator_icon_strong_color = Rgba(0.569,0.584,0.612,1.0), Rgba(0.569,0.584,0.612,1.0);
                    navigator_icon_weak_color = Rgba(0.569,0.584,0.612,1.0), Rgba(0.569,0.584,0.612,1.0);
                    navigator_top_padding = -3.0, -3.0;
                    navigator_bottom_padding = 7.0, 7.0;
                    navigator_list_view_width = 39.0, 39.0;
                    navigator_list_view {
                        background = Rgba::transparent() , Rgba::transparent();
                        highlight  = Rgb(0.96,0.85,0.725) , Rgb(0.96,0.85,0.725); // rgb(245,217,185)
                        highlight {
                            height = 29.0, 29.0;
                            corner_radius = 10.0, 10.0;
                        }
                        entry {
                            padding = 14.5, 14.5;
                        }
                        padding = 5.0, 5.0;
                    }
                    breadcrumbs {
                        height = 28.0, 28.0;
                        separator {
                            width = 8.0, 8.0;
                            height = 6.0, 6.0;
                        }
                        ellipsis {
                            background_width = 24.0, 24.0;
                            background_height = 10.0, 10.0;
                            background_corners_radius = 16.0, 16.0;
                            background_color = Rgb(0.89, 0.89, 0.9), Rgb(0.89, 0.89, 0.9);
                            circles_color = Rgb(0.74, 0.74, 0.75), Rgb(0.74, 0.74, 0.75);
                            circles_radius = 2.0, 2.0;
                            circles_gap = 2.0, 2.0;
                        }
                        entry {
                            margin = 1.0, 1.0;
                            hover_color = Rgba(0.0, 0.0, 0.0, 0.0), Rgba(0.0, 0.0, 0.0, 0.0);
                            font = "default", "default";
                            text_padding_left = 7.0, 7.0;
                            text_size = 12.0, 12.0;
                            selected_color = Rgba(0.5, 0.5, 0.51, 1.0), Rgba(0.5, 0.5, 0.51, 1.0);
                            highlight_corners_radius = 15.0, 15.0;
                            greyed_out_color = Rgba(0.79, 0.79, 0.8, 1.0), Rgba(0.79, 0.79, 0.8, 1.0);
                        }
                    }

                    favourites_section_base_color = Rgba::new(0.0, 0.42, 0.64, 1.0),Rgba::new(0.0, 0.42, 0.64, 1.0);

                    section {
                        column_grid {
                            column_gap = 3.0, 3.0;
                            entry_color_0 = Rgba(43.0 / 255.0, 117.0 / 255.0, 239.0 / 255.0, 1.0),Rgba(43.0 / 255.0, 117.0 / 255.0, 239.0 / 255.0, 1.0);
                            entry_color_1 = Rgba(62.0 / 255.0, 139.0 / 255.0, 41.0 / 255.0, 1.0),Rgba(62.0 / 255.0, 139.0 / 255.0, 41.0 / 255.0, 1.0);
                            entry_color_2 = Rgba(192.0 / 255.0, 71.0 / 255.0, 171.0 / 255.0, 1.0),Rgba(192.0 / 255.0, 71.0 / 255.0, 171.0 / 255.0, 1.0);
                            entry_color_3 = Rgba(121.0 / 255.0, 126.0 / 255.0, 37.0 / 255.0, 1.0),Rgba(121.0 / 255.0, 126.0 / 255.0, 37.0 / 255.0, 1.0);
                            entry_color_4 = Rgba(181.0 / 255.0, 97.0 / 255.0, 35.0 / 255.0, 1.0),Rgba(181.0 / 255.0, 97.0 / 255.0, 35.0 / 255.0, 1.0);
                            entry_color_5 = Rgba(61.0 / 255.0, 146.0 / 255.0, 206.0 / 255.0, 1.0),Rgba(61.0 / 255.0, 146.0 / 255.0, 206.0 / 255.0, 1.0);
                        }
                    }
                }
            }
            component_group {
                header {
                    text {
                        font = "default", "default";
                        size = 12.0, 12.0;
                        color_intensity = 1.0, 1.0;
                    }
                    height = 27.0, 27.0;
                    padding {
                        left   = 11.0, 11.0;
                        right  = 2.5, 2.5;
                        bottom = 5.0, 5.0;
                    }
                    shadow = shadow , shadow;
                    shadow {
                        size     = 27.0 , 27.0;
                        spread   = shadow::spread   , shadow::spread;
                        fading   = shadow::fading   , shadow::fading;
                        exponent = shadow::exponent , shadow::exponent;
                        offset_x = shadow::offset_x , shadow::offset_x;
                        offset_y = shadow::offset_y , shadow::offset_y;
                    }
                }
                selection {
                    corners_radius = 10.0, 10.0;
                    header_corners_radius = 5.0, 5.0;
                    horizontal_padding = 3.0, 3.0;
                    height = 30.0, 30.0;
                    wide_group_column_padding = 18.0, 18.0;
                    header_height = 21.0, 21.0;
                }
                background_color_intensity = 0.2, 0.2;
                selection_color_intensity = 1.0, 1.0;
                dimmed_color_intensity = 0.5, 0.5;
                entry_list {
                    background = Rgba::new(1.0, 0.0, 0.0, 0.5), Rgba::new(1.0, 0.0, 0.0, 0.5);
                    highlight = Rgba::new(1.0, 0.0, 0.0, 0.5), Rgba::new(1.0, 0.0, 0.0, 0.5);
                    selected_color = Rgba::white(), Rgba::white();
                    text {
                        font = "default", "default";
                        size = 12.0, 12.0;
                        color = Rgba(0.4,0.4,0.4,1.0), Rgba(0.4,0.4,0.4,1.0);
                        highlight_bold = 0.02, 0.02;
                    }
                    icon_text_gap = 5.0, 5.0;
                    icon {
                        size = 16.0, 16.0;
                        weak_color_intensity = 0.5, 0.5;
                    }
                    padding = 4.0, 4.0;
                    entry {
                        padding = 7.0, 7.0;
                    }
                    highlight {
                        height = 30.0, 30.0;
                        corner_radius = 0.0, 0.0;
                    }
                }
            }
        }
        searcher {
            action_list_gap = 10.0, 10.0;
            padding         = 5.0, 5.0;
            icons {
                favorites = Rgba(0.98,0.584,0.122,1.0)  , Rgba(0.98,0.584,0.122,1.0);
                computer_vision {
                    highlight = Rgba(0.872,0.267,0.255,1.0) , Rgba(0.872,0.267,0.255,1.0);
                }
                system {
                    background = Rgba(0.306,0.306,0.306,1.0) , Rgba(0.306,0.306,0.306,1.0);
                    content    = Rgba(0.988,0.996,1.0,1.0)   , Rgba(0.988,0.996,1.0,1.0);
                }
                libraries {
                    _0 = Rgba(0.541,0.545,0.545,1.0)  , Rgba(0.541,0.545,0.545,1.0);
                    _1 = Rgba(0.675,0.675,0.675,1.0) , Rgba(0.675,0.675,0.675,1.0);
                    _2 = Rgba(0.713,0.713,0.713,1.0) , Rgba(0.713,0.713,0.713,1.0);
                    _3 = Rgba(0.8,0.8,0.8,1.0) , Rgba(0.8,0.8,0.8,1.0);
                }
            }
        }
        file_browser {
            width  = 0.0,   0.0; // Should be updated when file browser will be implemented.
            height = 421.0, 421.0;
        }
        open_dialog {
            // Should be updated when file browser will be implemented.
            gap_between_panels = 0.0, 0.0;
        }
        project_list {
            width      = 202.0 , 202.0;
            padding    = 16.0, 16.0;
            height     = 421.0, 421.0;
            background = Rgba(0.992,0.996,1.0,1.0), Rgba(0.182,0.188,0.196,1.0);
            text       = widget::list_view::text, widget::list_view::text;
            text {
                size    = 12.0, 12.0;
                padding = 6.0 , 6.0 ;
            }
            bar {
                height      = 45.0, 45.0;
                border_size = 1.0, 1.0;
                border_color = Rgba(0.808,0.808,0.808,1.0)    , Rgba(0.808,0.808,0.808,1.0);
                label {
                    size  = 12.0, 12.0;
                    color = Rgba(0.439,0.439,0.439,1.0), Rgba(0.439,0.439,0.439,1.0);
                }
            }
        }

        window_control_buttons {
            radius  = 6.5, 6.5;
            spacing = application::window_control_buttons::radius, application::window_control_buttons::radius;
            padding {
                left   = 13.0, 13.0;
                top    = 13.0, 13.0;
                right  = 13.0, 13.0;
                bottom = 13.0, 13.0;
            }

            close {
                normal {
                    background_color = Rgb::new(1.0, 0.33, 0.33),  Rgb::new(1.0, 0.33, 0.33);
                    icon_color       = Rgba::new(0.0,0.0,0.0,0.0), Rgba::new(0.0,0.0,0.0,0.0);
                }
                hovered {
                    background_color = application::window_control_buttons::close::normal::background_color, application::window_control_buttons::close::normal::background_color;
                    icon_color       = Rgba::new(0.385,0.0,0.0,1.0), Rgba::new(0.385,0.0,0.0,1.0);
                }
                pressed {
                    background_color = Rgb::new(1.0, 0.5, 0.5),     Rgb::new(1.0, 0.5, 0.5);
                    icon_color       = Rgb::new(0.549,0.098,0.063), Rgb::new(0.549,0.098,0.063);
                }
            }

            fullscreen {
                normal {
                    background_color = Rgb::new(0.18, 0.75, 0.25), Rgb::new(0.18, 0.75, 0.25);
                    icon_color       = Rgba::new(0.0,0.,0.0,0.0),  Rgba::new(0.0,0.0,0.0,0.0);
                }
                hovered {
                    background_color = application::window_control_buttons::fullscreen::normal::background_color, application::window_control_buttons::fullscreen::normal::background_color;
                    icon_color       = Rgba::new(0.0,0.37,0.0,1.0), Rgba::new(0.0,0.37,0.0,1.0);
                }
                pressed {
                    background_color = Rgb::new(0.3, 0.96, 0.39), Rgb::new(0.3, 0.96, 0.39);
                    icon_color       = Rgb::new(0.0,0.38,0.0),    Rgb::new(0.0,0.38,0.0);
                }
            }
        }

        documentation {
            background = graph_editor::node::background, graph_editor::node::background;
        }
        status_bar {
            text = text, text;
            background = graph_editor::node::background , graph_editor::node::background;
            background {
                corner_radius = 14.0 , 14.0;
                shadow = shadow , shadow;
                shadow {
                    size     = shadow::size     , shadow::size;
                    spread   = shadow::spread   , shadow::spread;
                    fading   = shadow::fading   , shadow::fading;
                    exponent = shadow::exponent , shadow::exponent;
                    offset_x = shadow::offset_x , shadow::offset_x;
                    offset_y = shadow::offset_y , shadow::offset_y;
                }
            }
        }
    }
    code {
        syntax {
            base      = Lcha(0.09,0.0,0.0,1.0) , Lcha(1.0,0.0,0.0,0.7);
            disabled  = Lcha(0.7,0.0,0.0,1.0) , Lcha(1.0,0.0,0.0,0.2);
            expected  = Lcha(0.7,0.0,0.0,1.0) , Lcha(1.0,0.0,0.0,0.3);
            selection = Lcha(0.7,0.0,0.125,0.7) , Lcha(0.7,0.0,0.125,0.7);
            profiling {
                base      = Lcha(1.0,0.0,0.0,0.9) , Lcha(0.0,0.0,0.0,0.7);
                disabled  = Lcha(1.0,0.0,0.0,0.5) , Lcha(0.0,0.0,0.0,0.2);
                expected  = Lcha(1.0,0.0,0.0,0.5) , Lcha(0.0,0.0,0.0,0.3);
                selection = Lcha(1.0,0.0,0.0,1.0) , Lcha(0.0,0.0,0.0,1.0);
            }
        }
        types {
            hue_steps     = 512.0 , 512.0;
            hue_shift     = 0.0, 0.0;
            lightness     = 0.72 , 0.7;
            chroma        = 0.7 , 0.4;
            any           = code::syntax::base , code::syntax::base;
            any.selection = Lcha(0.8,0.0,0.0,1.0) , Lcha(0.5,0.0,0.0,1.0);
            selected      = graph_editor::node::background , graph_editor::node::background;
            overriden {
                Builtins {
                    Main {
                        Unresolved_Symbol {
                            hue       = 0.68, 0.0;
                            lightness = 0.09, 0.7;
                            chroma    = 0.0, 0.4;
                        }
                        Integer.hue = 0.68 , 0.68;
                        Number.hue = 0.68 , 0.68;
                        Text.hue = 0.22 , 0.217;
                    }
                }
            }
        }
    }
    graph_editor {
        default_x_gap_between_nodes     = 48.0  , 48.0;
        default_y_gap_between_nodes     = 32.0  , 32.0;
        minimal_x_spacing_for_new_nodes = 150.0 , 150.0;
        // Area around every existing node where attempts to place a new node may trigger a node
        // alignment mechanism.
        //
        // The specific conditions when the alignment mechanism is triggered, as well as the
        // algorithm used to perform the alignment, are governed by the Graph Editor.
        alignment_area_around_node {
            above_node           = 15.0  , 15.0;
            below_node           = 50.0 , 50.0;
            to_the_left_of_node  = 25.0  , 25.0;
            to_the_right_of_node = 25.0  , 25.0;
        }
        screen_margin_when_panning_camera_to_node {
            top = 40.0, 40.0;
            bottom = 80.0, 80.0;
            left = 80.0, 80.0;
            right = 300.0, 300.0;
        }
        node {
            // Original RGB values (for reference after fixing color-conversion issues)
            // light: rgb(253,254,255), old-dark: Lcha(0.2,0.014,0.18,1.0), dark: rgb(47,48,50)
            background         = Rgba(0.992,0.996,1.0,1.0), Rgba(0.182,0.188,0.196,1.0);
            background.skipped = graph_editor::node::background , graph_editor::node::background;
            selection          = selection, selection;
            selection {
                size = 3.0 , 3.0;
                offset = 5.0 , 5.0;
            }
            text           = Rgba(0.078,0.067,0.137,0.85) , Lcha(1.0,0.0,0.0,0.7);
            text {
                missing_arg    = Rgba(0.078,0.067,0.137,0.25) , Lcha(1.0,0.0,0.0,0.3);
                variant.dimmed = Lcha(0.7,0.0,0.0,0.7) , Lcha(0.25,0.014,0.18,1.0);
                selection      = Lcha(0.7,0.0,0.125,0.7) , Lcha(0.7,0.0,0.125,0.7);
            }
            actions {
                button {
                    non_toggled = Lcha(0.0,0.0,0.0,0.3)  , Lcha(0.4,0.0,0.0,1.0);
                    toggled     = Lcha(0.0,0.0,0.0,0.7)  , Lcha(1.0,0.0,0.0,0.7);
                    hovered     = Lcha(0.0,0.0,0.0,0.45) , Lcha(1.0,0.0,0.0,0.7);
                }
            }
            vcs {
                unchanged = Lcha::transparent(), Lcha::transparent();
                added     = Lcha::green(0.8,1.0), Lcha::green(0.8,1.0);
                edited    = Lcha::yellow(0.9,1.0), Lcha::yellow(0.9,1.0);
            }
            error {
                dataflow     = Rgba(1.0,0.655,0.141,1.0), Rgba(1.0,0.655,0.141,1.0);
                panic        = Rgba(1.0,0.341,0.125,1.0), Rgba(1.0,0.341,0.125,1.0);
                width        = 4.0  , 4.0;
                repeat_x     = 20.0 , 20.0;
                repeat_y     = 20.0 , 20.0;
                stripe_width = 10.0 , 10.0;
                stripe_angle = 45.0 , 45.0;
            }
            profiling {
                lightness    = code::types::lightness , code::types::lightness;
                chroma       = code::types::chroma    , code::types::chroma;
                min_time_hue = 0.38                   , 0.38;
                max_time_hue = 0.07                   , 0.07;
            }
            type_label {
                offset_y = -23.0, -23.0;
            }
        }
        visualization {
            background = graph_editor::node::background , graph_editor::node::background;
            text           = Lcha(0.0,0.0,0.0,0.7)   , Lcha(1.0,0.0,0.0,0.7);
            text.selection = Lcha(0.7,0.0,0.125,0.7) , Lcha(0.7,0.0,0.125,0.7);
            error {
                dataflow.text = Rgba(1.0,0.655,0.141,1.0), Rgba(1.0,0.655,0.141,1.0);
                panic.text    = Rgba(1.0,0.341,0.125,1.0), Rgba(1.0,0.341,0.125,1.0);
            }
            action_bar {
                // Original RGB values (for reference after fixing color-conversion issues)
                // rgb(237 240 243)
                background = Rgba(0.929,0.941,0.953,1.0) , Lcha(1.0,0.0,0.0,0.1);
                icon       = Lcha(0.0,0.0,0.0,0.7) , Lcha(1.0,0.0,0.0,0.7);
                text       = Lcha(0.0,0.0,0.0,0.7) , Lcha(1.0,0.0,0.0,0.7);
            }
            // Original RGB values (for reference after fixing color-conversion issues)
            // ... , rgb(35 41 47)
            selection = Rgba(0.306,0.647,0.992,0.14) , Rgba(0.137,0.16,0.184,1.0);
            selection {
                size = 8.0 , 8.0;
                offset = 0.0 , 0.0;
            }
        }
        breadcrumbs {
            full        = Lcha(0.0,0.0,0.0,0.7) , Lcha(1.0,0.0,0.0,0.7);
            transparent = Lcha(0.0,0.0,0.0,0.4) , Lcha(1.0,0.0,0.0,0.4);
            selected    = Lcha(0.0,0.0,0.0,0.7) , Lcha(1.0,0.0,0.0,0.7);
            hover       = Lcha(0.0,0.0,0.0,0.7) , Lcha(1.0,0.0,0.0,0.7);
            deselected  {
                left  = Lcha(0.0,0.0,0.0,0.5) , Lcha(1.0,0.0,0.0,0.5);
                right = Lcha(0.0,0.0,0.0,0.2) , Lcha(1.0,0.0,0.0,0.2);
            }
            background = application::background , application::background;
            background {
                corner_radius = 8.0 , 8.0;
                shadow = shadow , shadow;
                shadow {
                    size     = shadow::size     , shadow::size;
                    spread   = shadow::spread   , shadow::spread;
                    fading   = shadow::fading   , shadow::fading;
                    exponent = shadow::exponent , shadow::exponent;
                    offset_x = shadow::offset_x , shadow::offset_x;
                    offset_y = shadow::offset_y , shadow::offset_y;
                }
            }
        }
        edge {
            split {
                lightness_factor = 1.2 , 0.2;
                chroma_factor    = 0.8 , 1.0;
            }
        }
        profiling_button {
            non_toggled     = graph_editor::node::actions::button::non_toggled
                ,graph_editor::node::actions::button::non_toggled;
            toggled         = Lcha(0.7,0.5,0.12,1.0) , Lcha(0.7,0.5,0.12,1.0);
            hovered         = graph_editor::node::actions::button::hovered
                ,graph_editor::node::actions::button::hovered;
            toggled_hovered = Lcha(0.55,0.5,0.12,1.0) , Lcha(0.85,0.5,0.12,1.0);
        }
        prompt {
            background = application::background, application::background;
            background {
                corner_radius = 14.0, 14.0;
                padding       = 14.0, 14.0;
            }
            text       = Rgba(0.078,0.067,0.137,0.85) , Lcha(1.0,0.0,0.0,0.7);
            text {
                size = 20.0, 20.0;
            }
        }
        add_node_button {
            margin = 14.0, 14.0;
            size = 60.0, 60.0;
            background = Rgba(1.0, 1.0, 1.0, 1.0), Rgba(0.0, 0.0, 0.0, 1.0);
            color = Rgba(0.0, 0.451, 0.859, 1.0), Rgba(0.0, 0.451, 0.859, 1.0);

            hover {
                background = Rgba(0.9, 0.9, 1.0, 1.0), Rgba(0.9, 0.9, 1.0, 1.0);
                color = Rgba(0.0, 0.451, 0.859, 1.0), Rgba(0.0, 0.451, 0.859, 1.0);
            }
            click {
                background = Rgba(0.62, 0.62, 1.0, 1.0), Rgba(0.62, 0.62, 1.0, 1.0);
                color = Rgba(0.0, 0.451, 0.859, 1.0), Rgba(0.0, 0.451, 0.859, 1.0);
            }
        }
    }
    widget {
        list_view {
            background = graph_editor::node::background , graph_editor::node::background;
            highlight  = Rgba(0.906,0.914,0.922,1.0) , Lcha(1.0,0.0,0.0,0.15); // rgb(231,233,235)
            text = Lcha(0.0,0.0,0.0,0.7) , Lcha(1.0,0.0,0.0,0.7);
            text {
                selection = Lcha(0.7,0.0,0.125,0.7) , Lcha(0.7,0.0,0.125,0.7);
                font      = "default-mono", "default-mono";
                size      = 12.0, 12.0;
                highlight_bold = 0.02, 0.02;
            }
            entry {
                padding = 10.0, 10.0;
            }
            highlight {
                height = 24.0, 24.0;
                corner_radius = 12.0, 12.0;
            }
            padding = 5.0, 5.0;
        }
    }
    colors {
        dimming {
            lightness_factor = 1.1 , 1.1;
            chroma_factor    = 0.2 , 0.2;
        }
    }
    component {
        label {
            background = graph_editor::node::background , graph_editor::node::background;
            text       = Lcha(0.0,0.0,0.0,0.7) , Lcha(1.0,0.0,0.0,0.7);
            text {
                offset = 00.0, 00.0;
                size   = 12.0, 12.0;
                font   = "default", "default";
            }
            padding_outer   = 20.0, 20.0;
            padding_inner_x = 10.0, 10.0;
            padding_inner_y = 2.0, 2.0;
            height          = 30.0, 30.0;
        }
        slider {
            background = graph_editor::node::background , graph_editor::node::background;
            handle {
                color = Lcha(0.3,0.0,0.0,1.0), Lcha(0.7,0.0,0.0,1.0);
            }
            track {
                color       = Lcha(0.75,0.0,0.0,1.0), Lcha(0.3,0.0,0.0,1.0);
                hover_color = Lcha(0.75,0.0,0.0,1.0), Lcha(0.4,0.0,0.0,1.0);
            }
            background {
                color       = Lcha(1.0,0.0,0.0,0.5), Lcha(0.3,0.0,0.0,0.5);
                hover_color = Lcha(1.0,0.0,0.0,0.5), Lcha(0.4,0.0,0.0,0.5);
            }
            overflow {
                color = Lcha(0.0,0.0,0.0,1.0), Lcha(1.0,0.0,0.0,1.0);
                scale = 1.0, 1.0;
            }
        }
    }


    // === Generics ===

    accent = Rgba(0.306,0.647,0.992,1.0) , Lcha(0.72,0.54,0.22,1.0); // rgb(78,165,253)
    selection = Rgba(0.306,0.647,0.992,1.0) , Rgba(0.204,0.337,0.486,1.0); // rgb(78,165,253), rgb(52 86 124)
    shadow = Rgba(0.09,0.055,0.125,0.09) , Lcha(0.0,0.0,0.0,0.20); // rgba(23,14,32,0.09)
    shadow {
        size     = 25.0 , 25.0;
        spread   = -5.0 , -5.0;
        fading   = Rgba(0.09,0.055,0.125,0.0) , Lcha(0.0,0.0,0.0,0.0);
        exponent = 3.0 , 3.0;
        offset_x = 0.0 , 0.0;
        offset_y = -5.0 , -5.0;
        html {
            alpha  = 0.10  , 0.30;
            blur   = 10.0 , 10.0;
            spread = -2.0 , -2.0;
        }
    }
    text = Rgba(0.078,0.067,0.137,0.85) , Lcha(1.0,0.0,0.0,0.7);
    text {
        size   = 12.0, 12.0;
    }
}
