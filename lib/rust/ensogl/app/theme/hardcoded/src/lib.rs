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

use enso_prelude::*;
use ensogl_core::prelude::*;

use enso_shapely::before_main;
use ensogl_text::font::DEFAULT_FONT;
use ensogl_text::font::DEFAULT_FONT_MONO;



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
    ($id:tt $theme:ident [$($path:ident)*] $(#[$meta:meta])* $var:ident = $($e:expr),* $(;$($rest:tt)*)?) => {
        $theme.set(stringify!($($path.)*$var), _select_theme_expr!{$id $($e),*});
        _define_theme_literals!{$id $theme [$($path)*] $($($rest)*)?}
    };
    ($id:tt $theme:ident [$($path:ident)*] $(#[$meta:meta])* $path_segment:ident {$($t:tt)*} $($rest:tt)*) => {
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
    ([$($path:ident)*] $(#[$meta:meta])* $var:ident = $($e:expr),* $(;$($rest:tt)*)?) => {
        $(#[$meta])*
        pub const $var : StaticPath = StaticPath::new(stringify!($($path.)*$var));
        _define_theme_modules!{[$($path)*] $($($rest)*)?}
    };
    ([$($path:ident)*] $(#[$meta:meta])* $path_segment:ident {$($t:tt)*} $($rest:tt)*) => {
        $(#[$meta])*
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
    // when only one expression is specified, use it for all numbers.
    ($_id:tt $e0:expr) => {
        $e0
    };
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
            use ensogl_core::data::color::Lch;
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
        background = Rgb::from_base_255(210.0, 206.0, 203.0) , Rgba(0.125,0.133,0.141,1.0);
        tooltip {
            show_delay_duration_ms = 500.0, 500.0;
            hide_delay_duration_ms = 0.0, 0.0;
        }

        top_bar {
            padding_left = 19.0, 19.0;
            padding_top = 9.0, 9.0;
            gap = 16.0, 16.0;
            background {
                color = Rgba(1.0, 1.0, 1.0, 0.44), Rgba(0.0, 0.0, 0.0, 0.44);
                corner_radius = 24.0, 24.0;
            }
            project_name_with_environment_selector {
                background {
                    height = 32.0, 32.0;
                    padding_left = 10.0, 10.0;
                    padding_right = 4.0, 4.0;
                }
                gap = 8.0, 8.0;
                project_name {
                    // Rgb(118, 118, 118)
                    color = Lcha(0.49, 0.0, 0.75, 1.0), Lcha(0.49, 0.0, 0.75, 1.0);
                    color_unsaved = Lcha(0.49, 0.0, 0.75, 0.6), Lcha(0.49, 0.0, 0.75, 0.6);
                    text_size = 11.5, 11.5;
                }
            }
            breadcrumbs {
                full        = Lcha(0.0,0.0,0.0,0.7) , Lcha(1.0,0.0,0.0,0.7);
                transparent = Lcha(0.0,0.0,0.0,0.4) , Lcha(1.0,0.0,0.0,0.4);
                selected    = Lcha(0.0,0.0,0.0,0.7) , Lcha(1.0,0.0,0.0,0.7);
                hover       = Lcha(0.0,0.0,0.0,0.6) , Lcha(1.0,0.0,0.0,0.6);
                deselected  {
                    left  = Lcha(0.0,0.0,0.0,0.4) , Lcha(1.0,0.0,0.0,0.4);
                    right = Lcha(0.0,0.0,0.0,0.2) , Lcha(1.0,0.0,0.0,0.2);
                }
                unsaved {
                    selected    = Lcha(0.0,0.0,0.0,1.0) , Lcha(1.0,0.0,0.0,1.0);
                    hover       = Lcha(0.0,0.0,0.0,1.0) , Lcha(1.0,0.0,0.0,1.0);
                    deselected  {
                        left  = Lcha(0.0,0.0,0.0,0.8) , Lcha(1.0,0.0,0.0,0.8);
                        right = Lcha(0.0,0.0,0.0,0.6) , Lcha(1.0,0.0,0.0,0.6);
                    }
                }
            }
        }
        component_browser {
            panels_gap = 4.0, 4.0;
            documentation {
                width = 406.0, 406.0;
                height = 380.0, 380.0;
                background = application::component_browser::component_list_panel::background_color, application::component_browser::component_list_panel::background_color;
                corner_radius = 20.0, 20.0;
                breadcrumbs {
                    padding_y = 8.0, 8.0;
                    padding_x = 10.0, 10.0;
                    height = 40.0, 40.0;
                }
            }
            component_list_panel {
                width = 190.0, 190.0;
                height = 380.0, 380.0;
                background_color = Rgb::from_base_255(234.0, 234.0, 234.0),Rgb::from_base_255(236.0, 240.0, 242.0);
                corners_radius = 20.0, 20.0;
                padding_bottom = 4.0, 4.0;
                button_panel {
                    margin_bottom = -22.0, -22.0;
                    width = 190.0, 190.0;
                    height = 40.0, 40.0;
                    corner_radius = 20.0, 20.0;
                    inner_border_distance = 4.0, 4.0;
                    border_width = 0.5, 0.5;
                    background_color = application::component_browser::component_list_panel::background_color, application::component_browser::component_list_panel::background_color;
                    border_color = Rgba(0.76, 0.76, 0.76, 1.0), Rgba(0.76, 0.76, 0.76, 1.0);
                    padding = 12.0, 12.0;
                    gap = 12.0, 12.0;
                    right_side_margin = 38.0, 38.0;
                }
                grid {
                    width = 190.0, 190.0;
                    height = 358.0, 358.0;
                    padding_x = 4.0, 4.0;
                    padding_y = 0.0, 0.0;
                    column_gap = 3.0, 3.0;
                    entry_height = 32.0, 32.0;

                    // The `color` values support color types (like `color::Rgba`)
                    // and floating-point numbers. This is possible due to a custom stylesheet
                    // accessor defined for
                    // [`ide_view_component_list_panel_grid::entry::style::Color`] type.
                    // Floating-point numbers mean the alpha multiplier for the "main" color of the
                    // component group.
                    entry {
                        background.intensity = 0.0, 0.0;
                        dimmed = Rgb::from_base_255(160.0, 163.0, 165.0), Rgb::from_base_255(160.0, 163.0, 165.0);
                        padding = 8.0, 8.0;
                        text {
                            font = DEFAULT_FONT, DEFAULT_FONT;
                            y_offset = 8.0, 8.0;
                            y_offset_header = 5.0, 5.0;
                            x_offset_header = 0.0, 0.0;
                            size = 11.5, 11.5;
                            color = Rgba(0.0, 0.0, 0.0, 0.6), Rgba(0.0, 0.0, 0.0, 0.6);
                            highlight_bold = 0.01, 0.01;
                        }
                        icon {
                            size = 16.0, 16.0;
                            text_padding = 9.0, 9.0;
                            color = 1.0, 1.0;
                            dull_color_alpha = 0.25, 0.25;
                        }
                        special_icons {
                            join.intersection_alpha = 0.7, 0.7;
                            libraries {
                                dull_alpha = 1.0, 1.0;
                                secondary_alpha = 1.0, 1.0;
                                tertiary_alpha = 1.0, 1.0;
                            }
                            marketplace {
                                dull_alpha = 1.0, 1.0;
                                secondary_alpha = 1.0, 1.0;
                                tertiary_alpha = 1.0, 1.0;
                            }
                            computer_vision {
                                highlight = Rgba(0.872,0.267,0.255,1.0) , Rgba(0.872,0.267,0.255,1.0);
                            }
                            system {
                                background = Rgba(0.306,0.306,0.306,1.0) , Rgba(0.306,0.306,0.306,1.0);
                                content    = Rgba(0.988,0.996,1.0,1.0)   , Rgba(0.988,0.996,1.0,1.0);
                            }
                        }
                        highlight {
                            corners_radius = 16.0, 16.0;
                            hover.color = 0.4, 0.4;
                            selection {
                                background.intensity = 0.75, 0.75;
                                text.color = Rgba::white(), Rgba::white();
                                icon.color = Lcha(1.0, 0.0, 0.0, 1.0), Lcha(1.0, 0.0, 0.0, 1.0);
                            }
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

                    group_colors {
                        // Green
                        group_0 = Rgb::from_base_255(77.0, 154.0, 41.0), Rgb::from_base_255(77.0, 154.0, 41.0);
                        // Yellow
                        group_1 = Rgb::from_base_255(178.0, 121.0, 35.0), Rgb::from_base_255(178.0, 121.0, 35.0);
                        // Purple
                        group_2 = Rgb::from_base_255(151.0, 53.0, 185.0), Rgb::from_base_255(151.0, 53.0, 185.0);
                        // Pink
                        group_3 = Rgb::from_base_255(193.0, 71.0, 171.0), Rgb::from_base_255(193.0, 71.0, 171.0);
                        // Blue
                        group_4 = Rgb::from_base_255(42.0, 103.0, 223.0), Rgb::from_base_255(42.0, 103.0, 223.0);
                        // Orange
                        group_5 = Rgb::from_base_255(181.0, 97.0, 35.0), Rgb::from_base_255(181.0, 97.0, 35.0);
                        local_scope_group = Rgba::new(0.0, 0.42, 0.64, 1.0),Rgba::new(0.0, 0.42, 0.64, 1.0);
                    }
                }
                menu {
                    breadcrumbs {
                        crop_left = 8.0, 8.0;
                        crop_right = 3.0, 3.0;
                        height = 44.0, 44.0;
                        background_padding_x = 15.0, 15.0;
                        background_color = Rgb(0.463, 0.69, 0.376), Rgb(0.463, 0.69, 0.376);
                        background_height = 28.0, 28.0;
                        background_y_offset = 2.0, 2.0;
                        separator {
                            width = 8.0, 8.0;
                            height = 6.0, 6.0;
                            color = Rgba(1.0, 1.0, 1.0, 1.0), Rgba(1.0, 1.0, 1.0, 1.0);
                            offset_x = 1.0, 1.0;
                            offset_y = -2.0, -2.0;
                        }
                        ellipsis {
                            background_width = 28.0, 28.0;
                            background_height = 16.0, 16.0;
                            background_corners_radius = 100.0, 100.0;
                            background_color = Rgba(1.0, 1.0, 1.0, 0.16), Rgba(1.0, 1.0, 1.0, 0.16);
                            circles_color = Rgba(1.0, 1.0, 1.0, 1.0), Rgba(1.0, 1.0, 1.0, 1.0);
                            circles_radius = 2.0, 2.0;
                            circles_gap = 2.0, 2.0;
                            offset_x = 0.0, 0.0;
                            offset_y = -2.0, -2.0;
                        }
                        entry {
                            margin = 1.0, 1.0;
                            hover_color = Rgba(0.0, 0.0, 0.0, 0.0), Rgba(0.0, 0.0, 0.0, 0.0);
                            font = DEFAULT_FONT, DEFAULT_FONT;
                            text_y_offset = 6.0, 6.0;
                            text_padding_left = 0.0, 0.0;
                            text_size = 11.5, 11.5;
                            icon_x_offset = 2.0, 2.0;
                            icon_y_offset = 6.0, 6.0;
                            selected_color = Rgba(1.0, 1.0, 1.0, 1.0), Rgba(1.0, 1.0, 1.0, 1.0);
                            highlight_corners_radius = 15.0, 15.0;
                            greyed_out_color = Rgba(1.0, 1.0, 1.0, 0.15), Rgba(1.0, 1.0, 1.0, 0.15);
                        }
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
        project_list {
            width      = 202.0 , 202.0;
            height     = 428.0, 428.0;
            background = Rgba(0.992,0.996,1.0,1.0), Rgba(0.182,0.188,0.196,1.0);
            shadow_extent = 10.0, 10.0;
            corners_radius = 16.0, 16.0;
            paddings = 4.0, 4.0;
            bar {
                height = 45.0, 45.0;
                border_size = 1.0, 1.0;
                border_color = Rgba(0.808,0.808,0.808,1.0)    , Rgba(0.808,0.808,0.808,1.0);
                label {
                    padding = 16.0, 16.0;
                    size = 12.0, 12.0;
                    color = Rgba(0.439,0.439,0.439,1.0), Rgba(0.439,0.439,0.439,1.0);
                }
            }
            entry {
                height = 25.0, 25.0;
                corners_radius = application::project_list::corners_radius, application::project_list::corners_radius;
                selection_color = Rgba::transparent(), Rgba::transparent();
                hover_color = Rgba(0.906,0.914,0.922,1.0), Rgba(0.906,0.914,0.922,1.0);
                text {
                    padding_left = 10.0, 10.0;
                    padding_bottom = 7.0, 7.0;
                    size = 12.0, 12.0;
                    color = Rgba(0.439,0.439,0.439,1.0), Rgba(0.439,0.439,0.439,1.0);
                }
            }
        }

        window_control_buttons {
            radius  = 6.5, 6.5;
            spacing = application::window_control_buttons::radius, application::window_control_buttons::radius;

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
    }
    code {
        types {
            hue_steps     = 512.0 , 512.0;
            hue_shift     = 0.0, 0.0;
            lightness     = 0.72 , 0.7;
            chroma        = 0.7 , 0.4;
            any           = Lcha(0.09,0.0,0.0,1.0) , Lcha(1.0,0.0,0.0,0.7);
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
            background         = Rgba(0.992,0.996,1.0,1.0), Rgba(0.182,0.188,0.196,1.0);
            port_color_tint    = Rgba(1.0,1.0,1.0,0.15), Rgba(1.0,1.0,1.0,0.15);
            text               = Lcha(0.09,0.0,0.0,1.0), Lcha(1.0,0.0,0.0,0.7);
            corner_radius = 14.0, 14.0;
            selection {
                size = 20.0 , 20.0;
                opacity = 0.2 , 0.2;
                hover_opacity = 0.1 , 0.1;
            }
            actions {
                context_switch {
                    toggled     = Lcha(0.58, 0.67, 0.0825, 1.0), Lcha(0.58, 0.67, 0.0825, 1.0);
                }
                dull_alpha = 0.25, 0.25;
            }
            vcs {
                unchanged = Lcha::transparent(), Lcha::transparent();
                added     = Lcha::green(0.8,1.0), Lcha::green(0.8,1.0);
                edited    = Lcha::yellow(0.9,1.0), Lcha::yellow(0.9,1.0);
            }
            error {
                dataflow     = Lcha(0.566,0.564,0.082,1.0), Lcha(0.566,0.564,0.082,1.0);
                panic        = Rgba(0.7,0.235,0.08,1.0), Rgba(0.7,0.235,0.08,1.0);
                warning      = Rgba(1.0,0.655,0.141,1.0), Rgba(1.0,0.655,0.141,1.0);
                width        = 4.0  , 4.0;
                stripe_width = 10.0 , 10.0;
                stripe_gap   = 20.0 , 20.0;
                stripe_angle = 135.0 , 135.0;
            }
            pending {
                alpha_factor = 0.5;
            }
            type_label {
                offset_y = -23.0, -23.0;
            }

            temp_colors {
                color_0 = Lch(0.491, 0.339, 0.727), Lch(0.491, 0.339, 0.727);
                color_1 = Lch(0.447, 0.379, 0.968), Lch(0.447, 0.379, 0.968);
                color_2 = Lch(0.444, 0.124, 0.701), Lch(0.444, 0.124, 0.701);
            }
        }
        visualization {
            background = graph_editor::node::background, graph_editor::node::background;
            corner_radius = graph_editor::node::corner_radius, graph_editor::node::corner_radius;
            text           = Lcha(0.0,0.0,0.0,0.7)   , Lcha(1.0,0.0,0.0,0.7);
            text.selection = Lcha(0.7,0.0,0.125,0.7) , Lcha(0.7,0.0,0.125,0.7);
            error {
                dataflow.text = Rgba(1.0,0.341,0.125,1.0), Rgba(1.0,0.341,0.125,1.0);
                panic.text = Rgba(0.7,0.235,0.08,1.0), Rgba(0.7,0.235,0.08,1.0);
                warning.text = Rgba(1.0,0.655,0.141,1.0), Rgba(1.0,0.655,0.141,1.0);
            }
            action_bar {
                // Original RGB values (for reference after fixing color-conversion issues)
                // rgb(237 240 243)
                background = Rgba(0.929,0.941,0.953,1.0) , Lcha(1.0,0.0,0.0,0.1);
                icon       = Lcha(0.0,0.0,0.0,0.7) , Lcha(1.0,0.0,0.0,0.7);
                text       = Lcha(0.0,0.0,0.0,0.7) , Lcha(1.0,0.0,0.0,0.7);
            }
            selection {
                color = Rgba(0.306,0.647,0.992,0.14) , Rgba(0.137,0.16,0.184,1.0);
                width = 7.0, 7.0;
            }
            resize_grip {
                offset_x = 10.0, 10.0;
                offset_y = -10.0, -10.0;
            }
            text_grid {
                font = "DejaVu Sans Mono" , "DejaVu Sans Mono";
                font_size = 12.0 , 12.0;

            }
        }
        edge {
            disabled_color = Lcha(0.95,0.0,0.0,1.0), Lcha(0.95,0.0,0.0,1.0);
            split {
                lightness_factor = 1.2 , 0.2;
                chroma_factor    = 0.8 , 1.0;
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
        execution_environment_selector {
            background = Rgb::from_base_255(100.0, 181.0, 38.0), Rgb::from_base_255(100.0, 181.0, 38.0);
            divider = Rgba::black_with_alpha(0.12), Rgba::black_with_alpha(0.12);
            divider_offset = 32.5, 32.5;
            divider_padding = 10.0, 10.0;
            dropdown_width = 75.0, 75.0;
            height = 24.0, 24.0;
            menu_offset = 20.0, 20.0;
            play_button {
                color = Rgba::white_with_alpha(0.75), Rgba::white_with_alpha(0.75);
                triangle_size = 10.0, 10.0;
                offset = 15.0, 15.0;
                padding_x = 5.0, 5.0;
                padding_y = 7.0, 7.0;
                spinner {
                    color = Lch(0.8, 0.0, 0.0), Lch(0.8, 0.0, 0.0);
                    speed = 0.003, 0.003; // Radians/ms
                }
            }
        }
    }
    /// Styles dedicated for each individual node widget kind. The name of the style group should
    /// match the widget module name in `node::input::widget`.
    widget {
        single_choice {
            triangle_base = Lcha(1.0,0.0,0.0,0.5);
            triangle_connected = Lcha(1.0,0.0,0.0,1.0);
            triangle_size = Vector2(8.0, 6.0);
            /// Additional space around the triangle shape that will detect mouse hover.
            triangle_offset = Vector2(0.0, -7.0);
            dropdown_offset = Vector2(0.0, -20.0);
            dropdown_max_size = Vector2(800.0, 600.0);
            dropdown_tint = Rgba(0.0,0.0,0.0,0.1);
        }
        list_view {
            background = graph_editor::node::background;
            highlight  = Rgba(0.906,0.914,0.922,1.0), Lcha(1.0,0.0,0.0,0.15); // rgb(231,233,235)
            text = Lcha(0.0,0.0,0.0,0.7), Lcha(1.0,0.0,0.0,0.7);
            text {
                selection = Lcha(0.7,0.0,0.125,0.7);
                font      = DEFAULT_FONT_MONO;
                size      = 12.0;
                highlight_bold = 0.02;
            }
            entry {
                padding = 10.0;
            }
            highlight {
                height = 24.0;
                corner_radius = 12.0;
            }
            padding = 5.0;
        }
        blank {
            color = Lcha(0.0,0.0,0.0,0.33);
            size = Vector2(20.0, 4.0);
            margin_sides = 2.0;
            margin_top = 8.0;
            corner_radius = 4.0;
        }
        label {
            /// Base label style, used when the label doesn't belong to any of the groups defined
            /// below.
            base_color = Lcha(1.0,0.0,0.0,1.0), Lcha(1.0,0.0,0.0,0.7);
            base_weight = 400.0;
            /// Label style when the node is disabled. For disabled nodes, all labels are rendered
            /// with this style.
            disabled_color = Lcha(0.95,0.0,0.0,0.9);
            disabled_weight = 400.0;
            /// Label style for placeholder argument names. Implies that the argument value is
            /// using default value.
            placeholder_color = Lcha(1.0,0.0,0.0,0.7);
            placeholder_weight = 700.0;
            /// Label style for connected ports. In connected ports, all labels are rendered with
            /// this style.
            connected_color = Lcha(1.0,0.0,0.0,1.0);
            connected_weight = 400.0;
            pending_alpha = graph_editor::node::pending::alpha_factor;
        }
        separator {
            color = Rgba(0.0, 0.0, 0.0, 0.12);
            margin = 7.5;
            width = 1.0;
        }
        argument_name {
            /// Label style for argument names.
            color = Lcha(1.0,0.0,0.0,0.7);
            margin = widget::separator::margin;
            weight = 400.0;
        }
        method {
            icon_gap = 10.0;
            pending_icon_alpha = graph_editor::node::pending::alpha_factor;
        }
    }
    colors {
        dimming {
            lightness_factor = 1.1;
            chroma_factor = 0.2;
        }
    }
    component {
        label {
            background = graph_editor::node::background , graph_editor::node::background;
            text       = Lcha(0.0,0.0,0.0,0.7) , Lcha(1.0,0.0,0.0,0.7);
            text {
                offset = 0.0, 0.0;
                size   = 12.0, 12.0;
                font   = DEFAULT_FONT, DEFAULT_FONT;
            }
            padding_outer   = 20.0, 20.0;
            padding_inner_x = 8.0, 8.0;
            padding_inner_y = 6.0, 6.0;
        }
        slider {
            background = graph_editor::node::background , graph_editor::node::background;
            overshoot_limit = 60.0, 60.0;
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
        toggle_button {
            non_toggled = Lcha(0.0,0.0,0.0,0.3), Lcha(0.4,0.0,0.0,1.0);
            toggled = Lcha(0.0,0.0,0.0,0.7), Lcha(1.0,0.0,0.0,0.7);
            hovered = Lcha(0.0,0.0,0.0,0.45), Lcha(1.0,0.0,0.0,0.7);
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


// ==========================
// === Theme registration ===
// ==========================

/// Default theme registration. The theme is registered and enabled in a before-main entry point in
/// order for it to be visible when shaders are being gathered during compilation phase. This makes
/// themes not switchable at runtime, which we might want to fix somehow in the future.
#[before_main(2)]
pub fn enable_default_theme() {
    let themes = ensogl_core::display::world::with_context(|t| t.theme_manager.clone());
    builtin::light::register(&themes);
    builtin::light::enable(&themes);
    themes.update();
}
