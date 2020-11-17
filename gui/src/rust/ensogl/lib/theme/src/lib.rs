//! Application theme setup.

#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]



// ==============
// === Macros ===
// ==============

/// `define_theme` helper.
#[macro_export]
macro_rules! _define_theme_literals {
    ($theme:ident [$($path:ident)*]) => {};
    ($theme:ident [$($path:ident)*] $qual:ident . $($var:ident).+ = $e:expr $(;$($rest:tt)*)?) => {
        $crate::_define_theme_literals!{$theme [$($path)*] $qual { $($var).+ = $e } $($($rest)*)?}
    };
    ($theme:ident [$($path:ident)*] $var:ident = $e:expr $(;$($rest:tt)*)?) => {
        $theme.insert(stringify!($($path.)*$var), $e);
        $crate::_define_theme_literals!{$theme [$($path)*] $($($rest)*)?}
    };
    ($theme:ident [$($path:ident)*] $path_segment:ident {$($t:tt)*} $($rest:tt)*) => {
        $crate::_define_theme_literals!{$theme [$($path)* $path_segment] $($t)*}
        $crate::_define_theme_literals!{$theme [$($path)*] $($rest)*}
    };
}

/// `define_theme` helper.
#[macro_export]
macro_rules! _define_theme_modules {
    ($theme:ident [$($path:ident)*]) => {};
    ($theme:ident [$($path:ident)*] $qual:ident . $($var:ident).+ = $e:expr $(;$($rest:tt)*)?) => {
        $crate::_define_theme_modules!{$theme [$($path)*] $qual {$($var).+ = $e} $($($rest)*)?}
    };
    ($theme:ident [$($path:ident)*] $var:ident = $e:expr $(;$($rest:tt)*)?) => {
        pub const $var : StaticPath = StaticPath::new(stringify!($($path.)*$var));
        $crate::_define_theme_modules!{$theme [$($path)*] $($($rest)*)?}
    };
    ($theme:ident [$($path:ident)*] $path_segment:ident {$($t:tt)*} $($rest:tt)*) => {
        pub mod $path_segment {
            use ensogl_core::display::style::StaticPath;
            pub const HERE : StaticPath = StaticPath::new(stringify!($($path.)*$path_segment));
            $crate::_define_theme_modules!{$theme [$($path)* $path_segment] $($t)*}
        }
        $crate::_define_theme_modules!{$theme [$($path)*] $($rest)*}
    };
}

/// Used to define default theme. This one aside from generating code for `StyleManager` also creates
/// nested public modules that makes accessing values much better than with bare string literals.
/// It adds the `var` module with string constants, so now, instead of having to get data by string
/// literal - like `style.get("foo.bar.baz",fallback)`, you can do
/// `style.get(theme::foo::bar::baz,fallback)`.
#[macro_export]
macro_rules! define_default_theme {
    ($name:ident $($t:tt)*) => {
        define_theme!{$name $($t)*}

        #[allow(non_upper_case_globals)]
        #[allow(missing_docs)]
        #[allow(non_snake_case)]
        pub mod vars {
            $crate::_define_theme_modules!{$name [] $($t)*}
        }
        pub use vars::*;
    };
}

/// Generates code for `StyleManager` from given cascade style definition. It generates module equal
/// to the theme name and there will be function `setup` which creates a theme definition in `app`.
#[macro_export]
macro_rules! define_theme {
    ($name:ident $($t:tt)*) => {
        #[allow(missing_docs)]
        #[allow(non_snake_case)]
        #[allow(unused_imports)]
        pub mod $name {
            use super::*;
            use ensogl_core::application::Application;
            use ensogl_core::data::color::Lcha;
            use ensogl_core::display::style::theme;

            /// Setup the `$name` theme in application.
            pub fn setup(app:&Application) {
                let mut $name = theme::Theme::new();
                $crate::_define_theme_literals!{$name [] $($t)*}
                app.themes.register(stringify!($name),$name);
                app.themes.set_enabled(&[stringify!($name)]);
            }
        }
    };
}



// =============================
// === Light Theme & Modules ===
// =============================

define_default_theme! { __light_theme__
    application {
        background = Lcha(0.96,0.014,0.18,1.0);
    }
    code {
        syntax {
            base      = Lcha(0.0,0.0,0.0,0.7);
            disabled  = Lcha(0.0,0.0,0.0,0.3);
            expected  = Lcha(0.0,0.0,0.0,0.3);
            selection = Lcha(0.7,0.0,0.125,0.7);
        }
        types {
            luminance  = 0.8;
            chroma     = 0.6;
            missing    = Lcha(0.8,0.0,0.0,1.0);
            selected   = graph_editor::node::background;
            overriden {
                Text.hue   = 0.22;
                Number.hue = 0.68;
            }
        }
    }
    graph_editor {
        node {
            background         = Lcha(0.98,0.014,0.18,1.0);
            background.skipped = Lcha(0.98,0.014,0.18,1.0);
            shadow             = Lcha(0.0,0.0,0.0,0.20);
            shadow {
                fading   = Lcha(0.0,0.0,0.0,0.0);
                exponent = 2.0;
            }
            selection      = Lcha(0.83,0.63,0.436,1.0);
            selection.size = 9.0;
            text = Lcha(0.0,0.0,0.0,0.7);
            text {
                missing_arg    = Lcha(0.0,0.0,0.0,0.3);
                variant.dimmed = Lcha(0.7,0.0,0.0,0.7);
                selection      = Lcha(0.7,0.0,0.125,0.7);
            }
            actions {
                icon = Lcha(0.0,0.0,0.0,0.7);
                icon {
                    variant.dimmed = Lcha(0.7,0.0,0.0,0.7);
                }
            }
        }
        visualization {
            background = Lcha(0.98,0.014,0.18,1.0);
            shadow     = Lcha(0.0,0.0,0.0,0.20);
            shadow {
                fading   = Lcha(0.0,0.0,0.0,0.0);
                exponent = 2.0;
                html {
                    alpha = 0.16;
                    size  = 16.0;
                }
            }
            text           = Lcha(0.0,0.0,0.0,0.7);
            text.selection = Lcha(0.7,0.0,0.125,0.7);
            action_bar {
                background = Lcha(0.94,0.014,0.18,1.0);
                icon       = Lcha(0.0,0.0,0.0,0.7);
                text       = Lcha(0.0,0.0,0.0,0.7);
            }
        }
        breadcrumbs {
            full        = Lcha(0.0,0.0,0.0,0.7);
            transparent = Lcha(0.0,0.0,0.0,0.4);
            selected    = Lcha(0.0,0.0,0.0,0.7);
            hover       = Lcha(0.0,0.0,0.0,0.7);
            deselected  {
                left  = Lcha(0.0,0.0,0.0,0.5);
                right = Lcha(0.0,0.0,0.0,0.2);
            }
        }
        edge {
            split {
                lightness_factor = 1.2;
                chroma_factor    = 0.8;
            }
        }
    }
    widget {
        list_view {
            background = Lcha(0.98,0.014,0.18,1.0);
            highlight  = Lcha(0.83,0.63,0.436,1.0);
            shadow     = Lcha(0.0,0.0,0.0,0.20);
            shadow {
                fading   = Lcha(0.0,0.0,0.0,0.0);
                exponent = 2.0;
            }
            text = Lcha(0.0,0.0,0.0,0.7);
            text {
                highlight = Lcha(0.8,0.0,0.0,1.0);
                selection = Lcha(0.7,0.0,0.125,0.7);
            }
        }
    }
    colors {
        dimming {
            lightness_factor = 1.1;
            chroma_factor    = 0.2;
        }
    }
}



// ==================
// === Dark Theme ===
// ==================

define_theme! { __dark_theme__
    application {
        background = Lcha(0.13,0.014,0.18,1.0);
    }
    code {
        syntax {
            base      = Lcha(1.0,0.0,0.0,0.7);
            disabled  = Lcha(1.0,0.0,0.0,0.2);
            expected  = Lcha(1.0,0.0,0.0,0.3);
            selection = Lcha(0.7,0.0,0.125,0.7);
        }
        types {
            luminance  = 0.75;
            chroma     = 0.4;
            missing    = Lcha(0.5,0.0,0.0,1.0);
            selected   = graph_editor::node::background;
            overriden {
                Text.hue   = 0.217;
                Number.hue = 0.68;
            }
        }
    }
    graph_editor {
        node {
            background         = Lcha(0.2,0.014,0.18,1.0);
            background.skipped = Lcha(0.15,0.014,0.18,1.0);
            shadow             = Lcha(0.0,0.0,0.0,0.20);
            shadow {
                fading   = Lcha(0.0,0.0,0.0,0.0);
                exponent = 2.0;
            }
            selection      = Lcha(0.72,0.54,0.22,1.0);
            selection.size = 9.0;
            text           = Lcha(1.0,0.0,0.0,0.7);
            text {
                missing_arg    = Lcha(1.0,0.0,0.0,0.3);
                variant.dimmed = Lcha(0.25,0.014,0.18,1.0);
                selection      = Lcha(0.7,0.0,0.125,0.7);
            }
            actions {
                icon = Lcha(1.0,0.0,0.0,0.7);
                icon {
                    variant.dimmed = Lcha(0.4,0.0,0.0,1.0);
                }
            }
        }
        visualization {
            background = Lcha(0.2,0.014,0.18,1.0);
            shadow     = Lcha(0.0,0.0,0.0,0.20);
            shadow {
                fading   = Lcha(0.0,0.0,0.0,0.0);
                exponent = 2.0;
                html {
                    alpha = 0.16;
                    size  = 16.0
                }
            }
            text           = Lcha(1.0,0.0,0.0,0.7);
            text.selection = Lcha(0.7,0.0,0.125,0.7);
            action_bar {
                background = Lcha(0.3,0.014,0.18,1.0);
                icon       = Lcha(1.0,0.0,0.0,0.7);
                text       = Lcha(1.0,0.0,0.0,0.7);
            }
        }
        breadcrumbs {
            full        = Lcha(1.0,0.0,0.0,0.7);
            transparent = Lcha(1.0,0.0,0.0,0.4);
            selected    = Lcha(1.0,0.0,0.0,0.7);
            hover       = Lcha(1.0,0.0,0.0,0.7);
            deselected  {
                left  = Lcha(1.0,0.0,0.0,0.5);
                right = Lcha(1.0,0.0,0.0,0.2);
            }
        }
        edge {
            split {
                lightness_factor = 0.2;
                chroma_factor    = 1.0;
            }
        }
    }
    widget {
        list_view {
            background = Lcha(0.2,0.014,0.18,1.0);
            highlight  = Lcha(0.72,0.54,0.22,1.0);
            shadow     = Lcha(0.0,0.0,0.0,0.20);
            shadow {
                fading   = Lcha(0.0,0.0,0.0,0.0);
                exponent = 2.0;
            }
            text = Lcha(1.0,0.0,0.0,0.7);
            text {
                highlight = Lcha(0.7,0.0,0.0,1.0);
                selection = Lcha(0.7,0.0,0.125,0.7);
            }
        }
    }
    shadow = Lcha(0.0,0.0,0.0,0.20);
    shadow {
        fading   = Lcha(0.0,0.0,0.0,0.0);
        exponent = 2.0;
    }
    colors {
        dimming {
            lightness_factor = 0.8;
            chroma_factor    = 0.2;
        }
    }
}



// ====================
// === Theme Export ===
// ====================

/// Builtin themes.
pub mod builtin {
    pub use super::__light_theme__ as light;
    pub use super::__dark_theme__  as dark;
}
