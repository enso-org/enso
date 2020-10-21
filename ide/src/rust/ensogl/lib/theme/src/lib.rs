//! Application theme setup.
//!
#![warn(missing_docs)]
#![warn(trivial_casts)]
#![warn(trivial_numeric_casts)]
#![warn(unused_import_braces)]
#![warn(unused_qualifications)]
#![warn(unsafe_code)]
#![warn(missing_copy_implementations)]
#![warn(missing_debug_implementations)]

/// `define_theme` helper.
macro_rules! _define_theme_literals {
    ([$theme_name:ident $($path:ident)*] $var_name:ident = $e:expr) => {
        $theme_name.insert(stringify!($($path.)*$var_name), $e);
    };

    ([$($path:ident)*] $var_name:ident = $e:expr; $($rest:tt)*) => {
        _define_theme_literals!([$($path)*] $var_name = $e);
        _define_theme_literals!([$($path)*] $($rest)*);
    };

    ([$($path:ident)*] $path_segment:ident {$($t:tt)*}) => {
        _define_theme_literals!([$($path)* $path_segment] $($t)*);
    };

    ([$($path:ident)*] $path_segment:ident {$($t:tt)*} $($rest:tt)*) => {
        _define_theme_literals!([$($path)*] $path_segment {$($t)*});
        _define_theme_literals!([$($path)*] $($rest)*);
    };
}

macro_rules! _define_theme_modules {
    ([$theme_name:ident $($path:ident)*] $var_name:ident = $e:expr) => {
        pub const $var_name : &str = stringify!($($path.)*$var_name);
    };

    ([$($path:ident)*] $var_name:ident = $e:expr; $($rest:tt)*) => {
        _define_theme_modules!([$($path)*] $var_name = $e);
        _define_theme_modules!([$($path)*] $($rest)*);
    };

    ([$($path:ident)*] $path_segment:ident {$($t:tt)*}) => {
        pub mod $path_segment {
            _define_theme_modules!([$($path)* $path_segment] $($t)*);
        }
    };

    ([$($path:ident)*] $path_segment:ident {$($t:tt)*} $($rest:tt)*) => {
        _define_theme_modules!([$($path)*] $path_segment {$($t)*});
        _define_theme_modules!([$($path)*] $($rest)*);
    };
}

/// Used to define default theme. This one aside from generating code for `StyleManager` also creates
/// nested public modules that makes accessing values much better than with bare string literals.
/// It adds the `var` module with string constants, so now, instead of having to get data by string
/// literal - like `style.get("foo.bar.baz",fallback)`, you can do
/// `style.get(theme::vars::foo::bar::baz,fallback)`.
#[macro_export]
macro_rules! define_default_theme {
    ($name:ident $($t:tt)*) => {
        define_theme!($name $($t)*);

        #[allow(non_upper_case_globals)]
        #[allow(missing_docs)]
        pub mod vars {
            _define_theme_modules!([$name] $($t)*);
        }
    };
}

/// Used to define any theme.
/// Generates code for `StyleManager` from given cascade style definition.
/// It generates module equal to the theme name and there will be function `setup` which creates a
/// theme definition in `app`.
#[macro_export]
macro_rules! define_theme {
    ($name:ident $($t:tt)*) => {
        #[allow(missing_docs)]
        pub mod $name {
            use ensogl_core::application::Application;
            use ensogl_core::data::color;
            use ensogl_core::display::style::theme;

            /// Setup the `$name` theme in application.
            pub fn setup(app:&Application) {
                let mut $name = theme::Theme::new();
                _define_theme_literals!([$name] $($t)*);
                app.themes.register(stringify!($name),$name);
                app.themes.set_enabled(&[stringify!($name)]);
            }
        }
    };
}

define_theme! { dark
    application {
        background {
            color = color::Lcha::new(0.13,0.013,0.18,1.0)
        }
    }
    text_editor {
        text {
            color = color::Lcha::new(1.0,0.0,0.0,0.7);
            selection {
                color = color::Lcha::new(0.7,0.0,0.125,0.7)
            }
        }
    }
    graph_editor {
        node {
            background {
                color = color::Lcha::new(0.2,0.013,0.18,1.0);
                variant {
                    dimmed = color::Lcha::new(0.15,0.013,0.18,1.0)
                }
            }
            shadow {
                color        = color::Lcha::new(0.0,0.0,0.0,0.20);
                fading_color = color::Lcha::new(0.0,0.0,0.0,0.0);
                exponent     = 2.0
            }
            selection {
                color = color::Lcha::new(0.72,0.5,0.22,1.0);
                size = 7.0
            }
            text {
                color             = color::Lcha::new(1.0,0.0,0.0,0.7);
                missing_arg_color = color::Lcha::new(1.0,0.0,0.0,0.3);
                variant {
                    dimmed = color::Lcha::new(0.25,0.013,0.18,1.0)
                }
                selection {
                    color = color::Lcha::new(0.7,0.0,0.125,0.7)
                }
            }
            actions {
                icon {
                    color = color::Lcha::new(1.0,0.0,0.0,0.7);
                    variant {
                        dimmed = color::Lcha::new(0.4,0.00,0.0,1.0)
                    }
                }
            }
        }
        visualization {
            background {
                color = color::Lcha::new(0.2,0.013,0.18,1.0)
            }
            shadow {
                color        = color::Lcha::new(0.0,0.0,0.0,0.20);
                fading_color = color::Lcha::new(0.0,0.0,0.0,0.0);
                exponent     = 2.0;
                html {
                    alpha = 0.16;
                    size  = 16.0
                }
            }
            text {
                color = color::Lcha::new(1.0,0.0,0.0,0.7);
                selection {
                    color = color::Lcha::new(0.7,0.0,0.125,0.7)
                }
            }
            action_bar {
                background {
                    color = color::Lcha::new(0.3,0.013,0.18,1.0)
                }
                icon {
                    color = color::Lcha::new(1.0,0.0,0.0,0.7)
                }
                text {
                    color = color::Lcha::new(1.0,0.0,0.0,0.7)
                }
            }
        }
        breadcrumbs {
            full {
                color = color::Lcha::new(1.0,0.0,0.0,0.7)
            }
            transparent {
                color = color::Lcha::new(1.0,0.0,0.0,0.4)
            }
            selected {
                color = color::Lcha::new(1.0,0.0,0.0,0.6)
            }
            deselected{
                left {
                    color = color::Lcha::new(1.0,0.0,0.0,0.6)
                }
                right {
                    color = color::Lcha::new(1.0,0.0,0.0,0.2)
                }
            }
            hover {
                color = color::Lcha::new(1.0,0.0,0.0,0.6)
            }
        }
        edge {
            split_color {
                lightness_factor = 0.2;
                chroma_factor = 1.0
            }
            _type {
                missing {
                     color = color::Lcha::new(0.5,0.0,0.0,1.0)
                }
                color {
                    luminance = 0.5;
                    chroma = 0.8
                }
            }
        }
    }
    widget {
        list_view {
            background {
                color = color::Lcha::new(0.2,0.013,0.18,1.0)
            }
            shadow {
                color        = color::Lcha::new(0.0,0.0,0.0,0.20);
                fading_color = color::Lcha::new(0.0,0.0,0.0,0.0);
                exponent     = 2.0
            }
            highlight {
                color = color::Lcha::new(0.72,0.5,0.22,1.0)
            }
            text {
                color = color::Lcha::new(1.0,0.0,0.0,0.7);
                highlight {
                    color = color::Lcha::new(0.7,0.0,0.0,1.0)
                }
                selection {
                    color = color::Lcha::new(0.7,0.0,0.125,0.7)
                }
            }
        }
    }
    shadow {
        color        = color::Lcha::new(0.0,0.0,0.0,0.20);
        fading_color = color::Lcha::new(0.0,0.0,0.0,0.0);
        exponent     = 2.0
    }
    colors {
        dimming {
            lightness_factor = 0.8;
            chroma_factor    = 0.2
        }
    }
}

define_default_theme! { light
    application {
        background {
            color = color::Lcha::new(0.96,0.013,0.18,1.0)
        }
    }
    text_editor {
        text {
            color = color::Lcha::new(0.0,0.0,0.0,0.7);
            selection {
                color = color::Lcha::new(0.7,0.0,0.125,0.7)
            }
        }
    }
    graph_editor {
        node {
            background {
                color = color::Lcha::new(0.98,0.013,0.18,1.0);
                variant {
                    dimmed = color::Lcha::new(0.98,0.013,0.18,1.0)
                }
            }
            shadow {
                color        = color::Lcha::new(0.0,0.0,0.0,0.20);
                fading_color = color::Lcha::new(0.0,0.0,0.0,0.0);
                exponent     = 2.0
            }
            selection {
                color = color::Lcha::new(0.83,0.58,0.436,1.0);
                size = 7.0
            }
            text {
                color             = color::Lcha::new(0.0,0.0,0.0,0.7);
                missing_arg_color = color::Lcha::new(0.0,0.0,0.0,0.3);
                variant {
                    dimmed = color::Lcha::new(0.7,0.0,0.0,0.7)
                }
                selection {
                    color = color::Lcha::new(0.7,0.0,0.125,0.7)
                }
            }
            actions {
                icon {
                    color = color::Lcha::new(0.0,0.0,0.0,0.7);
                    variant {
                        dimmed = color::Lcha::new(0.7,0.0,0.0,0.7)
                    }
                }
            }
        }
        visualization {
            background {
                color = color::Lcha::new(0.98,0.013,0.18,1.0)
            }
            shadow {
                color        = color::Lcha::new(0.0,0.0,0.0,0.20);
                fading_color = color::Lcha::new(0.0,0.0,0.0,0.0);
                exponent     = 2.0;
                html {
                    alpha = 0.16;
                    size  = 16.0
                }
            }
            text {
                color = color::Lcha::new(0.0,0.0,0.0,0.7);
                selection {
                    color = color::Lcha::new(0.7,0.0,0.125,0.7)
                }
            }
            action_bar {
                background {
                    color = color::Lcha::new(0.94,0.013,0.18,1.0)
                }
                icon {
                    color = color::Lcha::new(0.0,0.0,0.0,0.7)
                }
                text {
                    color = color::Lcha::new(0.0,0.0,0.0,0.7)
                }
            }
        }
        breadcrumbs {
            full {
                color = color::Lcha::new(0.0,0.0,0.0,0.7)
            }
            transparent {
                color = color::Lcha::new(0.0,0.0,0.0,0.4)
            }
            selected {
                color = color::Lcha::new(0.0,0.0,0.0,0.6)
            }
            deselected{
                left {
                    color = color::Lcha::new(0.0,0.0,0.0,0.6)
                }
                right {
                    color = color::Lcha::new(0.0,0.0,0.0,0.2)
                }
            }
            hover {
                color = color::Lcha::new(0.0,0.0,0.0,0.6)
            }
        }
        edge {
            split_color {
                lightness_factor = 1.2;
                chroma_factor = 0.8
            }
            _type {
                missing {
                    color = color::Lcha::new(0.8,0.0,0.0,1.0)
                }
                color {
                    luminance = 0.8;
                    chroma = 0.6
                }
            }
        }
    }
    widget {
        list_view {
            background {
                color = color::Lcha::new(0.98,0.013,0.18,1.0)
            }
            shadow {
                color        = color::Lcha::new(0.0,0.0,0.0,0.20);
                fading_color = color::Lcha::new(0.0,0.0,0.0,0.0);
                exponent     = 2.0
            }
            highlight {
                color = color::Lcha::new(0.83,0.58,0.436,1.0)
            }
            text {
                color = color::Lcha::new(0.0,0.0,0.0,0.7);
                highlight {
                    color = color::Lcha::new(0.8,0.0,0.0,1.0)
                }
                selection {
                    color = color::Lcha::new(0.7,0.0,0.125,0.7)
                }
            }
        }
    }
    colors {
        dimming {
            lightness_factor = 1.1;
            chroma_factor    = 0.2
        }
    }
}
