// ==========================
// === Define Icons Macro ===
// ==========================

/// Macro for defining icon set.
///
/// The macro takes many modules with attached "variant name". Inside the modules, there should
/// be icon defined with `ensogl::define_shape_system!` macro. The macro will also generate an
/// enum called `Id` gathering all icon' "variant names". The enum will allow for dynamically
/// creating given icon shape view (returned as [`crate::icon::AnyIcon`]).
///
/// # Example
///
/// ```
/// use ensogl::prelude::*;
/// use ensogl::display::shape::*;
/// use ide_view_component_group::icon;
/// use ide_view_component_group::define_icons;
///
/// define_icons! {
///     /// The example of icon.
///     pub mod icon1(Icon1) {
///         // This is a normal module and you may define whatever you want. It must however
///         // define shape system with the macro below; otherwise the generated code wont compile.
///         //
///         // `use super::*` import is added silently.
///         ensogl::define_shape_system! {
///             (style:Style, strong_color: Vector4, weak_color: Vector4) {
///                 Plane().into()
///             }
///         }
///     }
///
///     pub mod icon2(Icon2) {
///         ensogl::define_shape_system! {
///             (style:Style, strong_color: Vector4, weak_color: Vector4) {
///                 Plane().fill(strong_color).into()
///             }
///         }
///     }
/// }
///
/// fn main () {
///     let app = ensogl::application::Application::new("root");
///     let logger = Logger::new("icon");
///     let icon1 = Id::Icon1.create_shape(&logger, Vector2(10.0, 10.0));
///     let icon2_id: Id = "Icon2".parse().unwrap();
///     assert_eq!(icon2_id, Id::Icon2);
///     let icon2 = icon2_id.create_shape(&logger, Vector2(11.0, 11.0));
///     app.display.default_scene.add_child(&icon1);
///     app.display.default_scene.add_child(&icon2);
///
///     // Invalid icon
///     let icon3 = "Icon3".parse::<Id>();
///     assert!(icon3.is_err());
/// }
#[macro_export]
macro_rules! define_icons {
    ($(
        $(#$meta:tt)*
        pub mod $name:ident($variant:ident) {
            $($content:tt)*
        }
    )*) => {
        $(
            $(#$meta)*
            pub mod $name {
                use super::*;
                $($content)*
            }
        )*

        /// An identifier of one of the icons generated by the same `define_icons` macro invocation.
        #[allow(missing_docs)]
        #[derive(Copy, Clone, Debug, Eq, PartialEq)]
        pub enum Id {
            $($variant),*
        }

        impl Id {
            /// Create icon's shape with given size.
            pub fn create_shape(&self, logger: impl AnyLogger, size: Vector2) -> $crate::icon::Any {
                match self {$(
                    Self::$variant => {
                        let view = $name::View::new(logger);
                        view.size.set(size);
                        let strong_color = view.strong_color.clone_ref();
                        let weak_color = view.weak_color.clone_ref();
                        let view = Box::new(view);
                        $crate::icon::Any {view, strong_color, weak_color}
                    }
                )*}
            }

            /// Call `f` for each possible icon id.
            pub fn for_each<F: FnMut(Self)>(mut f: F) {
                $(f(Self::$variant);)*
            }
        }

        impl FromStr for Id {
            type Err = $crate::icon::UnknownIcon;
            fn from_str(s: &str) -> Result<Id, Self::Err> {
                match s {
                    $(stringify!($variant) => Ok(Self::$variant),)*
                    name => Err(Self::Err {name: name.to_owned() }),
                }
            }
        }
    }
}
