//! Various Java Native Interface utilities.

#![allow(missing_debug_implementations)]

use jni::JNIEnv;
use jni::objects::*;

pub use stdlib::StdLib;



// =====================
// === JNI Utilities ===
// =====================


// === User Defined Types ===

/// Ergonomic JObject constructor.
#[derive(Clone)]
pub struct Object<'a> {
    /// JNI Environment.
    pub env:&'a JNIEnv<'a>,
    /// JNI Object ID.
    pub obj:JClass<'a>,
    /// JNI Method ID.
    pub fun:JMethodID<'a>,
}

impl<'a> Object<'a> {
    /// Looks up a JObject constructor by type and constructor arguments.
    ///
    /// Example: `Object::new(env, "org/enso/ast/Ast$Ast", "(Lscala/Option;JJLjava/lang/Object;)V")`
    pub fn new(env:&'a JNIEnv<'a>, typ:&str, args:&str) -> Self {
        let err = "Could not find class ".to_string() + typ;
        let obj = env.find_class(typ).expect(&err.clone());
        let fun = env.get_method_id(typ, "<init>", args).expect(&(err+" method "+args));

        Self{env,obj,fun}
    }

    /// Creates a new instance of the given object.
    pub fn init(&self, args:&[JValue]) -> JObject<'a> {
        self.env.new_object_unchecked(self.obj, self.fun, args).unwrap()
    }
}


/// === Scala Standard Library ===

/// Trait for creating builtin objects from standard library.
pub trait ToJValue<'a> {
    /// JValue construct.
    fn jvalue(self, lib:&StdLib<'a>) -> JValue<'a>;
}

impl<'a> ToJValue<'a> for bool {
    fn jvalue(self, _lib:&StdLib<'a>) -> JValue<'a> {
        self.into()
    }
}

impl<'a> ToJValue<'a> for u8 {
    fn jvalue(self, _lib:&StdLib<'a>) -> JValue<'a> {
        self.into()
    }
}

impl<'a> ToJValue<'a> for i16 {
    fn jvalue(self, _lib:&StdLib<'a>) -> JValue<'a> {
        self.into()
    }
}

impl<'a> ToJValue<'a> for i32 {
    fn jvalue(self, _lib:&StdLib<'a>) -> JValue<'a> {
        self.into()
    }
}

impl<'a> ToJValue<'a> for i64 {
    fn jvalue(self, _lib:&StdLib<'a>) -> JValue<'a> {
        self.into()
    }
}

impl<'a> ToJValue<'a> for f32 {
    fn jvalue(self, _lib:&StdLib<'a>) -> JValue<'a> {
        self.into()
    }
}

impl<'a> ToJValue<'a> for f64 {
    fn jvalue(self, _lib:&StdLib<'a>) -> JValue<'a> {
        self.into()
    }
}

// impl<'a> ToJValue<'a> for &[bool] {
//     fn jvalue(self, lib:&StdLib<'a>) -> JValue<'a> {
//         let array = lib.env.new_boolean_array(self.len() as i32).unwrap();
//         lib.env.set_boolean_array_region(array, 0, self).unwrap();
//         array.into()
//     }
// }

impl<'a> ToJValue<'a> for &[u8] {
    fn jvalue(self, lib:&StdLib<'a>) -> JValue<'a> {
        lib.env.byte_array_from_slice(self).unwrap().into()
    }
}

impl<'a> ToJValue<'a> for &[i16] {
    fn jvalue(self, lib:&StdLib<'a>) -> JValue<'a> {
        let array = lib.env.new_short_array(self.len() as i32).unwrap();
        lib.env.set_short_array_region(array, 0, self).unwrap();
        array.into()
    }
}

impl<'a> ToJValue<'a> for &[i32] {
    fn jvalue(self, lib:&StdLib<'a>) -> JValue<'a> {
        let array = lib.env.new_int_array(self.len() as i32).unwrap();
        lib.env.set_int_array_region(array, 0, self).unwrap();
        array.into()
    }
}

impl<'a> ToJValue<'a> for &[i64] {
    fn jvalue(self, lib:&StdLib<'a>) -> JValue<'a> {
        let array = lib.env.new_long_array(self.len() as i32).unwrap();
        lib.env.set_long_array_region(array, 0, self).unwrap();
        array.into()
    }
}

impl<'a> ToJValue<'a> for &[f32] {
    fn jvalue(self, lib:&StdLib<'a>) -> JValue<'a> {
        let array = lib.env.new_float_array(self.len() as i32).unwrap();
        lib.env.set_float_array_region(array, 0, self).unwrap();
        array.into()
    }
}

impl<'a> ToJValue<'a> for &[f64] {
    fn jvalue(self, lib:&StdLib<'a>) -> JValue<'a> {
        let array = lib.env.new_double_array(self.len() as i32).unwrap();
        lib.env.set_double_array_region(array, 0, self).unwrap();
        array.into()
    }
}

impl<'a> ToJValue<'a> for JObject<'a> {
    fn jvalue(self, _lib:&StdLib<'a>) -> JValue<'a> {
        self.into()
    }
}

impl<'a> ToJValue<'a> for JValue<'a> {
    fn jvalue(self, _lib:&StdLib<'a>) -> JValue<'a> {
        self
    }
}

impl<'a> ToJValue<'a> for String {
    fn jvalue(self, lib:&StdLib<'a>) -> JValue<'a> {
        lib.env.new_string(self).unwrap().into()
    }
}

// TODO[JV] implement stdlib constructors
impl<'a,T:ToJValue<'a>> ToJValue<'a> for Vec<T> {
    fn jvalue(self, lib:&StdLib<'a>) -> JValue<'a> {
        lib.vector.obj.init(&[]).into()
    }
}

impl<'a,T:ToJValue<'a>> ToJValue<'a> for Option<T> {
    fn jvalue(self, lib:&StdLib<'a>) -> JValue<'a> {
        match self {
            None    => lib.option.none.clone(),
            Some(t) => lib.option.some.init(&[t.jvalue(lib)]),
        }.into()
    }
}

impl<'a,T:ToJValue<'a>> ToJValue<'a> for Box<T> {
    fn jvalue(self, lib:&StdLib<'a>) -> JValue<'a> {
        (*self).jvalue(lib)
    }
}


impl<'a> ToJValue<'a> for uuid::Uuid {
    fn jvalue(self, lib:&StdLib<'a>) -> JValue<'a> {
        lib.uuid.obj.init(&[self.as_bytes().jvalue(lib)]).into()
    }
}


/// Scala standard library.
mod stdlib {
    use super::*;



    /// Constructors of types in scala strandard library.
    #[allow(missing_docs)]
    #[derive(Clone)]
    pub struct StdLib<'a> {
        pub env    : &'a JNIEnv<'a>,
        pub object : JClass<'a>,
        pub vector : stdlib::Vector<'a>,
        pub option : stdlib::Option<'a>,
        pub uuid   : stdlib::Uuid<'a>,
    }

    impl<'a> StdLib<'a> {
        /// Creates a new instance of `StdLib`.
        pub fn new(env:&'a JNIEnv<'a>) -> Self {
            Self {
                env,
                object : env.find_class("java/lang/Object").unwrap(),
                vector : stdlib::Vector::new(env),
                option : stdlib::Option::new(env),
                uuid   : stdlib::Uuid::new(env),
            }
        }
    }

    #[allow(missing_docs)]
    #[derive(Clone)]
    pub struct Vector<'a> { pub obj:Object<'a> }
    impl<'a> Vector<'a> {
        /// Constructs a new Vector.
        pub fn new(env:&'a JNIEnv<'a>) -> Self {
            Self{obj:Object::new(env, "scala/collection/mutable/ArrayBuffer", "()V")}
        }
    }

    #[allow(missing_docs)]
    #[derive(Clone)]
    pub struct Option<'a> { pub none:JObject<'a>, pub some:Object<'a> }
    impl<'a> Option<'a> {
        /// Constructs a new Option.
        pub fn new(env:&'a JNIEnv<'a>) -> Self {
            let none = env.get_static_field(
               env.find_class("scala/None$").unwrap(),
               "MODULE$",
               "Lscala/None$;",
            ).unwrap().l().unwrap();

            let some = Object::new(env, "scala/Some", "(Ljava/lang/Object;)V");

            Self{none, some}
        }
    }

    #[allow(missing_docs)]
    #[derive(Clone)]
    pub struct Uuid<'a> { pub obj:Object<'a> }
    impl<'a> Uuid<'a> {
        /// Constructs a new Uuid.
        pub fn new(env:&'a JNIEnv<'a>) -> Self {
            Self{obj:Object::new(env, "java/util/UUID", "([B)V")}
        }
    }

}
