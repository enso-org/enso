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
    pub env:&'a JNIEnv<'a>,
    pub obj:JClass<'a>,
    pub fun:JMethodID<'a>,
    pub typ:String,
}

impl<'a> Object<'a> {
    /// Looks up a JObject constructor by type and constructor arguments.
    ///
    /// Example: `Object::new(env, "org/enso/ast/Ast$Ast", "(Lscala/Option;JJLjava/lang/Object;)V")`
    pub fn new(env:&'a JNIEnv<'a>, typ:&str, args:&str) -> Self {
        let obj = env.find_class(typ).unwrap();
        let fun = env.get_method_id(typ, "<init>", args).unwrap();

        Self{env,obj,fun,typ:typ.into()}
    }

    /// Creates a new instance of the given object.
    pub fn init(&self, args:&[JValue]) -> JObject<'a> {
        self.env.new_object_unchecked(self.obj, self.fun, args).unwrap()
    }
}


/// === Scala Standard Library ===

/// Scala standard library.
mod stdlib {
    use super::*;



    /// Constructors of types in scala strandard library.
    #[derive(Clone)]
    pub struct StdLib<'a> {
        /// Scala Vector.
        pub vec: stdlib::Vector<'a>,
        /// Scala Option.
        pub option: stdlib::Option<'a>,
        /// Scala Uuid.
        pub uuid: stdlib::Uuid<'a>,
        /// Scala Option.
        pub string: stdlib::String<'a>,
    }

    impl<'a> StdLib<'a> {
        /// Creates a new instance of `StdLib`.
        pub fn new(env:&'a JNIEnv<'a>) -> Self {
            Self {
                vec: stdlib::Vector{env},
                option: stdlib::Option{env},
                uuid: stdlib::Uuid{env},
                string: stdlib::String{env},
            }
        }
    }

    /// Scala Vector.
    #[derive(Clone)]
    pub struct Vector<'a> { env:&'a JNIEnv<'a> }
    impl<'a> Vector<'a> {
        /// Constructs a new Vector.
        pub fn init<T>(&self, _arg:Vec<T>) -> JObject<'a> {
            unimplemented!()
        }
    }

    #[derive(Clone)]
    pub struct Option<'a> { env:&'a JNIEnv<'a> }
    impl<'a> Option<'a> {
        /// Constructs a new Option.
        pub fn init<T>(&self, _arg:std::option::Option<T>) -> JObject<'a> {
            unimplemented!()
        }
    }

   #[derive(Clone)]
    pub struct Uuid<'a> { env:&'a JNIEnv<'a> }
    impl<'a> Uuid<'a> {
        /// Constructs a new Option.
        pub fn init<T>(&self, _arg:uuid::Uuid) -> JObject<'a> {
            unimplemented!()
        }
    }

    #[derive(Clone)]
    pub struct String<'a> { env:&'a JNIEnv<'a> }
    impl<'a> String<'a> {
        /// Constructs a new Option.
        pub fn init<T>(&self, _arg:std::string::String) -> JObject<'a> {
            unimplemented!()
        }
    }
}
