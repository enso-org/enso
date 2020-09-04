//! Various Java Native Interface utilities.

#![allow(missing_debug_implementations)]

use super::ast::Name;
use super::ast::Type;

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

/// Writes a JNI signature of user defined type into string buffer i.e. `package$Path$MyType`.
pub fn jni_name(name:&mut String, package:&str, typ:&Type) {
    name.push_str(package);
    name.push('$');
    for path in &typ.path {
        name.push_str(path.str.as_str());
        name.push('$');
    }
    name.push_str(typ.name.str.as_str());
}


// === Builtin Types ===

/// A tuple of JNI name and Scala name.
#[derive(Debug,Clone,Copy)]
pub struct JName {
    /// JNI name i.e. `java/lang/Object`.
    pub jni:&'static str,
    /// Scala name i.e. `Object`.
    pub scala:&'static str,
}

/// Returns the builtin name of given name if it exists.
pub fn builtin(name:&Name) -> Option<JName> {
   let name = match name.str.as_str() {
       // ERASED
       "Box"                             => JName {jni:"",                   scala:""       },
       // PRIMITIVES (https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/types.html)
       "usize" | "isize" | "u64" | "i64" => JName {jni:"L",                  scala:"Long"   },
       "u32"   | "i32"                   => JName {jni:"I",                  scala:"Int"    },
       "u16"   | "i16"   | "i8"          => JName {jni:"S",                  scala:"Short"  },
       "u8"                              => JName {jni:"B",                  scala:"Byte"   },
       "f64"                             => JName {jni:"D",                  scala:"Double" },
       "f32"                             => JName {jni:"F",                  scala:"Float"  },
       "boolean"                         => JName {jni:"Z",                  scala:"Boolean"},
       "char"                            => JName {jni:"C",                  scala:"Char"   },
       // STDLIB
       "Object"                          => JName {jni:"java/lang/Object",   scala:"Object" },
       "Vec"                             => JName {jni:"scala/Vector",       scala:"Vector" },
       "Option"                          => JName {jni:"scala/Option",       scala:"Option" },
       _                                 => None?,
    };
    Some(name)
}


/// === Scala Standard Library ===

/// Scala standard library.
mod stdlib {
    use super::*;



    /// Constructors of types in scala strandard library.
    #[derive(Clone)]
    pub struct StdLib<'a> {
        /// Scala Vector.
        pub vector: stdlib::Vector<'a>,
        /// Scala Option.
        pub option: stdlib::Option<'a>,
    }

    impl<'a> StdLib<'a> {
        /// Creates a new instance of `StdLib`.
        pub fn new(env:&'a JNIEnv<'a>) -> Self {
            Self {
                vector: stdlib::Vector{env},
                option: stdlib::Option{env},
            }
        }
    }

    /// Scala Vector.
    #[derive(Clone)]
    pub struct Vector<'a> { env:&'a JNIEnv<'a> }

    impl<'a> Vector<'a> {
        /// Constructs a new Vector.
        pub fn init<T>(&self, _arg:Vec<T>, _fun:impl Fn(T) -> JValue<'a>) -> JObject<'a> {
            unimplemented!()
        }
    }

    #[derive(Clone)]
    pub struct Option<'a> { env:&'a JNIEnv<'a> }

    impl<'a> Option<'a> {
        /// Constructs a new Option.
        pub fn init<T>(&self, _arg:std::option::Option<T>, _fun:impl Fn(T) -> JValue<'a>) -> JObject<'a> {
            unimplemented!()
        }
    }
}
