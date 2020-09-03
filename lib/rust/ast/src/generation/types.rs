
use super::ast::Name;

use std;
use jni::JNIEnv;
use jni::objects::*;




// === User Defined Types ===

struct Object<'a> {
    pub env:&'a JNIEnv<'a>,
    pub obj:JClass<'a>,
    pub fun:JMethodID<'a>,
    pub typ:String,
}

impl<'a> Object<'a> {
    pub fn new(env:&'a JNIEnv<'a>, typ:&str, args:&str) -> Self {
        let obj = env.find_class(typ).unwrap();
        let fun = env.get_method_id(typ, "<init>", args).unwrap();

        Self{env,obj,fun,typ:typ.into()}
    }
    pub fn init(&self, args:&[JValue]) -> JObject {
        self.env.new_object_unchecked(self.obj, self.fun, args).unwrap()
    }
}


// === Builtin Types ===

pub struct JName { pub jni:&'static str, pub scala:&'static str }

pub fn builtin(name:&Name) -> Option<JName> {
   let name = match name.str.as_str() {
       // ERASED
       "Box"                             => JName {jni:"",               scala:""       },
       // PRIMITIVES (https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/types.html)
       "usize" | "isize" | "u64" | "i64" => JName {jni:"L",              scala:"Long"   },
       "u32"   | "i32"                   => JName {jni:"I",              scala:"Int"    },
       "u16"   | "i16"   | "i8"          => JName {jni:"S",              scala:"Short"  },
       "u8"                              => JName {jni:"B",              scala:"Byte"   },
       "f64"                             => JName {jni:"D",              scala:"Double" },
       "f32"                             => JName {jni:"F",              scala:"Float"  },
       "boolean"                         => JName {jni:"Z",              scala:"Boolean"},
       "char"                            => JName {jni:"C",              scala:"Char"   },
       // STDLIB
       "Vec"                             => JName {jni:"Lscala/Vector;", scala:"Vector" },
       "Option"                          => JName {jni:"Lscala/Option;", scala:"Option" },
       _                                 => None?,
    };
    Some(name)
}

pub struct StdLib<'a> {
    pub vector: Vector<'a>,
    pub option: Maybe<'a>,
}


pub struct Vector<'a> { pub env:&'a JNIEnv<'a> }

impl<'a> Vector<'a> {
    pub fn init<T>(&self, arg:Vec<T>, fun:impl Fn(T) -> JValue<'a>) -> JObject<'a> {
        unimplemented!()
    }
}

pub struct Maybe<'a> { pub env:&'a JNIEnv<'a> }

impl<'a> Maybe<'a> {
   pub fn init<T>(&self, arg:Option<T>, fun:impl Fn(T) -> JValue<'a>) -> JObject<'a> {
        unimplemented!()
   }
}
