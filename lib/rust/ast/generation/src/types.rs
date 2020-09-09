//! Various Java Native Interface utilities.

#![allow(missing_debug_implementations)]

use crate::api;
use crate::ast::Name;
use crate::ast::Type;

use std::collections::BTreeSet as Set;


// =====================
// === JNI Utilities ===
// =====================

/// Writes a JNI signature of user defined type into string buffer i.e. `package$Path$MyType`.
pub fn jni_name(mut name:String, typ:&Type, package:&str) -> String {
    name += package;
    name += "$";
    for path in &typ.path {
        name += api::name::typ(&path).str.as_str();
        name += "$";
    }
    name + typ.name.str.as_str()
}

/// Writes a JNI argument signature of given type into string buffer i.e. `Lpackage$Path$MyType;`.
pub fn jni_arg(mut name:String, typ:&Type, package:&str, type_names:&Set<Name>) -> String {
    if let Some(jname) = builtin(&typ.name) {
        if jname.jni.is_empty() {
            return jni_arg(name, &typ.args[0], package, type_names);
        }
        name += jname.jni;
    } else if !type_names.contains(&typ.name) {
        name += "Ljava/lang/Object;";
    } else {
        name += "L";
        name  = jni_name(name, typ, package);
        name += ";";
    }
    name
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
       "Box"                             => JName {jni:"",  scala:""       },
       // PRIMITIVES (https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/types.html)
       "usize" | "isize" | "u64" | "i64" => JName {jni:"J", scala:"Long"   },
       "u32"   | "i32"                   => JName {jni:"I", scala:"Int"    },
       "u16"   | "i16"   | "i8"          => JName {jni:"S", scala:"Short"  },
       "u8"                              => JName {jni:"B", scala:"Byte"   },
       "f64"                             => JName {jni:"D", scala:"Double" },
       "f32"                             => JName {jni:"F", scala:"Float"  },
       "boolean"                         => JName {jni:"Z", scala:"Boolean"},
       "char"                            => JName {jni:"C", scala:"Char"   },
        _                                => stdlib(name)?,
    };
    Some(name)
}

/// Returns the standard library name of given name if it exists.
pub fn stdlib(name:&Name) -> Option<JName> {
    let name = match name.str.as_str() {
        "Object"                          => JName {jni:"Ljava/lang/Object;",   scala:"Object" },
        "Vec"                             => JName {jni:"Lscala/collection/mutable/ArrayBuffer;",       scala:"ArrayBuffer" },
        "Option"                          => JName {jni:"Lscala/Option;",       scala:"Option" },
        "Uuid"                            => JName {jni:"Ljava/util/UUID;",     scala:"UUID"   },
        "String"                          => JName {jni:"Ljava/lang/String;",   scala:"String" },
        _                                 => None?,
    };
    Some(name)
}
