//! Various Java Native Interface utilities.

#![allow(missing_debug_implementations)]

use super::ast::Name;
use super::ast::Type;



// =====================
// === JNI Utilities ===
// =====================

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
       "Box"                             => JName {jni:"",  scala:""       },
       // PRIMITIVES (https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/types.html)
       "usize" | "isize" | "u64" | "i64" => JName {jni:"L", scala:"Long"   },
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

pub fn stdlib(name:&Name) -> Option<JName> {
    let name = match name.str.as_str() {
        "Object"                          => JName {jni:"java/lang/Object",   scala:"Object" },
        "Vec"                             => JName {jni:"scala/Vector",       scala:"Vector" },
        "Option"                          => JName {jni:"scala/Option",       scala:"Option" },
        "Uuid"                            => JName {jni:"java/util/UUID",     scala:"UUID"   },
        "String"                          => JName {jni:"java/lang/String",   scala:"String" },
        _                                 => None?,
    };
    Some(name)
}
