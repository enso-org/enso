package org.enso.interpreter.dsl.builtins;

import javax.annotation.processing.ProcessingEnvironment;
import javax.lang.model.type.TypeMirror;

/**
 * TypeWithKind provides a convenience wrapper for the types that can be encountered in builtins
 * construction.
 *
 * <p>For example: - Java's `Foo[]` type is of kind `Array` and base type `Foo` - `void` return type
 * is of kind `Void` and base type `Nothing` - all other accepted types are of kind `Object`
 */
public record TypeWithKind(String baseType, TypeKind kind) {
  static TypeWithKind createFromTpe(String tpeName) {
    if (tpeName.equals("void")) {
      return new TypeWithKind("Nothing", TypeKind.VOID);
    } else if (tpeName.endsWith("[]")) {
      int idx = tpeName.indexOf("[");
      return new TypeWithKind(tpeName.substring(0, idx), TypeKind.ARRAY);
    } else {
      return new TypeWithKind(tpeName, TypeKind.OBJECT);
    }
  }

  boolean isValidGuestType(ProcessingEnvironment env) {
    var typeMirror =
        switch (baseType) {
          case "long" -> env.getTypeUtils().getPrimitiveType(javax.lang.model.type.TypeKind.LONG);
          case "double" -> env.getTypeUtils()
              .getPrimitiveType(javax.lang.model.type.TypeKind.DOUBLE);
          case "boolean" -> env.getTypeUtils()
              .getPrimitiveType(javax.lang.model.type.TypeKind.BOOLEAN);
          default -> {
            var less = baseType.indexOf('<');
            var erased = less == -1 ? baseType : baseType.substring(0, less);
            var elem = env.getElementUtils().getTypeElement(erased);
            if (elem == null) {
              throw new NullPointerException("Cannot find " + baseType + " type");
            }
            yield elem.asType();
          }
        };
    return isValidGuestType(env, typeMirror);
  }

  public static boolean isValidGuestType(ProcessingEnvironment processingEnv, TypeMirror type) {
    return switch (type.getKind()) {
      case BOOLEAN, LONG, DOUBLE -> true;
      case VOID -> true;
      case DECLARED -> isTruffleObject(processingEnv, type);
      default -> false;
    };
  }

  static boolean isTruffleObject(ProcessingEnvironment processingEnv, TypeMirror type) {
    var truffleObject =
        processingEnv
            .getElementUtils()
            .getTypeElement("com.oracle.truffle.api.interop.TruffleObject");
    var isSubclass = processingEnv.getTypeUtils().isSubtype(type, truffleObject.asType());
    return isSubclass;
  }
}
