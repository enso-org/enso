package org.enso.compiler.pass.analyse.types;

import org.enso.pkg.QualifiedName;
import org.enso.pkg.QualifiedName$;

/** A helper class providing the builtin types. */
public final class BuiltinTypes {
  private BuiltinTypes() {}

  public static final String FQN_NUMBER = "Standard.Base.Data.Numbers.Number";
  public static final TypeRepresentation NUMBER = fromQualifiedName(FQN_NUMBER);
  static final String FQN_ANY = "Standard.Base.Any.Any";
  public static final TypeRepresentation TEXT = fromQualifiedName("Standard.Base.Data.Text.Text");
  public static final TypeRepresentation BOOLEAN =
      fromQualifiedName("Standard.Base.Data.Boolean.Boolean");
  public static final TypeRepresentation VECTOR =
      fromQualifiedName("Standard.Base.Data.Vector.Vector");
  public static final TypeRepresentation NOTHING =
      fromQualifiedName("Standard.Base.Nothing.Nothing");

  private static TypeRepresentation fromQualifiedName(String qualifiedName) {
    var fqn = QualifiedName$.MODULE$.fromString(qualifiedName);
    return new TypeRepresentation.AtomType(fqn);
  }

  static final String FQN_FUNCTION = "Standard.Base.Function.Function";
  private static final String FQN_INTEGER = "Standard.Base.Data.Numbers.Integer";
  public static final TypeRepresentation INTEGER = fromQualifiedName(FQN_INTEGER);
  private static final String FQN_FLOAT = "Standard.Base.Data.Numbers.Float";
  public static final TypeRepresentation FLOAT = fromQualifiedName(FQN_FLOAT);

  public static boolean isAny(QualifiedName qualifiedName) {
    var str = qualifiedName.toString();
    return str.equals(FQN_ANY);
  }

  public static boolean isFunction(QualifiedName qualifiedName) {
    var str = qualifiedName.toString();
    return str.equals(FQN_FUNCTION);
  }

  public static boolean isInteger(QualifiedName qualifiedName) {
    return qualifiedName.toString().equals(FQN_INTEGER);
  }

  public static boolean isFloat(QualifiedName qualifiedName) {
    return qualifiedName.toString().equals(FQN_FLOAT);
  }
}
