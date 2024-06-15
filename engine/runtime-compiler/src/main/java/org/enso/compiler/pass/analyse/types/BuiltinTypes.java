package org.enso.compiler.pass.analyse.types;

import org.enso.pkg.QualifiedName;
import org.enso.pkg.QualifiedName$;

/** A helper class providing the builtin types. */
public class BuiltinTypes {
  // TODO in next iterations we will want to resolve descriptions of these types based on the loaded
  // std-lib (from PackageRepository, if available). Note that if the std-lib is not imported,
  // some builtin types have different names - this should be handled here in some sane way.

  public final TypeRepresentation INTEGER = fromQualifiedName("Standard.Base.Data.Numbers.Integer");
  public final TypeRepresentation FLOAT = fromQualifiedName("Standard.Base.Data.Numbers.Float");

  public final TypeRepresentation NUMBER = fromQualifiedName("Standard.Base.Data.Numbers.Number");
  public final TypeRepresentation TEXT = fromQualifiedName("Standard.Base.Data.Text.Text");
  public final TypeRepresentation VECTOR = fromQualifiedName("Standard.Base.Data.Vector.Vector");
  public final TypeRepresentation NOTHING = fromQualifiedName("Standard.Base.Nothing.Nothing");

  private TypeRepresentation fromQualifiedName(String qualifiedName) {
    var fqn = QualifiedName$.MODULE$.fromString(qualifiedName);
    return new TypeRepresentation.AtomType(fqn, null);
  }

  public static boolean isAny(QualifiedName qualifiedName) {
    var str = qualifiedName.toString();
    return str.equals(anyQualifiedName) || str.equals("Standard.Base.Any");
  }

  public static boolean isFunction(QualifiedName qualifiedName) {
    var str = qualifiedName.toString();
    return str.equals("Standard.Base.Function.Function");
  }

  static final String anyQualifiedName = "Standard.Base.Any.Any";
}
