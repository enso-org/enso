package org.enso.compiler.pass.analyse.types.scope;

import org.enso.compiler.pass.analyse.types.BuiltinTypes;
import org.enso.pkg.QualifiedName;

/**
 * A helper that encapsulates the hierarchy of types.
 *
 * <ul>
 *   <li>Module types and Any, have no parents.
 *   <li>The types Integer and Float have Number as their parent.
 *   <li>Any other type has Any as its parent.
 * </ul>
 *
 * This should be aligned with Type.allTypes in the interpreter.
 */
public class TypeHierarchy {
  private TypeHierarchy() {}

  public static TypeScopeReference getParent(TypeScopeReference type) {
    switch (type.getKind()) {
      case MODULE_ASSOCIATED_TYPE:
        return null;
      case ATOM_TYPE:
        var name = type.getName();
        if (BuiltinTypes.isAny(name)) {
          // Any has no more parents
          return null;
        }

        if (BuiltinTypes.isInteger(name) || BuiltinTypes.isFloat(name)) {
          return TypeScopeReference.atomType(QualifiedName.fromString(BuiltinTypes.FQN_NUMBER));
        }

        return TypeScopeReference.ANY;
      case ATOM_EIGEN_TYPE:
        return TypeScopeReference.ANY;
      default:
        throw new RuntimeException("Unknown type kind: " + type.getKind());
    }
  }
}
