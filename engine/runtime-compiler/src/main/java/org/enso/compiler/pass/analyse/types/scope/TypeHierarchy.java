package org.enso.compiler.pass.analyse.types.scope;

import org.enso.compiler.pass.analyse.types.BuiltinTypes;
import org.enso.pkg.QualifiedName;

public class TypeHierarchy {
  public TypeScopeReference getParent(TypeScopeReference type) {
    switch (type.getKind()) {
      case MODULE_ASSOCIATED_TYPE:
        return null;
      case ATOM_TYPE:
        // TODO probably we will want to do this better?
        var name = type.getName();
        if (BuiltinTypes.isAny(name)) {
          // Any has no more parents
          return null;
        }

        if (BuiltinTypes.isInteger(name) || BuiltinTypes.isFloat(name)) {
          return TypeScopeReference.atomType(
              QualifiedName.fromString(BuiltinTypes.numberQualifiedName));
        }

        return TypeScopeReference.ANY;
      case ATOM_EIGEN_TYPE:
        return TypeScopeReference.ANY;
      default:
        throw new RuntimeException("Unknown type kind: " + type.getKind());
    }
  }
}
