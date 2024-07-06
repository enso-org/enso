package org.enso.compiler.pass.analyse.types.scope;

import org.enso.compiler.pass.analyse.types.TypeRepresentation;
import org.enso.pkg.QualifiedName;

/**
 * A reference to a Type-like scope that can have methods associated with it.
 *
 * <p>It can be one of three things: - An atom type - The eigentype of an atom type - this will hold
 * the type's static methods - An associated type of a module
 */
public final class TypeScopeReference {
  private final QualifiedName name;

  private final Kind kind;

  private TypeScopeReference(QualifiedName name, Kind kind) {
    this.name = name;
    this.kind = kind;
  }

  public static TypeScopeReference moduleAssociatedType(QualifiedName moduleName) {
    return new TypeScopeReference(moduleName, Kind.MODULE_ASSOCIATED_TYPE);
  }

  public static TypeScopeReference atomType(QualifiedName atomTypeName) {
    return new TypeScopeReference(atomTypeName, Kind.ATOM_TYPE);
  }

  public static TypeScopeReference atomEigenType(QualifiedName atomTypeName) {
    return new TypeScopeReference(atomTypeName, Kind.ATOM_EIGEN_TYPE);
  }

  public static TypeScopeReference atomType(QualifiedName atomTypeName, boolean staticCall) {
    return staticCall ? atomEigenType(atomTypeName) : atomType(atomTypeName);
  }

  @Override
  public int hashCode() {
    return name.hashCode() + kind.hashCode();
  }

  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof TypeScopeReference other)) {
      return false;
    }

    return name.equals(other.name) && kind.equals(other.kind);
  }

  enum Kind {
    MODULE_ASSOCIATED_TYPE,
    ATOM_TYPE,
    ATOM_EIGEN_TYPE
  }

  QualifiedName getName() {
    return name;
  }

  QualifiedName relatedModuleName() {
    switch (kind) {
      case MODULE_ASSOCIATED_TYPE:
        return name;
      case ATOM_TYPE:
      case ATOM_EIGEN_TYPE:
        var parent = name.getParent();
        assert parent.isDefined();
        return parent.get();
      default:
        throw new IllegalStateException("Unexpected value: " + kind);
    }
  }

  Kind getKind() {
    return kind;
  }

  @Override
  public String toString() {
    return switch (kind) {
      case MODULE_ASSOCIATED_TYPE -> "ModuleAssociatedType(" + name + ")";
      case ATOM_TYPE -> "AtomType(" + name + ")";
      case ATOM_EIGEN_TYPE -> "AtomEigenType(" + name + ")";
    };
  }

  public static TypeScopeReference ANY =
      TypeScopeReference.atomType(TypeRepresentation.ANY.getAssociatedType());
}
