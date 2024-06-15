package org.enso.compiler.pass.analyse.types.scope;

import org.enso.pkg.QualifiedName;

/**
 * A reference to a Type-like scope that can have methods associated with it.
 * <p>
 * It can be one of three things:
 * - An atom type
 * - The eigentype of an atom type - this will hold the type's static methods
 * - An associated type of a module
 */
public final class TypeScopeReference {
  private final QualifiedName name;
  private TypeScopeReference(QualifiedName name) {
    this.name = name;
  }

  public static TypeScopeReference moduleAssociatedType(QualifiedName moduleName) {
    return new TypeScopeReference(moduleName);
  }

  public static TypeScopeReference atomType(QualifiedName atomTypeName) {
    return new TypeScopeReference(atomTypeName);
  }

  public static TypeScopeReference atomEigenType(QualifiedName atomTypeName) {
    return new TypeScopeReference(atomTypeName.createChild("type"));
  }

  public static TypeScopeReference atomType(QualifiedName atomTypeName, boolean staticCall) {
    return staticCall ? atomEigenType(atomTypeName) : atomType(atomTypeName);
  }

  @Override
  public int hashCode() {
    return name.hashCode();
  }

  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof TypeScopeReference other)) {
      return false;
    }

    return name.equals(other.name);
  }
}
