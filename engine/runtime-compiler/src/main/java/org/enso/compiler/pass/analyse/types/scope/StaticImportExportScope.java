package org.enso.compiler.pass.analyse.types.scope;

import org.enso.compiler.common.MethodResolutionAlgorithm;
import org.enso.compiler.pass.analyse.types.TypeRepresentation;
import org.enso.pkg.QualifiedName;

/** The static counterpart of {@link org.enso.interpreter.runtime.scope.ImportExportScope}. */
public final class StaticImportExportScope {
  // TODO add support for only/hiding once https://github.com/enso-org/enso/issues/10796 is fixed
  private final QualifiedName referredModuleName;

  public StaticImportExportScope(QualifiedName referredModuleName) {
    this.referredModuleName = referredModuleName;
  }

  // This field should not be serialized.
  private Resolved cachedResolvedScope = null;

  public Resolved resolve(
      ModuleResolver moduleResolver, StaticMethodResolution methodResolutionAlgorithm) {
    if (cachedResolvedScope != null) {
      return cachedResolvedScope;
    }

    var module = moduleResolver.findModule(referredModuleName);
    if (module == null) {
      throw new IllegalStateException("Could not find module: " + referredModuleName);
    }
    var moduleScope = StaticModuleScope.forIR(module);
    var resolved = new Resolved(moduleScope, methodResolutionAlgorithm);
    cachedResolvedScope = resolved;
    return resolved;
  }

  /**
   * The resolved version of the import/export scope.
   *
   * <p>The qualified name is replaced with the actual reference to the referred scope.
   *
   * <p>This value should not be present in the metadata as it is not suitable for serialization. It
   * should be constructed ad-hoc whenever needed.
   */
  public static class Resolved {
    private final StaticModuleScope referredModuleScope;
    private final MethodResolutionAlgorithm<
            TypeRepresentation, TypeScopeReference, StaticImportExportScope, StaticModuleScope>
        methodResolutionAlgorithm;

    private Resolved(
        StaticModuleScope moduleScope,
        MethodResolutionAlgorithm<
                TypeRepresentation, TypeScopeReference, StaticImportExportScope, StaticModuleScope>
            methodResolutionAlgorithm) {
      this.referredModuleScope = moduleScope;
      this.methodResolutionAlgorithm = methodResolutionAlgorithm;
    }

    public TypeRepresentation getMethodForType(TypeScopeReference type, String name) {
      // TODO filtering only/hiding (see above) - for now we just return everything
      return referredModuleScope.getMethodForType(type, name);
    }

    public TypeRepresentation getExportedMethod(TypeScopeReference type, String name) {
      // TODO filtering only/hiding (see above) - for now we just return everything
      return methodResolutionAlgorithm.getExportedMethod(referredModuleScope, type, name);
    }
  }

  public QualifiedName getReferredModuleName() {
    return referredModuleName;
  }

  @Override
  public String toString() {
    return "StaticImportExportScope{" + referredModuleName + "}";
  }

  @Override
  public int hashCode() {
    return referredModuleName.hashCode();
  }

  @Override
  public boolean equals(Object obj) {
    if (!(obj instanceof StaticImportExportScope other)) {
      return false;
    }

    // TODO once hiding (see above) is added, these filters need to be added too
    return referredModuleName.equals(other.referredModuleName);
  }
}
