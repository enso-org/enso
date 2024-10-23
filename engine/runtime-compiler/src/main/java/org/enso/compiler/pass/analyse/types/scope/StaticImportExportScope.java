package org.enso.compiler.pass.analyse.types.scope;

import org.enso.compiler.pass.analyse.types.TypeRepresentation;
import org.enso.pkg.QualifiedName;

/** The static counterpart of {@link org.enso.interpreter.runtime.scope.ImportExportScope}. */
public class StaticImportExportScope {
  // TODO add support for only/hiding once https://github.com/enso-org/enso/issues/10796 is fixed
  private final QualifiedName referredModuleName;

  public StaticImportExportScope(QualifiedName referredModuleName) {
    this.referredModuleName = referredModuleName;
  }

  private transient MaterializedImportExportScope cachedMaterializedScope = null;

  public MaterializedImportExportScope materialize(ModuleResolver moduleResolver) {
    if (cachedMaterializedScope != null) {
      return cachedMaterializedScope;
    }

    var module = moduleResolver.findModule(referredModuleName);
    if (module == null) {
      throw new IllegalStateException("Could not find module: " + referredModuleName);
    }
    var moduleScope = StaticModuleScope.forIR(module);
    var materialized = new MaterializedImportExportScope(moduleScope, moduleResolver);
    cachedMaterializedScope = materialized;
    return materialized;
  }

  // I'm not yet sure about this, but for PoC
  public static class MaterializedImportExportScope {
    private final StaticModuleScope referredModuleScope;
    private final ModuleResolver moduleResolver;

    private MaterializedImportExportScope(
        StaticModuleScope moduleScope, ModuleResolver moduleResolver) {
      this.referredModuleScope = moduleScope;
      this.moduleResolver = moduleResolver;
    }

    public TypeRepresentation getMethodForType(TypeScopeReference type, String name) {
      // TODO filtering only/hiding - for now we just return everything
      return referredModuleScope.getMethodForType(type, name);
    }

    public TypeRepresentation getExportedMethod(TypeScopeReference type, String name) {
      // TODO filtering only/hiding - for now we just return everything
      return referredModuleScope.getExportedMethod(type, name, moduleResolver);
    }
  }

  public QualifiedName getReferredModuleName() {
    return referredModuleName;
  }
}
