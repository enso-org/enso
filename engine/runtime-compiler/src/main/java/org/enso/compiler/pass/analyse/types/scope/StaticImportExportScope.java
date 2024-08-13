package org.enso.compiler.pass.analyse.types.scope;

import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.pass.analyse.types.TypeRepresentation;
import org.enso.pkg.QualifiedName;

public class StaticImportExportScope {
  // TODO add support for only/hiding once https://github.com/enso-org/enso/issues/10796 is fixed
  private final QualifiedName referredModuleName;

  private StaticImportExportScope(QualifiedName referredModuleName) {
    this.referredModuleName = referredModuleName;
  }

  public static StaticImportExportScope buildFrom(Import.Module importModule) {
    QualifiedName moduleName = QualifiedName.fromParts(importModule.name().parts().map(Name::name));
    return new StaticImportExportScope(moduleName);
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
    var materialized = new MaterializedImportExportScope(moduleScope);
    cachedMaterializedScope = materialized;
    return materialized;
  }

  // I'm not yet sure about this, but for PoC
  public class MaterializedImportExportScope {
    private final StaticModuleScope referredModuleScope;

    private MaterializedImportExportScope(StaticModuleScope moduleScope) {
      this.referredModuleScope = moduleScope;
    }

    public TypeRepresentation getMethodForType(TypeScopeReference type, String name) {
      // TODO filtering only/hiding - for now we just return everything
      return referredModuleScope.getMethodForType(type, name);
    }
  }
}
