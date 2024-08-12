package org.enso.compiler.pass.analyse.types.scope;

import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.module.scope.Import;
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
}
