package org.enso.compiler.pass.analyse.types.scope;

import org.enso.compiler.PackageRepository;
import org.enso.compiler.core.ir.Module;
import org.enso.pkg.QualifiedName;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ModuleResolver {
  // TODO I think the encapsulation would be better if we were taking CompilerContext here
  // but currently I did not find a way for an IR pass to get an instance of CompilerContext, so we
  // go directly to PackageRepository
  private final PackageRepository packageRepository;
  private final Logger logger = LoggerFactory.getLogger(ModuleResolver.class);

  public ModuleResolver(PackageRepository packageRepository) {
    this.packageRepository = packageRepository;
  }

  public Module findModule(QualifiedName name) {
    if (packageRepository == null) {
      logger.error("Failed to resolve module {} because package repository was null.", name);
      return null;
    }

    var compilerModuleOpt = packageRepository.getLoadedModule(name.toString());
    if (compilerModuleOpt.isEmpty()) {
      return null;
    }

    var moduleIr = compilerModuleOpt.get().getIr();
    assert moduleIr != null : "Once a module is found, its IR should be present.";
    return moduleIr;
  }

  public Module findContainingModule(TypeScopeReference typeScopeReference) {
    return findModule(typeScopeReference.relatedModuleName());
  }
}
