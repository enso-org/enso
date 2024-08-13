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
    // We need special handling for Any, as if the Standard.Base.Any.Any is not imported, we need to 'redirect' into Standard.Builtins.Main. This is the only exception - other builtins will only show up if they were imported, only Any is special because it is the top type that can show up implicitly.
    if (typeScopeReference.equals(TypeScopeReference.ANY)) {
      return findModuleForAny();
    }

    return findModule(typeScopeReference.relatedModuleName());
  }

  private Module findModuleForAny() {
    var importedModuleWithShadowDefinitions = findModule(TypeScopeReference.ANY.relatedModuleName());
    if (importedModuleWithShadowDefinitions != null) {
      return importedModuleWithShadowDefinitions;
    }

    // Until Standard.Base.Any.Any is imported, we are relying on Standard.Builtins.Main, however, in the IR it has no methods defined.
    // We could rely on Builtins reading metadata - but currently that metadata does not contain type info _anyway_ so it is of little use to us.
    // Given that we need this only for Any, we currently hardcode the types of the 5 Any builtins. In the future we maybe want to make this part of the metadata.
    var fallbackBuiltinModule = findModule(QualifiedName.fromString("Standard.Builtins.Main"));
    assert fallbackBuiltinModule != null : "The fallback builtin module should always be present.";
    return fallbackBuiltinModule;
  }
}
