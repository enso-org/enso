package org.enso.compiler.pass.analyse.types;

import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.pass.analyse.types.scope.TypeScopeReference;
import org.enso.pkg.QualifiedName;

public class ModuleResolver {
  private final CompilerContext context;

  public ModuleResolver(CompilerContext context) {
    this.context = context;
  }

  Module findModule(QualifiedName name) {
    return context.findTopScopeModule(name.toString()).getIr();
  }

  Module findContainingModule(TypeScopeReference typeScopeReference) {

  }
}
