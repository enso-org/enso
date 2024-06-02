package org.enso.interpreter.runtime.scope;

import org.enso.compiler.context.CompilerContext;

public class TruffleCompilerModuleScopeBuilder extends CompilerContext.ModuleScopeBuilder {
  private final org.enso.interpreter.runtime.scope.ModuleScope.Builder scopeBuilder;

  public TruffleCompilerModuleScopeBuilder(
      org.enso.interpreter.runtime.scope.ModuleScope.Builder scopeBuilder) {
    this.scopeBuilder = scopeBuilder;
  }

  public final org.enso.interpreter.runtime.scope.ModuleScope.Builder unsafeScopeBuilder() {
    return scopeBuilder;
  }
}
