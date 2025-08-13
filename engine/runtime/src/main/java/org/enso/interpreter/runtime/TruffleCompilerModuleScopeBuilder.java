package org.enso.interpreter.runtime;

import org.enso.compiler.context.CompilerContext;
import org.enso.interpreter.runtime.scope.ModuleScopeBuilder;

final class TruffleCompilerModuleScopeBuilder extends CompilerContext.ModuleScopeBuilder {
  private final org.enso.interpreter.runtime.scope.ModuleScopeBuilder scopeBuilder;

  TruffleCompilerModuleScopeBuilder(
      org.enso.interpreter.runtime.scope.ModuleScopeBuilder scopeBuilder) {
    this.scopeBuilder = scopeBuilder;
  }

  static ModuleScopeBuilder fromCompilerModuleScopeBuilder(
      CompilerContext.ModuleScopeBuilder scopeBuilder) {
    return ((TruffleCompilerModuleScopeBuilder) scopeBuilder).unsafeScopeBuilder();
  }

  static ModuleScopeBuilder fromCompilerModule(CompilerContext.Module module) {
    return fromCompilerModuleScopeBuilder(module.getScopeBuilder());
  }

  org.enso.interpreter.runtime.scope.ModuleScopeBuilder unsafeScopeBuilder() {
    return scopeBuilder;
  }

  final void finish() {
    this.scopeBuilder.finish();
  }
}
