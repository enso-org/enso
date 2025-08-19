package org.enso.interpreter.runtime;

import java.util.function.Consumer;
import org.enso.compiler.context.CompilerContext;
import org.enso.interpreter.runtime.scope.ModuleScope;

final class TruffleCompilerModuleScopeBuilder extends ModuleScopeBuilder {
  TruffleCompilerModuleScopeBuilder(Module module, Consumer<ModuleScope> onFinish) {
    super(module, onFinish);
  }

  static ModuleScopeBuilder fromCompilerModuleScopeBuilder(
      CompilerContext.ModuleScopeBuilder scopeBuilder) {
    return (TruffleCompilerModuleScopeBuilder) scopeBuilder;
  }

  static ModuleScopeBuilder fromCompilerModule(CompilerContext.Module module) {
    return fromCompilerModuleScopeBuilder(module.getScopeBuilder());
  }
}
