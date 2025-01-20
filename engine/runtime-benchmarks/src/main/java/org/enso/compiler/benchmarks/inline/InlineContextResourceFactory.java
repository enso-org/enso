package org.enso.compiler.benchmarks.inline;

import org.enso.compiler.PackageRepository;
import org.enso.compiler.context.InlineContext;
import org.enso.interpreter.node.MethodRootNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.scope.ModuleScope;

public record InlineContextResourceFactory(
    ModuleScope moduleScope,
    Type assocTypeReceiver,
    EnsoContext ensoCtx,
    PackageRepository pkgRepository) {

  public InlineContext create() {
    var mainFunc = moduleScope.getMethodForType(assocTypeReceiver, "main");
    var mainFuncRootNode = (MethodRootNode) mainFunc.getCallTarget().getRootNode();
    var mainLocalScope = mainFuncRootNode.getLocalScope();
    return InlineContext.fromJava(
        mainLocalScope.createChild(),
        moduleScope.getModule().asCompilerModule(),
        scala.Option.apply(false),
        ensoCtx.getCompilerConfig(),
        scala.Option.apply(pkgRepository));
  }
}
