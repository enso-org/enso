package org.enso.compiler.pass.lint;

import java.util.List;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Function;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Warning;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.compiler.pass.MiniPassFactory;
import org.enso.compiler.pass.desugar.ComplexType$;
import org.enso.compiler.pass.desugar.GenerateMethodBodies$;
import org.enso.compiler.pass.desugar.LambdaShorthandToLambda$;
import org.enso.compiler.pass.desugar.NestedPatternMatch$;
import org.enso.compiler.pass.desugar.OperatorToFunction$;
import org.enso.compiler.pass.desugar.SectionsToBinOp;
import org.enso.compiler.pass.optimise.LambdaConsolidate$;
import org.enso.compiler.pass.resolve.IgnoredBindings$;
import org.enso.scala.wrapper.ScalaConversions;
import scala.collection.immutable.Seq;

public final class PrivateBuiltinMethodsCheck implements MiniPassFactory {
  public static final PrivateBuiltinMethodsCheck INSTANCE = new PrivateBuiltinMethodsCheck();

  private PrivateBuiltinMethodsCheck() {}

  @Override
  public MiniIRPass createForModuleCompilation(ModuleContext moduleContext) {
    if (moduleContext.module().isPrivate()) {
      return null;
    } else {
      return new Mini(moduleContext);
    }
  }

  @Override
  public MiniIRPass createForInlineCompilation(InlineContext inlineContext) {
    return null;
  }

  @Override
  public Seq<? extends IRProcessingPass> precursorPasses() {
    return ScalaConversions.asScala(
        List.of(
            ComplexType$.MODULE$,
            GenerateMethodBodies$.MODULE$,
            IgnoredBindings$.MODULE$,
            LambdaConsolidate$.MODULE$,
            LambdaShorthandToLambda$.MODULE$,
            NestedPatternMatch$.MODULE$,
            OperatorToFunction$.MODULE$,
            SectionsToBinOp.INSTANCE,
            UnusedBindings$.MODULE$));
  }

  @Override
  public Seq<? extends IRProcessingPass> invalidatedPasses() {
    return ScalaConversions.asScala(List.of());
  }

  private static class Mini extends MiniIRPass {

    public Mini(ModuleContext moduleContext) {}

    @Override
    public MiniIRPass prepare(IR parent, Expression child) {
      return null;
    }

    @Override
    public Expression transformExpression(Expression expr) {
      return expr;
    }

    @Override
    public Module transformModule(Module moduleIr) {
      for (var bind : ScalaConversions.asJava(moduleIr.bindings())) {
        switch (bind) {
          case Method.Explicit methodDef -> {
            var body =
                switch (methodDef.body()) {
                  case Function.Lambda lam -> lam.body();
                  case Expression expr -> expr;
                };
            if (UnusedBindings$.MODULE$.isBuiltinMethod(body)) {
              if (!methodDef.isPrivate()) {
                body.getDiagnostics()
                    .add(new Warning.NonPrivateBuiltinMethod(body.identifiedLocation()));
              }
            }
          }
          default -> {}
        }
      }
      return moduleIr;
    }
  }
}
