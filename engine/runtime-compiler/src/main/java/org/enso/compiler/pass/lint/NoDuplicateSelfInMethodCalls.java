package org.enso.compiler.pass.lint;

import java.util.List;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.ConstantsNames;
import org.enso.compiler.core.ir.CallArgument;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.expression.errors.Redefined;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.compiler.pass.MiniPassFactory;
import org.enso.compiler.pass.desugar.GenerateMethodBodies$;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

/** This pass ensures that there are no method calls with two `self` named arguments. */
public final class NoDuplicateSelfInMethodCalls implements MiniPassFactory {
  public static final NoDuplicateSelfInMethodCalls INSTANCE = new NoDuplicateSelfInMethodCalls();
  private static final Mini MINI_INSTANCE = new Mini();

  @Override
  public Seq<? extends IRProcessingPass> precursorPasses() {
    java.util.List<IRProcessingPass> list = java.util.List.of(GenerateMethodBodies$.MODULE$);
    return CollectionConverters.asScala(list).toList();
  }

  @Override
  public Seq<? extends IRProcessingPass> invalidatedPasses() {
    List<IRProcessingPass> empty = List.of();
    return CollectionConverters.asScala(empty).toList();
  }

  @Override
  public MiniIRPass createForModuleCompilation(ModuleContext moduleContext) {
    return MINI_INSTANCE;
  }

  @Override
  public MiniIRPass createForInlineCompilation(InlineContext inlineContext) {
    return MINI_INSTANCE;
  }

  private static final class Mini extends MiniIRPass {

    @Override
    public Expression transformExpression(Expression expr) {
      if (expr instanceof Application.Prefix application) {
        var namedSelfArguments = application.arguments().filter(Mini::isNamedSelfArgument);
        if (namedSelfArguments.length() > 1) {
          return new Redefined.SelfArg(application.identifiedLocation(), application.passData());
        }
      }
      return expr;
    }

    private static boolean isNamedSelfArgument(CallArgument callArg) {
      return callArg.name().isDefined()
          && callArg.name().get().name().equals(ConstantsNames.SELF_ARGUMENT);
    }
  }
}
