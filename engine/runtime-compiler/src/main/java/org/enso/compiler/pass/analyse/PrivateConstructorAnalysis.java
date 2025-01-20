package org.enso.compiler.pass.analyse;

import java.util.List;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.expression.errors.Syntax;
import org.enso.compiler.core.ir.expression.errors.Syntax.InconsistentConstructorVisibility$;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.compiler.pass.MiniPassFactory;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Ensures that all type definitions have either all constructors public, or all constructors
 * private.
 *
 * <p>Does not support inline compilation.
 */
public final class PrivateConstructorAnalysis implements MiniPassFactory {
  public static final PrivateConstructorAnalysis INSTANCE = new PrivateConstructorAnalysis();

  private PrivateConstructorAnalysis() {}

  @Override
  public Seq<IRProcessingPass> precursorPasses() {
    List<IRProcessingPass> passes = List.of(PrivateModuleAnalysis.INSTANCE);
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  @SuppressWarnings("unchecked")
  public Seq<IRProcessingPass> invalidatedPasses() {
    Object obj = scala.collection.immutable.Nil$.MODULE$;
    return (scala.collection.immutable.List<IRProcessingPass>) obj;
  }

  public MiniIRPass createForModuleCompilation(ModuleContext moduleContext) {
    return new Mini();
  }

  @Override
  public MiniIRPass createForInlineCompilation(InlineContext inlineContext) {
    return null;
  }

  private static final class Mini extends MiniIRPass {
    @Override
    public Expression transformExpression(Expression expr) {
      throw new IllegalStateException("Should not be called - prepare returns null");
    }

    @Override
    public MiniIRPass prepare(IR parent, Expression child) {
      return null;
    }

    @Override
    public Module transformModule(Module moduleIr) {
      var newBindings =
          moduleIr
              .bindings()
              .map(
                  binding -> {
                    if (binding instanceof Definition.Type type) {
                      var partitions = type.members().partition(Definition.Data::isPrivate);
                      var privateCtorsCnt = partitions._1.size();
                      var publicCtorsCnt = partitions._2.size();
                      var ctorsCnt = type.members().size();
                      if (!(privateCtorsCnt == ctorsCnt || publicCtorsCnt == ctorsCnt)) {
                        assert type.location().isDefined();
                        return new Syntax(
                            type.location().get(),
                            InconsistentConstructorVisibility$.MODULE$,
                            type.passData(),
                            type.diagnostics());
                      }
                    }
                    return binding;
                  });
      return moduleIr.copyWithBindings(newBindings);
    }
  }
}
