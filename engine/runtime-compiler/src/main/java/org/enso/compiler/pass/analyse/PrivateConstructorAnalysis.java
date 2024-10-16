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
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.IRProcessingPass;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Ensures that all type definitions have either all constructors public, or all constructors
 * private.
 */
public final class PrivateConstructorAnalysis implements IRPass {
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

  @Override
  public Module runModule(Module ir, ModuleContext moduleContext) {
    var newBindings =
        ir.bindings()
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
    return ir.copy(
        ir.copy$default$1(),
        ir.copy$default$2(),
        newBindings,
        ir.copy$default$4(),
        ir.copy$default$5(),
        ir.copy$default$6(),
        ir.copy$default$7(),
        ir.copy$default$8());
  }

  /** Not supported on a single expression. */
  @Override
  public Expression runExpression(Expression ir, InlineContext inlineContext) {
    return ir;
  }

  @Override
  public <T extends IR> T updateMetadataInDuplicate(T sourceIr, T copyOfIr) {
    return IRPass.super.updateMetadataInDuplicate(sourceIr, copyOfIr);
  }
}
