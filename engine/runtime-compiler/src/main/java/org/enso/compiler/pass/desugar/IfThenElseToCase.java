package org.enso.compiler.pass.desugar;

import java.util.List;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.ir.Empty;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Literal;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Pattern;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.core.ir.expression.IfThenElse;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.compiler.pass.MiniPassFactory;
import org.enso.compiler.pass.analyse.DemandAnalysis$;
import scala.Option;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

public final class IfThenElseToCase implements MiniPassFactory {

  public static final IfThenElseToCase INSTANCE = new IfThenElseToCase();

  private IfThenElseToCase() {}

  @Override
  public Seq<IRProcessingPass> precursorPasses() {
    List<IRProcessingPass> passes = List.of(GenerateMethodBodies$.MODULE$);
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  public Seq<IRProcessingPass> invalidatedPasses() {
    List<IRProcessingPass> passes = List.of(DemandAnalysis$.MODULE$);
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  public MiniIRPass createForModuleCompilation(ModuleContext moduleContext) {
    var ctx = InlineContext.fromModuleContext(moduleContext);
    return new Mini(ctx);
  }

  @Override
  public MiniIRPass createForInlineCompilation(InlineContext inlineContext) {
    return new Mini(inlineContext);
  }

  private static final class Mini extends MiniIRPass {

    private final InlineContext ctx;

    private Mini(InlineContext ctx) {
      this.ctx = ctx;
    }

    @Override
    public Expression transformExpression(Expression ir) {
      return switch (ir) {
        case IfThenElse expr -> {
          var condArg = expr.cond();
          var trueAt = expr.trueBranch().identifiedLocation();
          var falseAt = orNull(expr.falseBranch().map(b -> b.identifiedLocation()));
          var truePattern =
              new Pattern.Literal(new Literal.Bool(true, null, meta()), trueAt, meta());
          var trueBranch = new Case.Branch(truePattern, expr.trueBranch(), true, trueAt, meta());
          var falseCode =
              expr.falseBranch().nonEmpty() ? expr.falseBranch().get() : new Empty(null, meta());
          var falsePattern = new Pattern.Name(new Name.Blank(null, meta()), falseAt, meta());
          var falseBranch = new Case.Branch(falsePattern, falseCode, true, falseAt, meta());
          var branches = join(trueBranch, join(falseBranch, nil()));
          var caseExpr = new Case.Expr(condArg, branches, false, expr.identifiedLocation(), meta());
          yield caseExpr;
        }
        default -> ir;
      };
    }

    @SuppressWarnings("unchecked")
    private static final <T> scala.collection.immutable.List<T> nil() {
      return (scala.collection.immutable.List<T>) scala.collection.immutable.Nil$.MODULE$;
    }

    private static final <T> scala.collection.immutable.List<T> join(
        T head, scala.collection.immutable.List<T> tail) {
      return scala.collection.immutable.$colon$colon$.MODULE$.apply(head, tail);
    }

    private static <T> T orNull(Option<T> opt) {
      return opt.isEmpty() ? null : opt.get();
    }

    private static MetadataStorage meta() {
      return new MetadataStorage();
    }
  }
}
