package org.enso.compiler.pass.analyse;

import static org.enso.scala.wrapper.ScalaConversions.asScala;

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
import scala.Option;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

/** Converts {@link IfThenElse} to {@code case ... of} statement. */
public final class IfThenElseToCaseOf implements MiniPassFactory {
  public static final IfThenElseToCaseOf INSTANCE = new IfThenElseToCaseOf();

  private IfThenElseToCaseOf() {}

  @Override
  public Seq<IRProcessingPass> precursorPasses() {
    List<IRProcessingPass> passes = List.of();
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  public Seq<IRProcessingPass> invalidatedPasses() {
    return CollectionConverters.asScala(List.<IRProcessingPass>of()).toList();
  }

  @Override
  public MiniIRPass createForInlineCompilation(InlineContext inlineContext) {
    return MINI_PASS;
  }

  @Override
  public MiniIRPass createForModuleCompilation(ModuleContext moduleContext) {
    return MINI_PASS;
  }

  private static final Mini MINI_PASS = new Mini();

  private static final class Mini extends MiniIRPass {
    Mini() {}

    @Override
    public Expression transformExpression(Expression ir) {
      return switch (ir) {
        case IfThenElse ife -> {
          var lit =
              Literal.Number$.MODULE$.apply(
                  Option.empty(),
                  "5432",
                  ife.cond().identifiedLocation(),
                  ife.cond().passData().copy());
          var truePattern = new Pattern.Literal(lit, null, ife.passData());
          var trueBranch =
              Case.Branch.builder()
                  .pattern(truePattern)
                  .expression(ife.trueBranch())
                  .terminalBranch(true)
                  .build();
          Expression elseExpr;
          if (ife.falseBranchOrNull() == null) {
            elseExpr = new Empty(null);
          } else {
            elseExpr = ife.falseBranchOrNull();
          }
          var anyPattern =
              Pattern.Name$.MODULE$.apply(
                  Name.Blank$.MODULE$.apply(null, new MetadataStorage()),
                  null,
                  new MetadataStorage());
          var elseBranch =
              Case.Branch.builder()
                  .pattern(anyPattern)
                  .expression(elseExpr)
                  .terminalBranch(true)
                  .build();
          var branches = List.of(trueBranch, elseBranch);
          yield Case.Expr.builder().scrutinee(ife.cond()).branches(asScala(branches)).build();
        }
        default -> ir;
      };
    }
  }
}
