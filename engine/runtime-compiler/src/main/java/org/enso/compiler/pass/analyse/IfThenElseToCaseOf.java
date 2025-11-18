package org.enso.compiler.pass.analyse;

import static org.enso.scala.wrapper.ScalaConversions.asScala;

import java.util.List;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.ir.Empty;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Pattern;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.core.ir.expression.IfThenElse;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.compiler.pass.MiniPassFactory;
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
          var truePattern = new Pattern.Bool(true, null, ife.passData());
          var trueBranch =
              Case.Branch.builder()
                  .pattern(truePattern)
                  .expression(ife.trueBranch())
                  .location(ife.trueBranch().identifiedLocation())
                  .terminalBranch(true)
                  .build();
          Expression elseExpr;
          if (ife.falseBranchOrNull() == null) {
            elseExpr = new Empty(null);
          } else {
            elseExpr = ife.falseBranchOrNull();
          }
          var falsePattern = new Pattern.Bool(false, null, new MetadataStorage());
          var elseBranch =
              Case.Branch.builder()
                  .pattern(falsePattern)
                  .expression(elseExpr)
                  .location(elseExpr.identifiedLocation())
                  .terminalBranch(true)
                  .build();
          var branches = List.of(trueBranch, elseBranch);
          yield Case.Expr.builder()
              .scrutinee(ife.cond())
              .location(ife.identifiedLocation())
              .branches(asScala(branches))
              .build();
        }
        default -> ir;
      };
    }
  }
}
