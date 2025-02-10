package org.enso.compiler.pass.optimise;

import java.util.ArrayList;
import java.util.stream.Stream;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.CompilerError;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.Pattern;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.core.ir.expression.warnings.Unreachable;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.compiler.pass.MiniPassFactory;
import org.enso.compiler.pass.analyse.AliasAnalysis$;
import org.enso.compiler.pass.analyse.DataflowAnalysis$;
import org.enso.compiler.pass.analyse.DemandAnalysis$;
import org.enso.compiler.pass.analyse.TailCall;
import org.enso.compiler.pass.desugar.ComplexType$;
import org.enso.compiler.pass.desugar.FunctionBinding$;
import org.enso.compiler.pass.desugar.GenerateMethodBodies$;
import org.enso.compiler.pass.desugar.LambdaShorthandToLambda$;
import org.enso.compiler.pass.desugar.NestedPatternMatch$;
import org.enso.compiler.pass.resolve.DocumentationComments$;
import org.enso.compiler.pass.resolve.IgnoredBindings$;
import org.enso.scala.wrapper.ScalaConversions;
import scala.collection.immutable.List;
import scala.jdk.javaapi.CollectionConverters;

/**
 * This pass discovers and optimizes away unreachable case branches.
 *
 * <p>It removes these unreachable expressions from the IR, and attaches a {@link
 * org.enso.compiler.core.ir.Warning} diagnostic to the case expression itself.
 *
 * <p>Currently, a branch is considered 'unreachable' by this pass if:
 *
 * <ul>
 *   <li>It occurs after a catch-all branch.
 * </ul>
 *
 * <p>In the future, this pass should be expanded to consider patterns that are entirely subsumed by
 * previous patterns in its definition of unreachable, but this requires doing sophisticated
 * coverage analysis, and hence should happen as part of the broader refactor of nested patterns
 * desugaring.
 *
 * <p>This pass requires no configuration.
 *
 * <p>This pass requires the context to provide:
 *
 * <ul>
 *   <li>Nothing
 * </ul>
 */
public final class UnreachableMatchBranches implements MiniPassFactory {
  private UnreachableMatchBranches() {}

  public static final UnreachableMatchBranches INSTANCE = new UnreachableMatchBranches();

  @Override
  public List<IRProcessingPass> precursorPasses() {
    java.util.List<IRProcessingPass> passes = new ArrayList<>();
    passes.add(ComplexType$.MODULE$);
    passes.add(DocumentationComments$.MODULE$);
    passes.add(FunctionBinding$.MODULE$);
    passes.add(GenerateMethodBodies$.MODULE$);
    passes.add(LambdaShorthandToLambda$.MODULE$);
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  public List<IRProcessingPass> invalidatedPasses() {
    java.util.List<IRProcessingPass> passes = new ArrayList<>();
    passes.add(AliasAnalysis$.MODULE$);
    passes.add(DataflowAnalysis$.MODULE$);
    passes.add(DemandAnalysis$.MODULE$);
    passes.add(IgnoredBindings$.MODULE$);
    passes.add(NestedPatternMatch$.MODULE$);
    passes.add(TailCall.INSTANCE);
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  public MiniIRPass createForInlineCompilation(InlineContext inlineContext) {
    return new Mini();
  }

  @Override
  public MiniIRPass createForModuleCompilation(ModuleContext moduleContext) {
    return new Mini();
  }

  private static class Mini extends MiniIRPass {
    @Override
    public Expression transformExpression(Expression expr) {
      return switch (expr) {
        case Case cse -> optimizeCase(cse);
        default -> expr;
      };
    }

    /**
     * Optimizes a case expression by removing unreachable branches.
     *
     * <p>Additionally, it will attach a warning about unreachable branches to the case expression.
     *
     * @param cse the case expression to optimize
     * @return `cse` with unreachable branches removed
     */
    private Case optimizeCase(Case cse) {
      if (cse instanceof Case.Expr expr) {
        var branches = CollectionConverters.asJava(expr.branches());
        var reachableNonCatchAllBranches =
            branches.stream().takeWhile(branch -> !isCatchAll(branch));
        var firstCatchAll = branches.stream().filter(this::isCatchAll).findFirst();
        var unreachableBranches =
            branches.stream().dropWhile(branch -> !isCatchAll(branch)).skip(1).toList();
        List<Case.Branch> reachableBranches;
        if (firstCatchAll.isPresent()) {
          reachableBranches = appended(reachableNonCatchAllBranches, firstCatchAll.get());
        } else {
          reachableBranches = ScalaConversions.nil();
        }

        if (unreachableBranches.isEmpty()) {
          return expr;
        } else {
          var firstUnreachableWithLoc =
              unreachableBranches.stream()
                  .filter(branch -> branch.identifiedLocation() != null)
                  .findFirst();
          var lastUnreachableWithLoc =
              unreachableBranches.stream()
                  .filter(branch -> branch.identifiedLocation() != null)
                  .reduce((first, second) -> second);
          IdentifiedLocation unreachableLocation = null;
          if (firstUnreachableWithLoc.isPresent() && lastUnreachableWithLoc.isPresent()) {
            unreachableLocation =
                new IdentifiedLocation(
                    firstUnreachableWithLoc.get().location().get().start(),
                    lastUnreachableWithLoc.get().location().get().end(),
                    firstUnreachableWithLoc.get().id());
          }

          var diagnostic = new Unreachable.Branches(unreachableLocation);
          var copiedExpr = expr.copyWithBranches(reachableBranches);
          copiedExpr.getDiagnostics().add(diagnostic);
          return copiedExpr;
        }
      } else {
        throw new CompilerError("Unexpected case branch.");
      }
    }

    /**
     * Determines if a branch is a catch all branch.
     *
     * @param branch the branch to check
     * @return `true` if `branch` is catch-all, otherwise `false`
     */
    private boolean isCatchAll(Case.Branch branch) {
      return switch (branch.pattern()) {
        case Pattern.Name ignored -> true;
        default -> false;
      };
    }

    private static List<Case.Branch> appended(Stream<Case.Branch> branches, Case.Branch branch) {
      var ret = new ArrayList<>(branches.toList());
      ret.add(branch);
      return CollectionConverters.asScala(ret).toList();
    }
  }
}
