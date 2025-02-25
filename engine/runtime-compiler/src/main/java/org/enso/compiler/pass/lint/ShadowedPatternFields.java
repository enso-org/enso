package org.enso.compiler.pass.lint;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.CompilerError;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.MetadataStorage;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Pattern;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.core.ir.expression.Case.Branch;
import org.enso.compiler.core.ir.expression.warnings.Shadowed.PatternBinding;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.MiniIRPass;
import org.enso.compiler.pass.MiniPassFactory;
import org.enso.compiler.pass.analyse.AliasAnalysis$;
import org.enso.compiler.pass.analyse.DataflowAnalysis$;
import org.enso.compiler.pass.analyse.DemandAnalysis$;
import org.enso.compiler.pass.analyse.TailCall;
import org.enso.compiler.pass.desugar.GenerateMethodBodies$;
import org.enso.compiler.pass.desugar.NestedPatternMatch$;
import org.enso.compiler.pass.resolve.IgnoredBindings$;
import scala.collection.immutable.List;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

/**
 * This pass detects and renames shadowed pattern fields.
 *
 * <p>This is necessary both in order to create a warning, but also to ensure that alias analysis
 * doesn't get confused.
 *
 * <p>This pass requires no configuration.
 *
 * <p>This pass requires the context to provide:
 *
 * <p>- Nothing
 */
public final class ShadowedPatternFields implements MiniPassFactory {
  public static final ShadowedPatternFields INSTANCE = new ShadowedPatternFields();

  private ShadowedPatternFields() {}

  @Override
  public List<IRProcessingPass> precursorPasses() {
    java.util.List<IRProcessingPass> list = java.util.List.of(GenerateMethodBodies$.MODULE$);
    return CollectionConverters.asScala(list).toList();
  }

  @Override
  public List<IRProcessingPass> invalidatedPasses() {
    java.util.List<IRProcessingPass> list =
        java.util.List.of(
            AliasAnalysis$.MODULE$,
            DataflowAnalysis$.MODULE$,
            DemandAnalysis$.MODULE$,
            IgnoredBindings$.MODULE$,
            NestedPatternMatch$.MODULE$,
            TailCall.INSTANCE);
    return CollectionConverters.asScala(list).toList();
  }

  @Override
  public MiniIRPass createForModuleCompilation(ModuleContext moduleContext) {
    return new Mini();
  }

  @Override
  public MiniIRPass createForInlineCompilation(InlineContext inlineContext) {
    return new Mini();
  }

  private static final class Mini extends MiniIRPass {
    @Override
    @SuppressWarnings("unchecked")
    public Expression transformExpression(Expression expr) {
      return switch (expr) {
        case Case.Branch branch -> lintCaseBranch(branch);
        case Case.Expr caseExpr -> {
          Seq<Branch> newBranches = caseExpr.branches().map(this::lintCaseBranch).toSeq();
          yield caseExpr.copy(
              caseExpr.scrutinee(),
              newBranches,
              caseExpr.isNested(),
              caseExpr.location(),
              caseExpr.passData(),
              caseExpr.diagnostics(),
              caseExpr.id());
        }
        default -> expr;
      };
    }

    /**
     * Lints for shadowed pattern variables in a case branch.
     *
     * @param branch the case branch to lint
     * @return `branch`, with warnings for any shadowed pattern variables
     */
    private Case.Branch lintCaseBranch(Case.Branch branch) {
      var newPattern = lintPattern(branch.pattern());
      return branch.copy(
          newPattern,
          branch.expression(),
          branch.terminalBranch(),
          branch.location(),
          branch.passData(),
          branch.diagnostics(),
          branch.id());
    }

    /**
     * Lints a pattern for shadowed pattern variables.
     *
     * <p>A later pattern variable shadows an earlier pattern variable with the same name.
     *
     * @param pattern the pattern to lint
     * @return `pattern`, with a warning applied to any shadowed pattern variables
     */
    private Pattern lintPattern(Pattern pattern) {
      var seenNames = new HashSet<String>();
      var lastSeen = new HashMap<String, IR>();

      return go(pattern, seenNames, lastSeen);
    }

    private Pattern go(Pattern pattern, Set<String> seenNames, Map<String, IR> lastSeen) {
      return switch (pattern) {
        case Pattern.Name named -> {
          var name = named.name().name();
          if (seenNames.contains(name)) {
            var warning = new PatternBinding(name, lastSeen.get(name), named.identifiedLocation());
            lastSeen.put(name, named);
            var blank = new Name.Blank(named.identifiedLocation(), new MetadataStorage());
            var patternCopy = named.copyWithName(blank);
            patternCopy.getDiagnostics().add(warning);
            yield patternCopy;
          } else if (!(named.name() instanceof Name.Blank)) {
            lastSeen.put(name, named);
            seenNames.add(name);
            yield named;
          } else {
            yield named;
          }
        }
        case Pattern.Constructor cons -> {
          var newFields =
              cons.fields().reverse().map(field -> go(field, seenNames, lastSeen)).reverse();
          yield cons.copyWithFields(newFields);
        }
        case Pattern.Literal literal -> literal;
        case Pattern.Type typed -> {
          var name = typed.name().name();
          if (seenNames.contains(name)) {
            var warning = new PatternBinding(name, lastSeen.get(name), typed.identifiedLocation());
            lastSeen.put(name, typed);
            var blank = new Name.Blank(typed.identifiedLocation(), new MetadataStorage());
            var typedCopy =
                typed.copy(
                    blank,
                    typed.tpe(),
                    typed.location(),
                    typed.passData(),
                    typed.diagnostics(),
                    typed.id());
            typedCopy.getDiagnostics().add(warning);
            yield typedCopy;
          } else if (!(typed.name() instanceof Name.Blank)) {
            lastSeen.put(name, typed);
            seenNames.add(name);
            yield typed;
          } else {
            yield typed;
          }
        }
        case Pattern.Documentation doc -> throw new CompilerError(
            "Branch documentation should be desugared at an earlier stage.");
        default -> pattern;
      };
    }
  }
}
