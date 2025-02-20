package org.enso.compiler.pass.analyse;

import java.util.List;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.Implicits;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Pattern;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.core.ir.expression.Case.Branch;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap.ResolvedConstructor;
import org.enso.compiler.data.BindingsMap.ResolvedModule;
import org.enso.compiler.data.BindingsMap.ResolvedName;
import org.enso.compiler.pass.IRPass;
import org.enso.compiler.pass.IRProcessingPass;
import org.enso.compiler.pass.resolve.Patterns$;
import org.enso.pkg.QualifiedName;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Iterates all the symbols in the IR and checks for their {@link
 * org.enso.compiler.data.BindingsMap.Resolution} metadata. If they are present, it checks if the
 * symbol is private and if it is used outside of its defining project. If so, a private-access
 * error IR is generated.
 */
public class PrivateSymbolsAnalysis implements IRPass {
  public static final PrivateSymbolsAnalysis INSTANCE = new PrivateSymbolsAnalysis();

  private PrivateSymbolsAnalysis() {}

  @Override
  public Seq<IRProcessingPass> precursorPasses() {
    List<IRProcessingPass> passes =
        List.of(
            PrivateModuleAnalysis.INSTANCE, PrivateConstructorAnalysis.INSTANCE, Patterns$.MODULE$);
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  public Seq<IRProcessingPass> invalidatedPasses() {
    return nil();
  }

  @Override
  public <T extends IR> T updateMetadataInDuplicate(T sourceIr, T copyOfIr) {
    return IRPass.super.updateMetadataInDuplicate(sourceIr, copyOfIr);
  }

  @Override
  public Module runModule(Module ir, ModuleContext moduleContext) {
    var bindingsMap =
        (BindingsMap)
            Implicits.AsMetadata(ir)
                .unsafeGetMetadata(BindingAnalysis$.MODULE$, () -> "BindingsMap should be present");
    var newBindings =
        ir.bindings()
            .map(binding -> binding.mapExpressions(expr -> processExpression(expr, bindingsMap)));
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

  /** Not supported for expressions. */
  @Override
  public Expression runExpression(Expression ir, InlineContext inlineContext) {
    return ir;
  }

  @SuppressWarnings("unchecked")
  private Expression processExpression(Expression expr, BindingsMap bindingsMap) {
    return switch (expr) {
      case Case.Expr caseExpr -> {
        var newBranches =
            caseExpr.branches().map(branch -> processCaseBranch(branch, bindingsMap)).toSeq();
        var newScrutinee = processExpression(caseExpr.scrutinee(), bindingsMap);
        yield caseExpr.copy(
            newScrutinee,
            newBranches,
            caseExpr.copy$default$3(),
            caseExpr.copy$default$4(),
            caseExpr.copy$default$5(),
            caseExpr.copy$default$6(),
            caseExpr.copy$default$7());
      }
      case Name name -> processName(name, bindingsMap);
      default -> expr.mapExpressions(e -> processExpression(e, bindingsMap));
    };
  }

  private Branch processCaseBranch(Branch branch, BindingsMap bindingsMap) {
    var pat = branch.pattern();
    var newPat = processCasePattern(pat, bindingsMap);
    var newExpr = processExpression(branch.expression(), bindingsMap);
    return branch.copy(
        newPat,
        newExpr,
        branch.copy$default$3(),
        branch.copy$default$4(),
        branch.copy$default$5(),
        branch.copy$default$6(),
        branch.copy$default$7());
  }

  private Pattern processCasePattern(Pattern pattern, BindingsMap bindingsMap) {
    if (pattern instanceof Pattern.Constructor cons) {
      var consName = cons.constructor();
      var resolvedNames = tryResolveName(consName, bindingsMap);
      for (var resolvedName : resolvedNames) {
        if (isProjectPrivate(resolvedName)) {
          var curProjName = getProjName(bindingsMap.currentModule().getName());
          var resolvedProjName = getProjName(resolvedName.module().getName());
          if (!curProjName.equals(resolvedProjName)) {
            var reason =
                new org.enso.compiler.core.ir.expression.errors.Pattern.PrivateConstructor(
                    consName.name(), curProjName, resolvedProjName);
            return new org.enso.compiler.core.ir.expression.errors.Pattern(
                cons, reason, cons.passData(), cons.diagnostics());
          }
        }
      }
    }
    return pattern.mapExpressions(e -> processExpression(e, bindingsMap));
  }

  private Expression processName(Name name, BindingsMap bindingsMap) {
    var resolvedNames = tryResolveName(name, bindingsMap);
    for (var resolvedName : resolvedNames) {
      if (isProjectPrivate(resolvedName)) {
        var curProjName = getProjName(bindingsMap.currentModule().getName());
        var resolvedProjName = getProjName(resolvedName.module().getName());
        if (!curProjName.equals(resolvedProjName)) {
          var reason =
              new org.enso.compiler.core.ir.expression.errors.Resolution.PrivateEntity(
                  curProjName, resolvedProjName);
          return new org.enso.compiler.core.ir.expression.errors.Resolution(
              name, reason, name.passData(), name.diagnostics());
        }
      }
    }
    return name.mapExpressions(e -> processExpression(e, bindingsMap));
  }

  private static boolean isProjectPrivate(ResolvedName resolvedName) {
    return switch (resolvedName) {
      case ResolvedConstructor resolvedCons -> resolvedCons.cons().isProjectPrivate();
      case ResolvedModule resolvedMod -> {
        if (resolvedMod.module()
            instanceof org.enso.compiler.data.BindingsMap$ModuleReference$Concrete concreteMod) {
          yield concreteMod.module().isPrivate();
        } else {
          yield false;
        }
      }
      default -> false;
    };
  }

  private static String getProjName(QualifiedName qualName) {
    if (qualName.pathAsJava().size() < 2) {
      return "unknown";
    } else {
      return qualName.pathAsJava().get(0) + "." + qualName.pathAsJava().get(1);
    }
  }

  private List<ResolvedName> tryResolveName(Name name, BindingsMap bindingsMap) {
    return switch (name) {
      case Name.Literal lit -> {
        var resolved = bindingsMap.resolveName(lit.name());
        if (resolved.isRight()) {
          var resolvedNames = resolved.toOption().get();
          yield CollectionConverters.asJava(resolvedNames);
        } else {
          yield List.of();
        }
      }
      case Name.Qualified qual -> {
        var nameParts = qual.parts().map(Name::name);
        var resolved = bindingsMap.resolveQualifiedName(nameParts);
        if (resolved.isRight()) {
          var resolvedNames = resolved.toOption().get();
          yield CollectionConverters.asJava(resolvedNames);
        } else {
          yield List.of();
        }
      }
      default -> List.of();
    };
  }

  @SuppressWarnings("unchecked")
  private static <T> scala.collection.immutable.List<T> nil() {
    return (scala.collection.immutable.List<T>) scala.collection.immutable.Nil$.MODULE$;
  }
}
