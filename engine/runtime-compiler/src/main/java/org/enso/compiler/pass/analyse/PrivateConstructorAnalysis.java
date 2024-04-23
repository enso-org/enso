package org.enso.compiler.pass.analyse;

import java.util.List;
import java.util.UUID;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.Implicits;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import org.enso.compiler.core.ir.Pattern;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.core.ir.expression.errors.Syntax;
import org.enso.compiler.core.ir.expression.errors.Syntax.InconsistentConstructorVisibility$;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.Definition.Type;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap.ResolvedName;
import org.enso.compiler.pass.IRPass;
import org.enso.pkg.QualifiedName;
import scala.collection.immutable.Seq;
import scala.jdk.javaapi.CollectionConverters;

/**
 * Ensures that all type definitions have either all constructors public, or all constructors
 * private. Also ensures that the constructors used in pattern matching are accessible.
 */
public final class PrivateConstructorAnalysis implements IRPass {
  public static final PrivateConstructorAnalysis INSTANCE = new PrivateConstructorAnalysis();

  private UUID uuid;

  private PrivateConstructorAnalysis() {}

  @Override
  public void org$enso$compiler$pass$IRPass$_setter_$key_$eq(UUID v) {
    this.uuid = v;
  }

  @Override
  public UUID key() {
    return uuid;
  }

  @Override
  public Seq<IRPass> precursorPasses() {
    List<IRPass> passes = List.of(PrivateModuleAnalysis.INSTANCE);
    return CollectionConverters.asScala(passes).toList();
  }

  @Override
  @SuppressWarnings("unchecked")
  public Seq<IRPass> invalidatedPasses() {
    Object obj = scala.collection.immutable.Nil$.MODULE$;
    return (scala.collection.immutable.List<IRPass>) obj;
  }

  @Override
  public Module runModule(Module ir, ModuleContext moduleContext) {
    var bindingsMap =
        (BindingsMap)
            Implicits.AsMetadata(ir)
                .unsafeGetMetadata(BindingAnalysis$.MODULE$, () -> "BindingsMap should be present");
    var newBindings =
        ir.bindings()
            .map(
                binding -> {
                  if (binding instanceof Definition.Type type) {
                    return processType(type, bindingsMap);
                  } else {
                    return binding.mapExpressions(expr -> processExpression(expr, bindingsMap));
                  }
                });
    return ir.copy(
        ir.imports(),
        ir.exports(),
        newBindings,
        ir.location(),
        ir.passData(),
        ir.diagnostics(),
        ir.id());
  }

  private Definition processType(Type type, BindingsMap bindingsMap) {
    var privateCtorsCnt = type.members().filter(ctor -> ctor.isPrivate()).size();
    var publicCtorsCnt = type.members().filter(ctor -> !ctor.isPrivate()).size();
    var ctorsCnt = type.members().size();
    if (!(privateCtorsCnt == ctorsCnt || publicCtorsCnt == ctorsCnt)) {
      assert type.location().isDefined();
      return Syntax.apply(
          type.location().get(),
          InconsistentConstructorVisibility$.MODULE$,
          type.passData(),
          type.diagnostics());
    }
    return type.mapExpressions(expr -> processExpression(expr, bindingsMap));
  }

  private Expression processExpression(Expression expr, BindingsMap bindingsMap) {
    return switch (expr) {
      case Case.Expr caseExpr -> processCaseExpr(caseExpr, bindingsMap);
      default -> expr.mapExpressions(e -> processExpression(e, bindingsMap));
    };
  }

  @SuppressWarnings("unchecked")
  private Case.Expr processCaseExpr(Case.Expr caseExpr, BindingsMap bindingsMap) {
    var newBranches = caseExpr.branches().map(branch -> processCaseBranch(branch, bindingsMap));
    return new Case.Expr(
        caseExpr.scrutinee(),
        newBranches.toSeq(),
        caseExpr.location(),
        caseExpr.passData(),
        caseExpr.diagnostics());
  }

  private Case.Branch processCaseBranch(Case.Branch branch, BindingsMap bindingsMap) {
    var newExpr = processExpression(branch.expression(), bindingsMap);
    var newPattern = branch.pattern();
    if (branch.pattern() instanceof Pattern.Constructor cons) {
      var consName = cons.constructor();
      var resolvedName = resolveName(consName, bindingsMap);
      if (resolvedName != null) {
        var curProjName = getProjName(bindingsMap.currentModule().getName());
        var resolvedProjName = getProjName(resolvedName.module().getName());
        if (!curProjName.equals(resolvedProjName)) {
          var reason =
              new org.enso.compiler.core.ir.expression.errors.Pattern.PrivateConstructor(
                  consName.name(), curProjName, resolvedProjName);
          newPattern =
              new org.enso.compiler.core.ir.expression.errors.Pattern(
                  cons, reason, cons.passData(), cons.diagnostics());
        }
      }
    }
    return new Case.Branch(
        newPattern,
        newExpr,
        branch.terminalBranch(),
        branch.location(),
        branch.passData(),
        branch.diagnostics());
  }

  private static String getProjName(QualifiedName qualName) {
    if (qualName.pathAsJava().size() < 2) {
      return "unknown";
    } else {
      return qualName.pathAsJava().get(0) + "." + qualName.pathAsJava().get(1);
    }
  }

  private ResolvedName resolveName(Name name, BindingsMap bindingsMap) {
    return switch (name) {
      case Name.Literal lit -> {
        var resolved = bindingsMap.resolveName(lit.name());
        assert resolved.isRight() : "ResolutionError should have been handled by previous passes";
        yield (ResolvedName) resolved.getOrElse(() -> null);
      }
      case Name.Qualified qual -> {
        var nameParts = qual.parts().map(Name::name);
        var resolved = bindingsMap.resolveQualifiedName(nameParts);
        assert resolved.isRight() : "ResolutionError should have been handled by previous passes";
        yield (ResolvedName) resolved.getOrElse(() -> null);
      }
      default -> null;
    };
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
