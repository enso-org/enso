package org.enso.compiler.pass;

import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.CompilerError;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.DefinitionArgument;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.Name;
import scala.Option;

public final class MiniPassTraverser {
  private MiniPassTraverser() {}

  /**
   * Compiles the whole module with chained {@link MiniIRPass}.
   *
   * @param miniPass Mini pass that must support module compilation, i.e., should be created with
   *     {@link MiniPassFactory#createForModuleCompilation(ModuleContext)}.
   * @return Transformed module IR.
   */
  public static Module compileModuleWithMiniPass(Module moduleIr, MiniIRPass miniPass) {
    var newModuleIr = compileRecursively(moduleIr, miniPass);
    return (Module) newModuleIr;
  }

  public static Expression compileInlineWithMiniPass(Expression exprIr, MiniIRPass miniPass) {
    miniPass.prepare(exprIr);
    var newIr = compileRecursively(exprIr, miniPass);
    if (!miniPass.checkPostCondition(newIr)) {
      throw new CompilerError("Post condition failed after applying mini pass " + miniPass);
    }
    return (Expression) newIr;
  }

  private static IR compileRecursively(IR ir, MiniIRPass miniPass) {
    miniPass.prepare(ir);
    IR newIr;
    if (ir.children().isEmpty()) {
      newIr = ir;
    } else {
      var transformedChildren =
          ir.children()
              .map(
                  child -> {
                    var newChild = compileRecursively(child, miniPass);
                    if (newChild == null) {
                      throw new IllegalStateException("Mini pass returned null");
                    }
                    if (!miniPass.checkPostCondition(newChild)) {
                      throw new CompilerError(
                          "Post condition failed after applying mini pass " + miniPass);
                    }
                    return newChild;
                  });
      newIr = copyWithNewChildren(ir, transformedChildren);
    }
    var transformedIr = miniPass.transformIr(newIr);
    if (!miniPass.checkPostCondition(transformedIr)) {
      throw new CompilerError("Post condition failed after applying mini pass " + miniPass);
    }
    return transformedIr;
  }

  /**
   * Special copy handling of IR elements where the type and the order of children is ambiguous. For
   * example {@link DefinitionArgument.Specified}.
   *
   * @param ir Parent IR.
   * @param newChildren List of transformed children.
   * @return Copy of the parent IR with new children.
   */
  private static IR copyWithNewChildren(IR ir, scala.collection.immutable.List<IR> newChildren) {
    return switch (ir) {
      case DefinitionArgument.Specified defArg -> {
        if (newChildren.isEmpty()) {
          throw new IllegalArgumentException("Definition argument must have at least one child");
        } else if (newChildren.size() == 1) {
          var newName = (Name) newChildren.apply(0);
          yield defArg.copy(
              newName,
              defArg.ascribedType(),
              defArg.defaultValue(),
              defArg.suspended(),
              defArg.location(),
              defArg.passData(),
              defArg.diagnostics(),
              defArg.id());
        } else if (newChildren.size() == 2) {
          var newName = (Name) newChildren.apply(0);
          var newAscribedTypeOrDefaultVal = (Expression) newChildren.apply(1);
          if (defArg.ascribedType().isDefined()) {
            yield defArg.copy(
                newName,
                Option.apply(newAscribedTypeOrDefaultVal),
                defArg.defaultValue(),
                defArg.suspended(),
                defArg.location(),
                defArg.passData(),
                defArg.diagnostics(),
                defArg.id());
          } else if (defArg.defaultValue().isDefined()) {
            yield defArg.copy(
                newName,
                defArg.ascribedType(),
                Option.apply(newAscribedTypeOrDefaultVal),
                defArg.suspended(),
                defArg.location(),
                defArg.passData(),
                defArg.diagnostics(),
                defArg.id());
          } else {
            throw new IllegalArgumentException(
                "Definition argument must have at most two children");
          }
        } else if (newChildren.size() == 3) {
          var newName = (Name) newChildren.apply(0);
          var newAscribedType = (Expression) newChildren.apply(1);
          var newDefaultValue = (Expression) newChildren.apply(2);
          yield defArg.copy(
              newName,
              Option.apply(newAscribedType),
              Option.apply(newDefaultValue),
              defArg.suspended(),
              defArg.location(),
              defArg.passData(),
              defArg.diagnostics(),
              defArg.id());
        } else {
          throw new IllegalArgumentException(
              "Definition argument must have at most three children");
        }
      }
      default -> ir.withNewChildren(newChildren);
    };
  }
}
