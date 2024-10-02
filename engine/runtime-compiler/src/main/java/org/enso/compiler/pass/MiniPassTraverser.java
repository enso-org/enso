package org.enso.compiler.pass;

import org.enso.compiler.context.ModuleContext;
import org.enso.compiler.core.CompilerError;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;

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
    var preparedMiniPass = miniPass.prepare(exprIr);
    var newIr = compileRecursively(exprIr, preparedMiniPass);
    if (!preparedMiniPass.checkPostCondition(newIr)) {
      throw new CompilerError("Post condition failed after applying mini pass " + preparedMiniPass);
    }
    return (Expression) newIr;
  }

  private static IR compileRecursively(IR ir, MiniIRPass miniPass) {
    var preparedMiniPass = miniPass.prepare(ir);
    IR newIr;
    if (ir.children().isEmpty()) {
      newIr = ir;
    } else {
      var transformedChildren =
          ir.children()
              .map(
                  child -> {
                    var newChild = compileRecursively(child, preparedMiniPass);
                    if (newChild == null) {
                      throw new IllegalStateException("Mini pass returned null");
                    }
                    if (!preparedMiniPass.checkPostCondition(newChild)) {
                      throw new CompilerError(
                          "Post condition failed after applying mini pass " + preparedMiniPass);
                    }
                    return newChild;
                  });
      newIr = ir.withNewChildren(transformedChildren);
    }
    var transformedIr = preparedMiniPass.transformIr(newIr);
    if (!preparedMiniPass.checkPostCondition(transformedIr)) {
      throw new CompilerError("Post condition failed after applying mini pass " + preparedMiniPass);
    }
    return transformedIr;
  }
}
