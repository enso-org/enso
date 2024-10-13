package org.enso.compiler.pass;

import java.util.ArrayList;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;

/** Implementation of {@link MiniIRPass#compile}. */
final class MiniPassTraverser {
  private MiniPassTraverser() {}

  static IR compileRecursively(IR ir, MiniIRPass miniPass) {
    var preparedMiniPass =
        switch (ir) {
          case Module m -> miniPass.prepareForModule(m);
          case Expression e -> miniPass.prepare(e);
          default -> throw new IllegalArgumentException("" + ir);
        };
    IR newIr;
    var childExpressions = new ArrayList<Expression>();
    ir.mapExpressions(
        (ch) -> {
          childExpressions.add(ch);
          return ch;
        });
    if (childExpressions.isEmpty()) {
      newIr = ir;
    } else {
      var changed = false;
      for (var i = 0; i < childExpressions.size(); i++) {
        var child = childExpressions.get(i);
        var newChild = (Expression) compileRecursively(child, preparedMiniPass);
        if (child == newChild) {
          continue;
        }
        changed = true;
        childExpressions.set(i, newChild);
      }
      if (changed) {
        var index = new int[1];
        newIr = ir.mapExpressions((old) -> childExpressions.get(index[0]++));
      } else {
        newIr = ir;
      }
    }
    var transformedIr =
        switch (newIr) {
          case Module m -> miniPass.transformModule(m);
          case Expression e -> miniPass.transformExpression(e);
          default -> throw new IllegalArgumentException("" + newIr);
        };
    return transformedIr;
  }
}
