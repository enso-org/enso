package org.enso.compiler.pass;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.core.ir.Module;

/** Implementation of {@link MiniIRPass#compile}. */
final class MiniPassTraverser {
  private final MiniIRPass miniPass;
  private List<IR> in;
  private final List<IR> out;
  private int outIndex;

  private MiniPassTraverser(MiniIRPass miniPass, List<IR> out, int outIndex) {
    this.miniPass = miniPass;
    this.out = out;
    this.outIndex = outIndex;
  }

  private boolean enqueue(Collection<MiniPassTraverser> queue) {
    if (in == null) {
      var ir = out.get(outIndex);
      in = enqueueSubExpressions(queue, ir, miniPass);
      return !in.isEmpty();
    } else {
      return false;
    }
  }

  private void convertExpression() {
    if (outIndex != -1) {
      var oldIr = out.get(outIndex);
      var index = new int[1];
      var newIr = oldIr.mapExpressions((old) -> (Expression) in.get(index[0]++));
      var transformedIr =
          switch (newIr) {
            case Module m -> miniPass.transformModule(m);
            case Expression e -> miniPass.transformExpression(e);
            default -> throw new IllegalArgumentException("" + oldIr);
          };
      if (oldIr != transformedIr) {
        out.set(outIndex, transformedIr);
      }
      outIndex = -1;
    }
  }

  static IR compileDeep(IR root, MiniIRPass miniPass) {
    var result = new IR[] {root};
    var rootTask = new MiniPassTraverser(miniPass, Arrays.asList(result), 0);
    var stackOfPendingIrs = new LinkedList<MiniPassTraverser>();
    stackOfPendingIrs.add(rootTask);
    while (!stackOfPendingIrs.isEmpty()) {
      if (stackOfPendingIrs.peekLast().enqueue(stackOfPendingIrs)) {
        // continue descent
        continue;
      }
      var deepestIr = stackOfPendingIrs.removeLast();
      deepestIr.convertExpression();
    }
    assert result[0] != null;
    return result[0];
  }

  /**
   * @param queue queue to put objects in
   * @param ir IR to process
   * @param miniPass process with this mini pass
   * @return {@code true} if the has been modified with new tries to process first
   */
  private static List<IR> enqueueSubExpressions(
      Collection<MiniPassTraverser> queue, IR ir, MiniIRPass miniPass) {
    var childExpressions = new ArrayList<IR>();
    var i = new int[1];
    ir.mapExpressions(
        (ch) -> {
          var preparedMiniPass = miniPass.prepare(ir, ch);
          childExpressions.add(ch);
          queue.add(new MiniPassTraverser(preparedMiniPass, childExpressions, i[0]++));
          return ch;
        });
    return childExpressions;
  }
}
