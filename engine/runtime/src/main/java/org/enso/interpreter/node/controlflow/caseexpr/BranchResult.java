package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoObject;

final class BranchResult extends EnsoObject {
  private final boolean isMatched;
  private final Object result;

  BranchResult(boolean isMatched, Object result) {
    this.isMatched = isMatched;
    this.result = result;
  }

  public boolean isMatched() {
    return isMatched;
  }

  public Object result() {
    return result;
  }

  static BranchResult failure(Node node) {
    return new BranchResult(false, EnsoContext.get(node).getBuiltins().nothing());
  }

  static BranchResult success(Object result) {
    return new BranchResult(true, result);
  }

  @Override
  @TruffleBoundary
  public Object toDisplayString(boolean allowSideEffects) {
    return "BranchResult(" + isMatched + ")";
  }
}
