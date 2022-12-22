package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.EnsoContext;

public class BranchResult {
  private final boolean matched;
  private final Object result;

  public BranchResult(boolean matched, Object result) {
    this.matched = matched;
    this.result = result;
  }

  public static BranchResult failure(Node node) {
    return new BranchResult(false, EnsoContext.get(node).getBuiltins().nothing());
  }

  public static BranchResult success(Object result) {
    return new BranchResult(true, result);
  }

  public boolean isMatched() {
    return matched;
  }

  public Object getResult() {
    return result;
  }
}
