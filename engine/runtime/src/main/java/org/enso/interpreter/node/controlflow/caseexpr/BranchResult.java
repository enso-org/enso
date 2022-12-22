package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.EnsoContext;

public record BranchResult(boolean isMatched, Object result) {

  public static BranchResult failure(Node node) {
    return new BranchResult(false, EnsoContext.get(node).getBuiltins().nothing());
  }

  public static BranchResult success(Object result) {
    return new BranchResult(true, result);
  }

}
