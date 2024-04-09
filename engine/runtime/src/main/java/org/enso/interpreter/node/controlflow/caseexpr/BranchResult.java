package org.enso.interpreter.node.controlflow.caseexpr;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoObject;

record BranchResult(boolean isMatched, Object result) implements EnsoObject {
  static BranchResult failure(Node node) {
    return new BranchResult(false, EnsoContext.get(node).getBuiltins().nothing());
  }

  static BranchResult success(Object result) {
    return new BranchResult(true, result);
  }
}
