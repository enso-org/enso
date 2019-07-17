package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.Atom;
import org.enso.interpreter.runtime.errors.TypeError;

public class MatchNode extends ExpressionNode {
  @Child private ExpressionNode target;
  @Children private final CaseNode[] cases;
  @Child private CaseNode fallback;
  private final BranchProfile typeErrorProfile = BranchProfile.create();

  public MatchNode(ExpressionNode target, CaseNode[] cases, CaseNode fallback) {
    this.target = target;
    this.cases = cases;
    this.fallback = fallback;
  }

  @Override public void markTail() {
    for (CaseNode caseNode: cases) {
      caseNode.markTail();
    }
    fallback.markTail();
  }

  @ExplodeLoop
  @Override
  public Object executeGeneric(VirtualFrame frame) {
    try {
      Atom atom = target.executeAtom(frame);
      for (CaseNode caseNode : cases) {
        caseNode.execute(frame, atom);
      }
      fallback.execute(frame, atom);
      CompilerDirectives.transferToInterpreter();
      throw new RuntimeException("Impossible behavior.");
    } catch (BranchSelectedException e) {
      return e.getResult();
    } catch (UnexpectedResultException e) {
      typeErrorProfile.enter();
      throw new TypeError("Expected an Atom.", this);
    }
  }
}
