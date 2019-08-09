package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.UnexpectedResultException;
import com.oracle.truffle.api.profiles.BranchProfile;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.TypeError;

/** A node representing a pattern match on an Atom. */
public class MatchNode extends ExpressionNode {
  @Child private ExpressionNode target;
  @Children private final CaseNode[] cases;
  @Child private CaseNode fallback;
  private final BranchProfile typeErrorProfile = BranchProfile.create();

  /**
   * Creates a node that pattern matches on an Atom.
   *
   * @param target the atom to pattern match on
   * @param cases the branches of the pattern match
   * @param fallback the fallback case of the pattern match
   */
  public MatchNode(ExpressionNode target, CaseNode[] cases, CaseNode fallback) {
    this.target = target;
    this.cases = cases;
    this.fallback = fallback;
  }

  /**
   * Sets whether or not the pattern match is tail-recursive.
   *
   * @param isTail whether or not the expression is tail-recursive
   */
  @Override
  @ExplodeLoop
  public void setTail(boolean isTail) {
    for (CaseNode caseNode : cases) {
      caseNode.setTail(isTail);
    }
    fallback.setTail(isTail);
  }

  /**
   * Executes the pattern match.
   *
   * @param frame the stack frame for execution
   * @return the result of the pattern match
   */
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
      // Note [Branch Selection Control Flow]
      return e.getResult();

    } catch (UnexpectedResultException e) {
      typeErrorProfile.enter();
      throw new TypeError("Expected an Atom.", this);
    }
  }

  /* Note [Branch Selection Control Flow]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Truffle provides no easy way to return control flow from multiple paths. This is entirely due
   * to Java's (current) lack of support for case-expressions.
   *
   * As a result, this implementation resorts to using an exception to short-circuit evaluation on
   * a successful match. This short-circuiting also allows us to hand the result of evaluation back
   * out to the caller (here) wrapped in the exception.
   *
   * The main alternative to this was desugaring to a nested-if, which would've been significantly
   * harder to maintain, and also resulted in significantly higher code complexity.
   */
}
