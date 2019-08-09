package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.error.InexhaustivePatternMatchException;

/**
 * This node is used when there is no explicit default case provided by the user for a pattern
 * match. It is used to signal inexhaustivity.
 */
public class DefaultFallbackNode extends CaseNode {

  /**
   * Executes the case expression's error case.
   *
   * @param frame the stack frame in which to execute
   * @param target the constructor to destructure
   */
  @Override
  public void execute(VirtualFrame frame, Atom target) {
    throw new InexhaustivePatternMatchException(this.getParent());
  }
}
