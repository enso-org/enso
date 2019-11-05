package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.InexhaustivePatternMatchException;

/**
 * This node is used when there is no explicit default case provided by the user for a pattern
 * match. It is used to signal inexhaustivity.
 */
public class DefaultFallbackNode extends CaseNode {

  /**
   * Executes the case expression's error case, by throwing a {@link
   * InexhaustivePatternMatchException}.
   *
   * @param frame the stack frame in which to execute
   * @param target the constructor to destructure
   */
  private void execute(VirtualFrame frame, Object target) {
    throw new InexhaustivePatternMatchException(this.getParent());
  }

  /**
   * Executes the case expression's error case, by throwing a {@link
   * InexhaustivePatternMatchException}.
   *
   * @param frame the stack frame in which to execute
   * @param target the atom to match and destructure
   */
  @Override
  public void executeAtom(VirtualFrame frame, Atom target) {
    execute(frame, target);
  }

  /**
   * Executes the case expression's error case, by throwing a {@link
   * InexhaustivePatternMatchException}.
   *
   * @param frame the stack frame in which to execute
   * @param target the function to match
   */
  @Override
  public void executeFunction(VirtualFrame frame, Function target) {
    execute(frame, target);
  }

  /**
   * Executes the case expression's error case, by throwing a {@link
   * InexhaustivePatternMatchException}.
   *
   * @param frame the stack frame in which to execute
   * @param target the number to match
   */
  @Override
  public void executeNumber(VirtualFrame frame, long target) {
    execute(frame, target);
  }
}
