package org.enso.interpreter.node.controlflow;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.InexhaustivePatternMatchException;

/**
 * This node is used when there is no explicit default case provided by the user for a pattern
 * match. It is used to signal inexhaustivity.
 */
@NodeInfo(
    shortName = "DefaultFallback",
    description = "A fallback branch for a case expression when none is explicitly provided")
public class DefaultFallbackNode extends CaseNode {

  /**
   * Executes the case expression's error case, by throwing a {@link
   * InexhaustivePatternMatchException}.
   *
   * @param frame the stack frame in which to execute
   * @param target the atom to match and destructure
   */
  @Override
  public void executeAtom(VirtualFrame frame, Atom target) {
    throw new InexhaustivePatternMatchException(this.getParent(), target.getConstructor());
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
    throw new InexhaustivePatternMatchException(this.getParent(), "Function");
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
    throw new InexhaustivePatternMatchException(this.getParent(), target);
  }
}
