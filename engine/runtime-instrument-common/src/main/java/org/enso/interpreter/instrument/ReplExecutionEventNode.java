package org.enso.interpreter.instrument;

import java.util.Map;
import scala.util.Either;

public interface ReplExecutionEventNode {

  /**
   * Lists all the bindings available in the current execution scope.
   *
   * @return a map, where keys are variable names and values are current values of variables.
   */
  Map<String, Object> listBindings();

  /**
   * Evaluates an arbitrary expression in the current execution context.
   *
   * @param expression the expression to evaluate
   * @return the result of evaluating the expression or an exception that caused failure
   */
  Either<Exception, Object> evaluate(String expression);

  /**
   * Returns the String representation of the provided object as defined by Enso {@code to_text}
   * operation.
   *
   * @param object the object to show
   * @return String representation of the provided object or a failure if it cannot be inferred
   */
  Either<Exception, String> showObject(Object object);

  /**
   * Terminates this REPL session.
   *
   * <p>The last result of {@link #evaluate(String)} (or {@link
   * org.enso.interpreter.runtime.builtin.Builtins#nothing()} if {@link #evaluate(String)} was not
   * called before) will be returned from the instrumented node.
   *
   * <p>This function must always be called at the end of REPL session, as otherwise the program
   * will never resume. It's forbidden to use this object after exit has been called.
   */
  void exit();
}
