package org.enso.interpreter.runtime.callable.argument;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.frame.MaterializedFrame;

/** Runtime representation of a suspended function argument. */
public class Thunk {
  private final RootCallTarget callTarget;
  private final MaterializedFrame scope;

  /**
   * Creates a runtime thunk.
   *
   * @param callTarget the {@link CallTarget} representing the argument's expression
   * @param scope the caller scope used for evaluating the {@code callTarget}
   */
  public Thunk(RootCallTarget callTarget, MaterializedFrame scope) {
    this.callTarget = callTarget;
    this.scope = scope;
  }

  /**
   * Returns the call target representing the argument's expression.
   *
   * @return the call target representing the argument's expression.
   */
  public CallTarget getCallTarget() {
    return callTarget;
  }

  /**
   * Returns the caller scope.
   *
   * @return the caller scope used for evaluating this thunk.
   */
  public MaterializedFrame getScope() {
    return scope;
  }
}
