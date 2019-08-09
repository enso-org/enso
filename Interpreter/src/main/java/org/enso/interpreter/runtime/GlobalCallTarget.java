package org.enso.interpreter.runtime;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.interop.TruffleObject;

/**
 * A representation of the top-level callable pieces of code in Enso.
 */
public class GlobalCallTarget implements TruffleObject {
  @CompilationFinal private RootCallTarget target;

  /**
   * Creates a new call target.
   *
   * @param target a handle to the code to associate with this target
   */
  public GlobalCallTarget(RootCallTarget target) {
    this.target = target;
  }

  /**
   * Gets the handle to the code associated with this target.
   *
   * @return a handle to the associated code
   */
  public RootCallTarget getTarget() {
    return target;
  }

  /**
   * Sets the internal code handle.
   *
   * @param target a handle to the code to be associated with this target
   */
  public void setTarget(RootCallTarget target) {
    this.target = target;
  }
}
