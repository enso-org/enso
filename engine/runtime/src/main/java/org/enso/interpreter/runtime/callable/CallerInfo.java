package org.enso.interpreter.runtime.callable;

import com.oracle.truffle.api.frame.MaterializedFrame;
import org.enso.interpreter.runtime.scope.LocalScope;
import org.enso.interpreter.runtime.scope.ModuleScope;

/**
 * Represents the caller execution context, to be passed to functions that declare the need for it.
 */
public class CallerInfo {
  private final MaterializedFrame frame;
  private final LocalScope localScope;
  private final ModuleScope moduleScope;

  /**
   * Creates a new instance of caller information
   *
   * @param frame the caller's execution frame
   * @param localScope the local scope caller uses
   * @param moduleScope the module scope caller was defined in
   */
  public CallerInfo(MaterializedFrame frame, LocalScope localScope, ModuleScope moduleScope) {
    this.frame = frame;
    this.localScope = localScope;
    this.moduleScope = moduleScope;
  }

  /**
   * Gets the caller's execution frame.
   *
   * @return the caller's execution frame
   */
  public MaterializedFrame getFrame() {
    return frame;
  }

  /**
   * Gets the caller's local scope metadata.
   *
   * @return the caller's local scope metadata
   */
  public LocalScope getLocalScope() {
    return localScope;
  }

  /**
   * Gets the caller's module scope.
   *
   * @return the caller's module scope.
   */
  public ModuleScope getModuleScope() {
    return moduleScope;
  }
}
