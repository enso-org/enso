package org.enso.interpreter.service.error;

import java.util.UUID;

/** Thrown when a module containing the given expression id can not be found. */
public class ModuleNotFoundForExpressionIdException extends ModuleNotFoundException {

  /**
   * Create new instance of this error.
   *
   * @param expressionId the expression identifier.
   */
  public ModuleNotFoundForExpressionIdException(UUID expressionId) {
    super("containing expression " + expressionId);
  }
}
