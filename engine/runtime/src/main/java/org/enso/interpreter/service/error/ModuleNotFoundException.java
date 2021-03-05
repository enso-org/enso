package org.enso.interpreter.service.error;

import java.util.UUID;

/** Thrown when a module was requested but could not be found. */
public class ModuleNotFoundException extends RuntimeException implements ServiceException {

  /**
   * Create new instance of this error.
   *
   * @param name the qualified name of the non-existent module.
   */
  public ModuleNotFoundException(String name) {
    super("Module " + name + " not found.");
  }

  public ModuleNotFoundException(UUID expressionId) {
    super("Module containing " + expressionId + " not found.");
  }
}
