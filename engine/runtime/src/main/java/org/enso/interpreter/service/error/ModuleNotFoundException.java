package org.enso.interpreter.service.error;

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
}
