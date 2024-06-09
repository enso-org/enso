package org.enso.interpreter.service.error;

/** Thrown when the constructor can not be found for a given module. */
public class TypeNotFoundException extends RuntimeException implements ServiceException {

  private final String module;

  /**
   * Create new instance of this error.
   *
   * @param module the qualified module name.
   * @param type the name of the non-existent type.
   */
  public TypeNotFoundException(String module, String type) {
    super("Type " + type + " not found in module " + module + ".");
    this.module = module;
  }

  /**
   * @return module name.
   */
  public String getModule() {
    return module;
  }
}
