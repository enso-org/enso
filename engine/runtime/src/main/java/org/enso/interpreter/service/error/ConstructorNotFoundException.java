package org.enso.interpreter.service.error;

/** Thrown when the constructor can not be found for a given module. */
public class ConstructorNotFoundException extends RuntimeException implements ServiceException {

  private final String module;

  /**
   * Create new instance of this error.
   *
   * @param module the qualified module name.
   * @param constructor the name of the non-existent constructor.
   */
  public ConstructorNotFoundException(String module, String constructor) {
    super("Constructor " + constructor + " not found in module " + module + ".");
    this.module = module;
  }

  /** @return moudle name. */
  public String getModule() {
    return module;
  }
}
