package org.enso.interpreter.service.error;

/** Thrown when the method definition is not found in the given module. */
public class MethodNotFoundException extends RuntimeException implements ServiceException {

  private final String module;

  /**
   * Create new instance of this error.
   *
   * @param target the construct that was attempted to be invoked.
   * @param module the qualified module name.
   * @param method the name of the non-existing method.
   */
  public MethodNotFoundException(String module, Object target, String method) {
    super("Object " + target + " does not define method " + method + " in module " + module + ".");
    this.module = module;
  }

  /**
   * @return module name
   */
  public String getModule() {
    return module;
  }
}
