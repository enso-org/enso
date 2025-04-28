package org.enso.tableau;

public class HyperUnsupportedTypeError extends RuntimeException {
  private final String type_name;

  public HyperUnsupportedTypeError(String type_name) {
    super("The type " + type_name + " is not supported in hyper files.");
    this.type_name = type_name;
  }

  public String getTypeName() {
    return type_name;
  }
}
