package org.enso.tableau;

import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

class HyperUnsupportedTypeError extends RuntimeException {
  private final String type_name;

  public HyperUnsupportedTypeError(String type_name) {
    super("The type " + type_name + " is not supported in hyper files.");
    this.type_name = type_name;
  }

  public String getTypeName() {
    return type_name;
  }

  public Value asEnsoAtom() {
    return EnsoMeta.makeInstance(
        "Standard.Tableau.Hyper_Errors", "Hyper_Unsupported_Type", "Error", getTypeName());
  }
}
