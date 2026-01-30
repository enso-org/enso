package org.enso.tableau;

import org.enso.base.polyglot.EnsoMeta;
import org.graalvm.polyglot.Value;

class HyperTableNotFound extends RuntimeException {
  private final String schema;
  private final String name;

  public HyperTableNotFound(String schema, String name, Throwable cause) {
    super("The table " + schema + "." + name + " does not exist.", cause);
    this.schema = schema;
    this.name = name;
  }

  public String getSchema() {
    return schema;
  }

  public String getName() {
    return name;
  }

  public Value asEnsoAtom() {
    return EnsoMeta.makeInstance(
        "Standard.Tableau.Hyper_Errors", "Hyper_Table_Not_Found", "Error", getSchema(), getName());
  }
}
