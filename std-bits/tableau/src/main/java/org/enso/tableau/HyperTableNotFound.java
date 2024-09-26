package org.enso.tableau;

public class HyperTableNotFound extends RuntimeException {
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
}
