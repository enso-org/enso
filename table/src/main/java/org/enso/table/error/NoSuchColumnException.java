package org.enso.table.error;

/** An exception thrown when a column is looked up by a non-existent name. */
public class NoSuchColumnException extends RuntimeException {
  private final String name;

  /**
   * Creates a new instance of this error.
   *
   * @param name the column name
   */
  public NoSuchColumnException(String name) {
    super("The column with name " + name + " does not exist.");
    this.name = name;
  }
}
