package org.enso.table.data.table;

/** A representation of a table structure. */
public class Table {

  private final Column[] columns;

  /**
   * Creates a new table
   *
   * @param columns the columns contained in this table.
   */
  public Table(Column[] columns) {
    this.columns = columns;
  }

  /** @return the number of rows in this table */
  public long nrows() {
    if (columns == null || columns.length == 0) {
      return 0;
    } else {
      return columns[0].getStorage().size();
    }
  }

  /** @return the columns of this table */
  public Column[] getColumns() {
    return columns;
  }
}
