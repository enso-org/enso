package org.enso.table.data.table;

import org.enso.table.data.column.storage.BoolStorage;

import java.util.BitSet;

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

  public Column getColumnByName(String name) {
    for (Column column : columns) {
      if (column.getName().equals(name)) {
        return column;
      }
    }
    return null;
  }

  public Table mask(Column maskCol, boolean naVal) {
    BoolStorage storage = (BoolStorage) maskCol.getStorage();
    BitSet mask = new BitSet();
    mask.or(storage.getValues());
    if (storage.isNegated()) {
      mask.flip(0, (int) storage.size());
    }
    mask.andNot(storage.getIsMissing());
    int cardinality = mask.cardinality();
    Column[] newColumns = new Column[columns.length];
    for (int i = 0; i < columns.length; i++) {
      newColumns[i] = columns[i].mask(mask, cardinality);
    }
    return new Table(newColumns);
  }
}
