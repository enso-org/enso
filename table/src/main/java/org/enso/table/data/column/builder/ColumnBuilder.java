package org.enso.table.data.column.builder;

import org.enso.table.data.column.Column;

public abstract class ColumnBuilder {
  public abstract ColumnBuilder parseAndAppend(String value);

  public abstract Column seal();
}
