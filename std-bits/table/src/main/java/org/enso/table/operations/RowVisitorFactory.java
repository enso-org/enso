package org.enso.table.operations;

import org.enso.table.data.column.storage.ColumnStorage;

public interface RowVisitorFactory {
  GroupRowVisitor getNewRowVisitor();

  ColumnStorage<?> seal();
}
