package org.enso.table.data.table.join.lookup;

import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.table.Column;

public sealed interface LookupColumnDescription {
  record KeepOriginal(Column column) implements LookupColumnDescription {}

  record MergeColumns(Column original, Column lookupReplacement, StorageType commonType)
      implements LookupColumnDescription {}

  record AddNew(Column lookupColumn) implements LookupColumnDescription {}
}
