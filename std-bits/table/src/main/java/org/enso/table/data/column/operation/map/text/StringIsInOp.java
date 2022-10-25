package org.enso.table.data.column.operation.map.text;

import org.enso.table.data.column.operation.map.SpecializedIsInOp;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;

import java.util.HashSet;
import java.util.List;

public class StringIsInOp<S extends Storage<String>> extends SpecializedIsInOp<String, S> {
  @Override
  protected CompactRepresentation<String> prepareList(List<?> list) {
    HashSet<String> set = new HashSet<>();
    boolean hasNulls = false;
    for (Object o : list) {
      hasNulls |= o == null;
      if (o instanceof String s) {
        set.add(s);
      }
    }
    return new CompactRepresentation<>(set, hasNulls);
  }
}
