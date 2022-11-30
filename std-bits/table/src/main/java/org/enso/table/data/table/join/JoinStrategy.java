package org.enso.table.data.table.join;

import org.enso.table.data.table.Table;

import java.util.List;

public interface JoinStrategy {
  JoinResult join(Table left, Table right, List<JoinCondition> conditions);
}
