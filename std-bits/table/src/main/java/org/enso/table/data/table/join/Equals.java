package org.enso.table.data.table.join;

import org.enso.table.data.table.Column;

public record Equals(Column left, Column right) implements JoinCondition {}
