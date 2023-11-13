package org.enso.table.data.table.join.conditions;

import org.enso.table.data.table.Column;

public record Between(Column left, Column rightLower, Column rightUpper) implements JoinCondition {}
