package org.enso.table.data.table.join;

import org.enso.table.data.table.Column;

import java.util.Locale;

public record EqualsIgnoreCase(Column left, Column right, Locale locale) implements JoinCondition {}
