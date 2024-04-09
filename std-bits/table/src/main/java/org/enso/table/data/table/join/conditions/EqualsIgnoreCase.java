package org.enso.table.data.table.join.conditions;

import java.util.Locale;
import org.enso.table.data.table.Column;

public record EqualsIgnoreCase(Column left, Column right, Locale locale)
    implements HashableCondition {}
