package org.enso.table.util;

import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TimeOfDayType;

public class Polyglot_Helper {
  public static boolean isPolyglotConversionNeeded(
      StorageType expectedType, boolean expectDataflowErrors) {
    if (expectDataflowErrors) {
      return true;
    }

    if (expectedType == null) {
      return true;
    }

    boolean mayHoldDates =
        expectedType instanceof AnyObjectType
            || expectedType instanceof DateType
            || expectedType instanceof DateTimeType
            || expectedType instanceof TimeOfDayType;
    return mayHoldDates;
  }
}
