package org.enso.base;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Comparator;
import java.util.function.BiFunction;

public class ObjectComparator implements Comparator<Object> {
  private static ObjectComparator INSTANCE;

  /**
   * A singleton instance of an ObjectComparator
   *
   * @param fallbackComparator this MUST be the default .compare_to function for Enso. Needs to be
   *     passed to allow calling back from Java.
   * @return Comparator object
   */
  public static ObjectComparator getInstance(BiFunction<Object, Object, Long> fallbackComparator) {
    if (INSTANCE == null) {
      INSTANCE = new ObjectComparator((l, r) -> fallbackComparator.apply(l, r).intValue());
    }

    return INSTANCE;
  }

  private final BiFunction<Object, Object, Integer> fallbackComparator;

  public ObjectComparator() {
    this(
        (a, b) -> {
          throw new ClassCastException("Incomparable keys.");
        });
  }

  public ObjectComparator(BiFunction<Object, Object, Integer> fallbackComparator) {
    this.fallbackComparator = fallbackComparator;
  }

  @Override
  public int compare(Object thisValue, Object thatValue) throws ClassCastException {
    // NULLs
    if (thisValue == null) {
      if (thatValue != null) {
        return 1;
      }
      return 0;
    }
    if (thatValue == null) {
      return -1;
    }

    // Booleans
    if (thisValue instanceof Boolean && thatValue instanceof Boolean) {
      boolean thisBool = (Boolean) thisValue;
      boolean thatBool = (Boolean) thatValue;
      if (thisBool == thatBool) {
        return 0;
      }
      return thisBool ? 1 : -1;
    }

    // Long this
    if (thisValue instanceof Long) {
      Long thisLong = (Long) thisValue;
      if (thatValue instanceof Long) {
        return thisLong.compareTo((Long) thatValue);
      }
      if (thatValue instanceof Double) {
        Double thatDouble = (Double) thatValue;
        if (thisLong > thatDouble) {
          return 1;
        }
        if (thisLong < thatDouble) {
          return -1;
        }
        return 0;
      }
    }

    // Double this
    if (thisValue instanceof Double) {
      Double thisDouble = (Double) thisValue;
      if (thatValue instanceof Double) {
        return thisDouble.compareTo((Double) thatValue);
      }
      if (thatValue instanceof Long) {
        Long thatLong = (Long) thatValue;
        if (thisDouble > thatLong) {
          return 1;
        }
        if (thisDouble < thatLong) {
          return -1;
        }
        return 0;
      }
    }

    // Text
    if (thisValue instanceof String && thatValue instanceof String) {
      return Text_Utils.compare_normalized((String) thisValue, (String) thatValue);
    }

    // DateTimes
    if (thisValue instanceof LocalDate) {
      LocalDate thisDate = (LocalDate) thisValue;
      if (thatValue instanceof LocalDate) {
        return thisDate.compareTo((LocalDate) thatValue);
      }
      if (thatValue instanceof LocalDateTime) {
        return thisDate.atStartOfDay().compareTo((LocalDateTime) thatValue);
      }
    }
    if (thisValue instanceof LocalDateTime) {
      LocalDateTime thisDateTime = (LocalDateTime) thisValue;
      if (thatValue instanceof LocalDate) {
        return thisDateTime.compareTo(((LocalDate) thatValue).atStartOfDay());
      }
      if (thatValue instanceof LocalDateTime) {
        return thisDateTime.compareTo((LocalDateTime) thatValue);
      }
    }

    // TimeOfDay
    if (thisValue instanceof LocalTime) {
      LocalTime thisTime = (LocalTime) thisValue;
      if (thatValue instanceof LocalTime) {
        return thisTime.compareTo((LocalTime) thatValue);
      }
    }

    // Fallback to Enso
    return fallbackComparator.apply(thisValue, thatValue);
  }
}
