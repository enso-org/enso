package org.enso.table.data.index;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Arrays;

public class MultiValueKey implements Comparable<MultiValueKey> {
  private final Object[] values;
  private final int hashCodeValue;
  private final boolean allNull;
  private final boolean floatValue;

  public MultiValueKey(Object[] values) {
    this.values = values;

    boolean allNull = true;
    boolean floatValue = false;

    // Precompute HashCode - using Apache.Commons.Collections.Map.MultiKeyMap.hash algorithm
    int h = 0;
    for (Object value: this.values) {
      if (value != null) {
        Object folded = foldObject(value);
        floatValue = floatValue || (folded instanceof Double);
        h ^= folded.hashCode();
        allNull = false;
      }
    }
    h += ~(h << 9);
    h ^= h >>> 14;
    h += h << 4;

    this.hashCodeValue = h ^ (h >>> 10);
    this.allNull = allNull;
    this.floatValue = floatValue;
  }

  @Override
  public int hashCode() {
    return this.hashCodeValue;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    MultiValueKey that = (MultiValueKey) o;
    return hashCodeValue == that.hashCodeValue && Arrays.equals(values, that.values);
  }

  public boolean areAllNull() {
    return allNull;
  }

  public boolean hasFloatValues() { return floatValue; }

  protected static Object foldObject(Object value) {
    if (value instanceof Long) {
      return value;
    } else if (value instanceof Integer) {
      return ((Integer)value).longValue();
    } else if (value instanceof Byte) {
      return ((Byte)value).longValue();
    } else if (value instanceof Float && ((Float)value) % 1 == 0) {
      return ((Float)value).longValue();
    } else if (value instanceof Double && ((Double)value) % 1 == 0) {
      return ((Double)value).longValue();
    } else if (value instanceof Float) {
      return ((Float)value).doubleValue();
    } else if (value instanceof Double) {
      return value;
    }

    return value;
  }

  public static int compareObjects(Object thisValue, Object thatValue) throws ClassCastException {
    // NULLs
    if (thisValue == null) {
      if (thatValue != null) {
        return -1;
      }
      return 0;
    }
    if (thatValue == null) {
      return 1;
    }

    // Booleans
    if (thisValue instanceof Boolean && thatValue instanceof Boolean) {
      boolean thisBool = (Boolean)thisValue;
      boolean thatBool = (Boolean)thatValue;
      if (thisBool == thatBool) {
        return 0;
      }
      return thisBool ? 1 : -1;
    }

    // Long this
    if (thisValue instanceof Long) {
      Long thisLong = (Long)thisValue;
      if (thatValue instanceof Long) {
        return thisLong.compareTo((Long)thatValue);
      }
      if (thatValue instanceof Double) {
        Double thatDouble = (Double)thatValue;
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
      Double thisDouble = (Double)thisValue;
      if (thatValue instanceof Double) {
        return thisDouble.compareTo((Double)thatValue);
      }
      if (thatValue instanceof Long) {
        Long thatLong = (Long)thatValue;
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
      // Needs to use ICU comparisons
      return ((String)thisValue).compareTo((String)thatValue);
    }

    // DateTimes
    if (thisValue instanceof LocalDate) {
      LocalDate thisDate = (LocalDate)thisValue;
      if (thatValue instanceof LocalDate) {
        return thisDate.compareTo((LocalDate)thatValue);
      }
      if (thatValue instanceof LocalDateTime) {
        return thisDate.atStartOfDay().compareTo((LocalDateTime)thatValue);
      }
    }
    if (thisValue instanceof LocalDateTime) {
      LocalDateTime thisDateTime = (LocalDateTime)thisValue;
      if (thatValue instanceof LocalDate) {
        return thisDateTime.compareTo(((LocalDate)thatValue).atStartOfDay());
      }
      if (thatValue instanceof LocalDateTime) {
        return thisDateTime.compareTo((LocalDateTime)thatValue);
      }
    }

    // TimeOfDay
    if (thisValue instanceof LocalTime) {
      LocalTime thisTime = (LocalTime)thisValue;
      if (thatValue instanceof LocalTime) {
        return thisTime.compareTo((LocalTime)thatValue);
      }
    }

    // Give Up!
    throw new ClassCastException("Incomparable keys.");
  }

  @Override
  public int compareTo(MultiValueKey that) {
    if (that == null) {
      throw new NullPointerException();
    }

    if (that.values.length != values.length) {
      throw new ClassCastException("Incomparable keys.");
    }

    for (int i = 0; i < values.length; i++) {
      int comparison = compareObjects(values[i], that.values[i]);
      if (comparison != 0) {
        return comparison;
      }
    }

    return 0;
  }
}
