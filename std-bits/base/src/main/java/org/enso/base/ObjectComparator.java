package org.enso.base;

import org.graalvm.collections.Equivalence;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;

public class ObjectComparator extends Equivalence implements Comparator<Object> {
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

  @Override
  public boolean equals(Object a, Object b) {
    return this.compare(a, b) == 0;
  }

  @Override
  public int hashCode(Object o) {
    int typeHash;
    int valueHash;

    if (o instanceof Double dblValue && dblValue.longValue() == dblValue) {
      Long lngValue = dblValue.longValue();
      typeHash = lngValue.getClass().hashCode();
      valueHash = lngValue.hashCode();
    } else if (o instanceof String string) {
      String normal = Text_Utils.normalize(string);
      typeHash = normal.getClass().hashCode();
      valueHash = normal.hashCode();
    } else if (o instanceof Map m) {
      // Polyglot Scenarios - Handle Vector specially otherwise map over !null keys
      if (m.containsKey("to_array") && m.get("to_array") instanceof List l) {
        typeHash = l.getClass().hashCode();
        valueHash = 1;
        for (Object v : l) {
          if (v != null) {
            valueHash = (((valueHash << 5) + valueHash) + this.hashCode(v));
          }
        }
      } else {
        typeHash = o.getClass().hashCode();
        valueHash = 1;
        for (Object k : m.keySet()) {
          Object v = m.get(k);
          if (v != null) {
            int entryHash = k.hashCode() ^ this.hashCode(v);
            valueHash = (((valueHash << 5) + valueHash) + entryHash);
          }
        }
      }
    } else {
      typeHash = o.getClass().hashCode();
      valueHash = o.hashCode();
    }

    return (((typeHash << 5) + typeHash) ^ valueHash);
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
    if (thisValue instanceof Boolean thisBool && thatValue instanceof Boolean thatBool) {
      if (thisBool == thatBool) {
        return 0;
      }
      return thisBool ? 1 : -1;
    }

    // Long this
    if (thisValue instanceof Long thisLong) {
      if (thatValue instanceof Long thatLong) {
        return thisLong.compareTo(thatLong);
      }
      if (thatValue instanceof Double thatDouble) {
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
    if (thisValue instanceof Double thisDouble) {
      if (thatValue instanceof Double thatDouble) {
        return thisDouble.compareTo(thatDouble);
      }
      if (thatValue instanceof Long thatLong) {
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
    if (thisValue instanceof String thisString && thatValue instanceof String thatString) {
      return Text_Utils.compare_normalized(thisString, thatString);
    }

    // DateTimes
    if (thisValue instanceof LocalDate thisDate) {
      if (thatValue instanceof LocalDate thatDate) {
        return thisDate.compareTo(thatDate);
      }
      if (thatValue instanceof LocalDateTime thatDateTime) {
        return thisDate.atStartOfDay().compareTo(thatDateTime);
      }
    }
    if (thisValue instanceof LocalDateTime thisDateTime) {
      if (thatValue instanceof LocalDate thatDate) {
        return thisDateTime.compareTo(thatDate.atStartOfDay());
      }
      if (thatValue instanceof LocalDateTime thatDateTime) {
        return thisDateTime.compareTo(thatDateTime);
      }
    }

    // TimeOfDay
    if (thisValue instanceof LocalTime thisTime) {
      if (thatValue instanceof LocalTime thatTime) {
        return thisTime.compareTo(thatTime);
      }
    }

    // Fallback to Enso
    return fallbackComparator.apply(thisValue, thatValue);
  }
}
