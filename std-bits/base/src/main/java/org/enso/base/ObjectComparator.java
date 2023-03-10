package org.enso.base;

import org.graalvm.polyglot.Value;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.Comparator;
import java.util.Locale;
import java.util.function.BiFunction;
import java.util.function.Function;

public class ObjectComparator implements Comparator<Object> {
  private static ObjectComparator INSTANCE;

  /**
   * A singleton instance of an ObjectComparator.
   *
   * @param fallbackComparator this MUST be the default .compare_to function for Enso. Needs to be
   *     passed to allow calling back from Java.
   * @return Comparator object.
   */
  public static ObjectComparator getInstance(Function<Object, Function<Object, Value>> fallbackComparator) {
    if (INSTANCE == null) {
      INSTANCE = new ObjectComparator(fallbackComparator);
    }

    return INSTANCE;
  }

  private final Function<Object, Function<Object, Value>> fallbackComparator;
  private final Function<String, Function<String, Value>> textComparator;


  public ObjectComparator() {
    this(
        (a) -> (b) -> {
          throw new CompareException(a, b);
        });
  }

  public ObjectComparator(Function<Object, Function<Object, Value>> fallbackComparator) {
    this(fallbackComparator, (a) -> (b) -> Value.asValue(Text_Utils.compare_normalized(a, b)));
  }

  private ObjectComparator(Function<Object, Function<Object, Value>> fallbackComparator, Function<String, Function<String, Value>> textComparator) {
    this.fallbackComparator = fallbackComparator;
    this.textComparator = textComparator;
  }

  /**
   * Create a copy of the ObjectComparator with case-insensitive text comparisons.
   * @param locale to use for case folding.
   * @return Comparator object.
   */
  public ObjectComparator withCaseInsensitivity(Locale locale) {
    return new ObjectComparator(this.fallbackComparator, (a) -> (b) -> Value.asValue(Text_Utils.compare_normalized_ignoring_case(a, b, locale)));
  }

  /**
   * Create a copy of the ObjectComparator with case-insensitive text comparisons.
   * @param textComparator custom comparator for Text.
   * @return Comparator object.
   */
  public ObjectComparator withCustomTextComparator(Function<String, Function<String, Value>> textComparator) {
    return new ObjectComparator(this.fallbackComparator, textComparator);
  }

  @Override
  public int compare(Object thisValue, Object thatValue) throws ClassCastException {
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
      return convertComparatorResult(textComparator.apply(thisString).apply(thatString), thisString, thatString);
    }

    // DateTimes
    if (thisValue instanceof LocalDate thisDate) {
      if (thatValue instanceof LocalDate thatDate) {
        return thisDate.compareTo(thatDate);
      }
    }

    if (thisValue instanceof ZonedDateTime thisDateTime) {
      if (thatValue instanceof ZonedDateTime thatDateTime) {
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
    return convertComparatorResult(fallbackComparator.apply(thisValue).apply(thatValue), thisValue, thatValue);
  }

  private static int convertComparatorResult(Value comparatorResult, Object leftOperand, Object rightOperand) {
    if (comparatorResult.isNumber() && comparatorResult.fitsInInt()) {
      return comparatorResult.asInt();
    } else {
      throw new CompareException(leftOperand, rightOperand);
    }
  }
}
