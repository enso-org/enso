package org.enso.table.data.column.operation.comparators;

import java.time.LocalTime;
import java.util.function.BiPredicate;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.TimeOfDayType;

public class TimeOfDayComparators extends GenericComparators<LocalTime> {
  public static final TimeOfDayComparators EQ = new TimeOfDayComparators(LocalTime::equals) {
    @Override
    protected boolean onIncomparable(Object left, Object right) {
      return false;
    }
  };

  public static final TimeOfDayComparators NEQ = new TimeOfDayComparators((a, b) -> !a.equals(b)) {
    @Override
    protected boolean onIncomparable(Object left, Object right) {
      return true;
    }
  };

  public static final TimeOfDayComparators LT = new TimeOfDayComparators(LocalTime::isBefore);
  public static final TimeOfDayComparators LTE = new TimeOfDayComparators((a, b) -> !a.isAfter(b));
  public static final TimeOfDayComparators GT = new TimeOfDayComparators(LocalTime::isAfter);
  public static final TimeOfDayComparators GTE = new TimeOfDayComparators((a, b) -> !a.isBefore(b));

  private TimeOfDayComparators(BiPredicate<LocalTime, LocalTime> comparator) {
    super(comparator);
  }

  @Override
  protected ColumnStorage<LocalTime> asTypedStorage(ColumnStorage<?> storage) {
    return TimeOfDayType.INSTANCE.asTypedStorage(storage);
  }

  @Override
  protected LocalTime asTypedValue(Object value) {
    Object adapted = Polyglot_Utils.convertPolyglotValue(value);
    if (adapted instanceof LocalTime localTime) {
      return localTime;
    }
    return null;
  }

  @Override
  public boolean canApplyMap(ColumnStorage<?> left, Object rightValue) {
    return left.getType() instanceof TimeOfDayType;
  }

  @Override
  public boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    return left.getType() instanceof TimeOfDayType
        && (right.getType() instanceof TimeOfDayType || right.getType() instanceof AnyObjectType);
  }
}
