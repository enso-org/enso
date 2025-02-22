package org.enso.table.data.column.operation.comparators;

import java.time.LocalDate;
import java.util.function.BiPredicate;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.DateType;

public final class DateComparators extends GenericComparators<LocalDate> {
  public static final DateComparators EQ = new DateComparators(LocalDate::isEqual, false);
  public static final DateComparators NEQ = new DateComparators((a, b) -> !a.isEqual(b), false);
  public static final DateComparators LT = new DateComparators(LocalDate::isBefore);
  public static final DateComparators LTE = new DateComparators((a, b) -> !a.isAfter(b));
  public static final DateComparators GT = new DateComparators(LocalDate::isAfter);
  public static final DateComparators GTE = new DateComparators((a, b) -> !a.isBefore(b));

  private DateComparators(BiPredicate<LocalDate, LocalDate> comparator) {
    this(comparator, true);
  }

  private DateComparators(BiPredicate<LocalDate, LocalDate> comparator, boolean throwOnOther) {
    super(comparator, throwOnOther);
  }

  @Override
  protected ColumnStorage<LocalDate> asTypedStorage(ColumnStorage<?> storage) {
    return DateType.INSTANCE.asTypedStorage(storage);
  }

  @Override
  protected LocalDate asTypedValue(Object value) {
    Object adapted = Polyglot_Utils.convertPolyglotValue(value);
    if (adapted instanceof LocalDate localDate) {
      return localDate;
    }
    return null;
  }

  @Override
  public boolean canApplyMap(ColumnStorage<?> left, Object rightValue) {
    return left.getType() instanceof DateType;
  }

  @Override
  public boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    return left.getType() instanceof DateType
        && (right.getType() instanceof DateType || right.getType() instanceof AnyObjectType);
  }
}
