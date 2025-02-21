package org.enso.table.data.column.operation.comparators;

import java.time.ZonedDateTime;
import java.util.function.BiPredicate;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.DateTimeType;

public final class DateTimeComparators extends GenericComparators<ZonedDateTime> {
  public static final DateTimeComparators EQ =
      new DateTimeComparators(ZonedDateTime::isEqual, false);
  public static final DateTimeComparators NEQ =
      new DateTimeComparators((a, b) -> !a.equals(b), false);
  public static final DateTimeComparators LT = new DateTimeComparators(ZonedDateTime::isBefore);
  public static final DateTimeComparators LTE = new DateTimeComparators((a, b) -> !a.isAfter(b));
  public static final DateTimeComparators GT = new DateTimeComparators(ZonedDateTime::isAfter);
  public static final DateTimeComparators GTE = new DateTimeComparators((a, b) -> !a.isBefore(b));

  private DateTimeComparators(BiPredicate<ZonedDateTime, ZonedDateTime> comparator) {
    this(comparator, true);
  }

  private DateTimeComparators(
      BiPredicate<ZonedDateTime, ZonedDateTime> comparator, boolean throwOnOther) {
    super(comparator, throwOnOther);
  }

  @Override
  protected ColumnStorage<ZonedDateTime> asTypedStorage(ColumnStorage<?> storage) {
    return DateTimeType.INSTANCE.asTypedStorage(storage);
  }

  @Override
  protected ZonedDateTime asTypedValue(Object value) {
    Object adapted = Polyglot_Utils.convertPolyglotValue(value);
    if (adapted instanceof ZonedDateTime zonedDateTime) {
      return zonedDateTime;
    }
    return null;
  }

  @Override
  public boolean canApplyMap(ColumnStorage<?> left, Object rightValue) {
    return left.getType() instanceof DateTimeType;
  }

  @Override
  public boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    return left.getType() instanceof DateTimeType
        && (right.getType() instanceof DateTimeType || right.getType() instanceof AnyObjectType);
  }
}
