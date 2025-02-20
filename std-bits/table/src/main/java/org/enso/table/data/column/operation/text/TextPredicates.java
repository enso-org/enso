package org.enso.table.data.column.operation.text;

import java.util.function.BiPredicate;
import org.enso.base.Text_Utils;
import org.enso.table.data.column.operation.comparators.Comparators;
import org.enso.table.data.column.operation.comparators.GenericComparators;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.Column;
import org.enso.table.error.UnexpectedTypeException;

public final class TextPredicates extends GenericComparators<String> {
  public static final TextPredicates STARTS_WITH = new TextPredicates(Text_Utils::starts_with);
  public static final TextPredicates ENDS_WITH = new TextPredicates(Text_Utils::ends_with);
  public static final TextPredicates CONTAINS = new TextPredicates(Text_Utils::contains);

  private TextPredicates(BiPredicate<String, String> predicate) {
    super(predicate, true);
  }

  public Column apply(Column left, Object right, String newName) {
    var leftStorage = left.getStorage();
    if (leftStorage.getType() instanceof NullType) {
      return new Column(newName, BoolStorage.makeEmpty(leftStorage.getSize()));
    }
    return Comparators.performComparison(leftStorage, right, newName, this);
  }

  @Override
  protected RuntimeException makeCompareError(Object left, Object right) {
    return new UnexpectedTypeException("a Text", right.toString());
  }

  @Override
  protected ColumnStorage<String> asTypedStorage(ColumnStorage<?> storage) {
    return TextType.VARIABLE_LENGTH.asTypedStorage(storage);
  }

  @Override
  protected String asTypedValue(Object value) {
    if (value instanceof String stringValue) {
      return stringValue;
    }
    return null;
  }

  @Override
  public boolean canApplyMap(ColumnStorage<?> left, Object rightValue) {
    var storageType = left.getType();
    return storageType instanceof TextType || storageType instanceof NullType;
  }

  @Override
  public boolean canApply(ColumnStorage<?> left, ColumnStorage<?> right) {
    return canApplyMap(left, null) && canApplyMap(right, null);
  }
}
