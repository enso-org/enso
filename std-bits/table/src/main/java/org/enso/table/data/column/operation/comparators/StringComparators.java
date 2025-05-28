package org.enso.table.data.column.operation.comparators;

import java.util.function.BiPredicate;
import org.enso.base.Text_Utils;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.TextType;

public class StringComparators extends GenericComparators<String> {
  public static final StringComparators EQ = new StringComparators(Text_Utils::equals) {
    @Override
    protected boolean onIncomparable(Object left, Object right) {
      return false;
    }
  };

  public static final StringComparators NEQ = new StringComparators((a, b) -> !Text_Utils.equals(a, b)) {
    @Override
    protected boolean onIncomparable(Object left, Object right) {
      return true;
    }
  };

  public static final StringComparators LT =
      new StringComparators((a, b) -> Text_Utils.compare_normalized(a, b) < 0);
  public static final StringComparators LTE =
      new StringComparators((a, b) -> Text_Utils.compare_normalized(a, b) <= 0);
  public static final StringComparators GT =
      new StringComparators((a, b) -> Text_Utils.compare_normalized(a, b) > 0);
  public static final StringComparators GTE =
      new StringComparators((a, b) -> Text_Utils.compare_normalized(a, b) >= 0);

  protected StringComparators(BiPredicate<String, String> comparator) {
    super(comparator);
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
    return left.getType() instanceof TextType;
  }

  @Override
  public boolean canApplyZip(ColumnStorage<?> left, ColumnStorage<?> right) {
    return left.getType() instanceof TextType
        && (right.getType() instanceof TextType || right.getType() instanceof AnyObjectType);
  }
}
