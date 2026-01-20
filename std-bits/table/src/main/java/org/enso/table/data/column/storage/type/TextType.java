package org.enso.table.data.column.storage.type;

import java.util.Objects;
import org.enso.base.Text_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.problems.ProblemAggregator;

public final class TextType implements StorageType<String> {
  public static final TextType VARIABLE_LENGTH = new TextType(-1, false);

  public static TextType fixedLength(long length) {
    return new TextType(length, true);
  }

  public static TextType variableLengthWithLimit(long maxLength) {
    assert maxLength > 0;
    return new TextType(maxLength, false);
  }

  private final long maxLength;
  private final boolean fixedLength;

  private TextType(long maxLength, boolean fixedLength) {
    if (maxLength == 0 || maxLength < -1) {
      throw new IllegalArgumentException(
          "The maxLength of a text type must be positive or -1 to indicate unlimited length.");
    }
    this.maxLength = maxLength;
    this.fixedLength = fixedLength;
  }

  @Override
  public char typeChar() {
    return fixedLength ? 'T' : 'S';
  }

  @Override
  public long size() {
    return maxLength;
  }

  public long maxLength() {
    return maxLength;
  }

  /** Returns if it is a fixed length string. */
  public boolean fixedLength() {
    return fixedLength;
  }

  public boolean fits(String string) {
    if (string == null) {
      return true;
    }

    if (maxLength == -1) {
      return true;
    }

    long length = Text_Utils.grapheme_length(string);
    if (fixedLength) {
      return length == maxLength;
    } else {
      return length <= maxLength;
    }
  }

  /** Truncate or pad the string to make sure that it fits. */
  public String adapt(String string) {
    if (maxLength == -1) {
      return string;
    }

    long textLength = Text_Utils.grapheme_length(string);

    if (textLength > maxLength) {
      return Text_Utils.take_prefix(string, maxLength);
    } else if (fixedLength && textLength < maxLength) {
      return string + " ".repeat(Math.toIntExact(maxLength - textLength));
    } else {
      return string;
    }
  }

  /**
   * Checks if values of otherType can be transferred to this type without any conversions.
   *
   * <p>For example, values of type TextType(3, false) will fit TextType(3, true), but they need to
   * be padded to fit the target type. So this function will return false for such a case.
   */
  public boolean fitsExactly(TextType otherType) {
    if (fixedLength) {
      if (otherType.fixedLength) {
        return maxLength == otherType.maxLength;
      } else {
        return false;
      }
    } else {
      return maxLength == -1 || (otherType.maxLength != -1 && maxLength >= otherType.maxLength);
    }
  }

  public static TextType preciseTypeForValue(String value) {
    return fixedLength(Text_Utils.grapheme_length(value));
  }

  public static TextType maxType(StorageType<?> left, StorageType<?> right) {
    if (left == null && right == null) {
      return VARIABLE_LENGTH;
    } else if (left instanceof TextType leftText) {
      if (right instanceof TextType rightText) {
        return maxType(leftText, rightText);
      }
      return leftText;
    } else if (right instanceof TextType rightText) {
      return rightText;
    } else {
      throw new IllegalArgumentException(
          "Cannot compute max type for non-text types: " + left + ", " + right);
    }
  }

  public static TextType maxType(TextType type1, TextType type2) {
    if (type1.maxLength < 0 || type2.maxLength < 0) {
      return VARIABLE_LENGTH;
    }

    boolean bothFixed = type1.fixedLength && type2.fixedLength;
    if (bothFixed && type1.maxLength == type2.maxLength) {
      return fixedLength(type1.maxLength);
    } else {
      return variableLengthWithLimit(Math.max(type1.maxLength, type2.maxLength));
    }
  }

  public static TextType concatTypes(TextType type1, TextType type2) {
    if (type1.maxLength < 0 || type2.maxLength < 0) {
      return VARIABLE_LENGTH;
    }

    boolean bothFixed = type1.fixedLength && type2.fixedLength;
    long lengthSum = type1.maxLength + type2.maxLength;
    if (lengthSum == 0) {
      return VARIABLE_LENGTH;
    }

    return new TextType(lengthSum, bothFixed);
  }

  @Override
  public boolean isOfType(StorageType<?> other) {
    return other instanceof TextType;
  }

  @Override
  public String valueAsType(Object value) {
    return (value instanceof String s) ? s : null;
  }

  @Override
  public BuilderForType<String> makeBuilder(
      long initialCapacity, ProblemAggregator problemAggregator) {
    return Builder.getForText(this, initialCapacity);
  }

  @Override
  public ColumnStorage<String> asTypedStorage(ColumnStorage<?> storage) {
    if (storage.getType() instanceof TextType) {
      @SuppressWarnings("unchecked")
      var output = (ColumnStorage<String>) storage;
      return output;
    }
    throw new IllegalArgumentException("Storage is not of TextType");
  }

  @Override
  public boolean equals(Object o) {
    if (o == null || getClass() != o.getClass()) return false;
    TextType textType = (TextType) o;
    return maxLength == textType.maxLength && fixedLength == textType.fixedLength;
  }

  @Override
  public int hashCode() {
    return Objects.hash(maxLength, fixedLength);
  }
}
