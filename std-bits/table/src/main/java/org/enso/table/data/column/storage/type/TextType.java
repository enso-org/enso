package org.enso.table.data.column.storage.type;

import org.enso.base.Text_Utils;

public record TextType(long maxLength, boolean fixedLength) implements StorageType {
  public static final TextType VARIABLE_LENGTH = new TextType(-1, false);

  public static TextType fixedLength(long length) {
    return new TextType(length, true);
  }

  public static TextType variableLengthWithLimit(long maxLength) {
    assert maxLength >= 0;
    return new TextType(maxLength, false);
  }

  public boolean fits(String string) {
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
   * <p>
   * For example, values of type TextType(3, false) will fit TextType(3, true), but they need to be padded to fit the
   * target type. So this function will return false for such a case.
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
    return new TextType(lengthSum, bothFixed);
  }
}
