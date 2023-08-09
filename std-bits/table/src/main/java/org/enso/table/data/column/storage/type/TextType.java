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
