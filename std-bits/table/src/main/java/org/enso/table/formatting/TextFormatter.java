package org.enso.table.formatting;

public class TextFormatter implements DataFormatter {

  public String format(String value) {
    return value;
  }

  @Override
  public String format(Object value) {
    if (value == null) {
      return NULL_REPRESENTATION;
    }

    if (value instanceof String string) {
      return format(string);
    }

    throw new IllegalArgumentException("Unsupported type for TextFormatter.");
  }

  @Override
  public boolean canFormat(Object value) {
    return value instanceof String;
  }
}
