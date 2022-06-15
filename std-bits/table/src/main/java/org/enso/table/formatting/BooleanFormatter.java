package org.enso.table.formatting;

public class BooleanFormatter implements DataFormatter {
  private final String trueRepresentation;
  private final String falseRepresentation;

  public BooleanFormatter(String trueRepresentation, String falseRepresentation) {
    this.trueRepresentation = trueRepresentation;
    this.falseRepresentation = falseRepresentation;
  }

  public String format(boolean value) {
    if (value) {
      return trueRepresentation;
    } else {
      return falseRepresentation;
    }
  }

  @Override
  public String format(Object value) {
    if (value == null) {
      return NULL_REPRESENTATION;
    }

    if (value instanceof Boolean) {
      return format((boolean) value);
    }

    throw new IllegalArgumentException("Unsupported type for BooleanFormatter.");
  }

  @Override
  public boolean canFormat(Object value) {
    return value instanceof Boolean;
  }
}
