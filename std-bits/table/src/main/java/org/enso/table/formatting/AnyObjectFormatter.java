package org.enso.table.formatting;

public class AnyObjectFormatter implements DataFormatter {

  private final DataFormatter[] knownFormats;

  public AnyObjectFormatter(DataFormatter[] knownFormats) {
    this.knownFormats = knownFormats;
  }

  @Override
  public String format(Object value) {
    if (value == null) return NULL_REPRESENTATION;

    for (DataFormatter formatter : knownFormats) {
      if (formatter.canFormat(value)) {
        return formatter.format(value);
      }
    }

    return value.toString();
  }

  @Override
  public boolean canFormat(Object value) {
    return true;
  }
}
