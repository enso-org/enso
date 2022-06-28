package org.enso.table.formatting;

import java.text.DecimalFormat;

public class IntegerFormatter implements DataFormatter {
  private final DecimalFormat integerFormat;

  public IntegerFormatter(String thousandSeparator) {
    // We use the decimal format, because only it provides the thousand separator.
    integerFormat = new DecimalFormat();
    var symbols = integerFormat.getDecimalFormatSymbols();

    if (thousandSeparator != null) {
      if (thousandSeparator.length() != 1) {
        throw new IllegalArgumentException(
            "The `thousandSeparator` should consist of exactly one code point.");
      } else {
        symbols.setGroupingSeparator(thousandSeparator.charAt(0));
        integerFormat.setGroupingUsed(true);
        integerFormat.setGroupingSize(3);
      }
    } else {
      integerFormat.setGroupingUsed(false);
    }

    integerFormat.setDecimalSeparatorAlwaysShown(false);
    integerFormat.setDecimalFormatSymbols(symbols);
  }

  public String format(long value) {
    return integerFormat.format(value);
  }

  @Override
  public String format(Object value) {
    if (value == null) {
      return NULL_REPRESENTATION;
    }

    if (value instanceof Long integer) {
      return format(integer.longValue());
    }

    throw new IllegalArgumentException("Unsupported type for IntegerFormatter.");
  }

  @Override
  public boolean canFormat(Object value) {
    return value instanceof Long;
  }
}
