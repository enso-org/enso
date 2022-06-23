package org.enso.table.formatting;

import java.text.DecimalFormat;

public class DecimalFormatter implements DataFormatter {
  private final DecimalFormat decimalFormat;
  public static final String INFINITY = "Infinity";

  public DecimalFormatter(String thousandSeparator, String decimalPoint) {
    decimalFormat = new DecimalFormat();
    var symbols = decimalFormat.getDecimalFormatSymbols();

    if (decimalPoint.length() != 1) {
      throw new IllegalArgumentException(
          "The `decimalPoint` should consist of exactly one code point.");
    } else {
      symbols.setDecimalSeparator(decimalPoint.charAt(0));
    }

    if (thousandSeparator != null) {
      if (thousandSeparator.length() != 1) {
        throw new IllegalArgumentException(
            "The `thousandSeparator` should consist of exactly one code point.");
      } else {
        symbols.setGroupingSeparator(thousandSeparator.charAt(0));
        decimalFormat.setGroupingUsed(true);
        decimalFormat.setGroupingSize(3);
      }
    } else {
      decimalFormat.setGroupingUsed(false);
    }

    symbols.setInfinity(INFINITY);
    decimalFormat.setDecimalFormatSymbols(symbols);
    decimalFormat.setDecimalSeparatorAlwaysShown(true);
    decimalFormat.setMaximumFractionDigits(Integer.MAX_VALUE);
    decimalFormat.setMinimumFractionDigits(1);
  }

  public String format(double value) {
    return decimalFormat.format(value);
  }

  @Override
  public String format(Object value) {
    if (value == null) {
      return NULL_REPRESENTATION;
    }

    if (value instanceof Double decimal) {
      return format(decimal.doubleValue());
    }

    throw new IllegalArgumentException("Unsupported type for DecimalFormatter.");
  }

  @Override
  public boolean canFormat(Object value) {
    return value instanceof Double;
  }
}
