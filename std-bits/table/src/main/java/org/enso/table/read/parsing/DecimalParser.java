package org.enso.table.read.parsing;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.text.ParseException;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.NumericBuilder;

public class DecimalParser extends TypeParser {
  private final char thousandsSeparatorChar;
  private final String thousandsSeparator;
  private final char decimalPoint;
  private final DecimalFormat decimalFormat;

  public DecimalParser(final char decimalPoint, final String thousandsSeparator) {
    this.decimalPoint = decimalPoint;

    if (thousandsSeparator != null) {
      if (thousandsSeparator.length() != 1) {
        throw new IllegalArgumentException(
            "The `thousandsSeparator` should consist of exactly one code point.");
      }

      this.thousandsSeparator = thousandsSeparator;
      thousandsSeparatorChar = thousandsSeparator.charAt(0);
    } else {
      this.thousandsSeparator = null;
      thousandsSeparatorChar = '\0';
    }

    decimalFormat = new DecimalFormat();
    var symbols = decimalFormat.getDecimalFormatSymbols();
    symbols.setDecimalSeparator(decimalPoint);
    decimalFormat.setDecimalFormatSymbols(symbols);
  }

  public Object parseSingleValue(String text) {
    for (int i = 0; i < text.length(); ++i) {
      char c = text.charAt(i);
      boolean ok =
          ('0' <= c && c <= '9') || (thousandsSeparator != null && c == thousandsSeparatorChar);
      if (!ok) return INVALID_FORMAT;
    }

    String replaced = thousandsSeparator == null ? text : text.replace(thousandsSeparator, "");
    try {
      return decimalFormat.parse(replaced);
    } catch (ParseException e) {
      return INVALID_FORMAT;
    }
  }

  @Override
  public Builder makeBuilderWithCapacity(long capacity) {
    return NumericBuilder.createLongBuilder((int) capacity);
  }
}
