package org.enso.table.read.parsing;

import java.text.DecimalFormat;
import java.text.ParsePosition;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.NumericBuilder;
import org.enso.table.read.parsing.problems.NumericProblemAggregator;

public class DecimalParser extends TypeParser<NumericProblemAggregator> {
  private final char thousandsSeparatorChar;
  private final String thousandsSeparator;
  private final char decimalPoint;
  private final DecimalFormat decimalFormat;
  private final boolean leadingZerosAllowed;

  public DecimalParser(
      final char decimalPoint, final String thousandsSeparator, final boolean leadingZerosAllowed) {
    this.leadingZerosAllowed = leadingZerosAllowed;
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

  @Override
  public Object parseSingleValue(String text, NumericProblemAggregator problemAggregator) {
    if (thousandsSeparator != null
        && (text.startsWith(thousandsSeparator) || text.endsWith(thousandsSeparator))) {
      problemAggregator.reportInvalidFormat(text);
      return null;
    }

    String replaced = thousandsSeparator == null ? text : text.replace(thousandsSeparator, "");
    ParsePosition pos = new ParsePosition(0);
    double result = decimalFormat.parse(replaced, pos).doubleValue();
    if (pos.getIndex() != replaced.length()) {
      problemAggregator.reportInvalidFormat(text);
      return null;
    }

    if (!leadingZerosAllowed && hasLeadingZeros(replaced)) {
      problemAggregator.reportLeadingZeroes(text);
    }

    return result;
  }

  private boolean hasLeadingZeros(String s) {
    int firstDigitPos = 0;
    if (s.charAt(0) == '+' || s.charAt(0) == '-') {
      firstDigitPos = 1;
    }

    return s.charAt(firstDigitPos) == '0'
        && firstDigitPos + 1 < s.length()
        && s.charAt(firstDigitPos + 1) != decimalPoint;
  }

  @Override
  public Builder makeBuilderWithCapacity(long capacity) {
    return NumericBuilder.createLongBuilder((int) capacity);
  }

  @Override
  public NumericProblemAggregator makeProblemAggregator() {
    return new NumericProblemAggregator();
  }
}
