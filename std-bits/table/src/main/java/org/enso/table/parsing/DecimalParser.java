package org.enso.table.parsing;

import java.text.DecimalFormat;
import java.text.ParsePosition;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.NumericBuilder;
import org.enso.table.parsing.problems.NumericProblemAggregator;

public class DecimalParser extends DatatypeParser<NumericProblemAggregator> {
  private final String thousandsSeparator;
  private final char decimalPoint;
  private final DecimalFormat decimalFormat;
  private final boolean leadingZerosAllowed;

  public DecimalParser(
      final String decimalPoint,
      final String thousandsSeparator,
      final boolean leadingZerosAllowed) {
    this.leadingZerosAllowed = leadingZerosAllowed;

    if (decimalPoint.length() != 1) {
      throw new IllegalArgumentException(
          "The `decimalPoint` should consist of exactly one code point.");
    } else {
      this.decimalPoint = decimalPoint.charAt(0);
    }

    if (thousandsSeparator != null && thousandsSeparator.length() != 1) {
      throw new IllegalArgumentException(
          "The `thousandsSeparator` should consist of exactly one code point.");
    }
    this.thousandsSeparator = thousandsSeparator;

    decimalFormat = new DecimalFormat();
    var symbols = decimalFormat.getDecimalFormatSymbols();
    symbols.setDecimalSeparator(this.decimalPoint);
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

    // If the number starts with a plus, we need to remove it because DecimalFormat does not like
    // it. But we also ensure that we do not let through a "+-" by accident.
    if (replaced.length() >= 2 && replaced.charAt(0) == '+' && replaced.charAt(1) != '-') {
      replaced = replaced.substring(1);
    }

    ParsePosition pos = new ParsePosition(0);
    Number result = decimalFormat.parse(replaced, pos);
    if (result == null || pos.getIndex() != replaced.length()) {
      problemAggregator.reportInvalidFormat(text);
      return null;
    }

    if (!leadingZerosAllowed && hasLeadingZeros(replaced)) {
      problemAggregator.reportLeadingZeroes(text);
      return null;
    }

    return result.doubleValue();
  }

  /**
   * Assumes that the provided string represents a valid integer, in particular that it is not
   * empty.
   */
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
    return NumericBuilder.createDoubleBuilder((int) capacity);
  }

  @Override
  public NumericProblemAggregator makeProblemAggregator() {
    return new NumericProblemAggregator();
  }
}
