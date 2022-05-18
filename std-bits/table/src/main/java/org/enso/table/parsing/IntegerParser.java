package org.enso.table.parsing;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.NumericBuilder;
import org.enso.table.parsing.problems.NumericProblemAggregator;

public class IntegerParser extends DatatypeParser<NumericProblemAggregator> {
  private final String thousandsSeparator;
  private final boolean leadingZerosAllowed;

  public IntegerParser(final String thousandsSeparator, final boolean leadingZerosAllowed) {
    this.leadingZerosAllowed = leadingZerosAllowed;
    if (thousandsSeparator != null && thousandsSeparator.length() != 1) {
      throw new IllegalArgumentException(
          "The `thousandsSeparator` should consist of exactly one code point.");
    }
    this.thousandsSeparator = thousandsSeparator;
  }

  @Override
  public Object parseSingleValue(String text, NumericProblemAggregator problemAggregator) {
    if (thousandsSeparator != null
        && (text.startsWith(thousandsSeparator) || text.endsWith(thousandsSeparator))) {
      problemAggregator.reportInvalidFormat(text);
      return null;
    }

    String replaced = thousandsSeparator == null ? text : text.replace(thousandsSeparator, "");
    try {
      long value = Long.parseLong(replaced);

      if (!leadingZerosAllowed && hasLeadingZeros(replaced)) {
        problemAggregator.reportLeadingZeroes(text);
        return null;
      }

      return value;
    } catch (NumberFormatException exception) {
      problemAggregator.reportInvalidFormat(text);
      return null;
    }
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

    return s.charAt(firstDigitPos) == '0' && firstDigitPos + 1 < s.length();
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
