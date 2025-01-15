package org.enso.table.parsing;

import org.enso.base.parser.FormatDetectingNumberParser;
import org.enso.base.parser.NegativeSign;
import org.enso.base.parser.NumberWithSeparators;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.NumericBuilder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.parsing.problems.CommonParseProblemAggregator;
import org.enso.table.parsing.problems.ParseProblemAggregator;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;

public class NumberParser extends IncrementalDatatypeParser {
  /**
   * Creates a new integer instance of this parser.
   *
   * @param integerTargetType the target type describing how large integer values can be accepted
   * @param trimValues whether to trim the input values
   * @param decimalPoint the decimal point set for the current format, or null if not specified;
   *     this parser does not use decimal point (since it is for integers) but it ensure that if a
   *     decimal point is chosen, the inferred thousand separator will not clash with that specific
   *     decimal point
   * @param thousandSeparator the thousand separator to use (if null then will be inferred)
   */
  public static NumberParser createIntegerParser(
      IntegerType integerTargetType,
      boolean allowSymbol,
      boolean allowLeadingZeroes,
      boolean trimValues,
      String decimalPoint,
      String thousandSeparator) {
    assert integerTargetType != null;
    return new NumberParser(
        integerTargetType,
        allowSymbol,
        allowLeadingZeroes,
        trimValues,
        false,
        decimalPoint,
        thousandSeparator);
  }

  /**
   * Creates a new decimal instance of this parser.
   *
   * @param allowSymbol whether to allow symbols in the input
   * @param allowLeadingZeroes whether to allow leading zeroes in the input
   * @param trimValues whether to trim the input values
   * @param allowExponentialNotation whether to allow exponential notation in the input
   * @param decimalPoint the decimal point set for the current format (if null then will be
   *     inferred)
   * @param thousandSeparator the thousand separator to use (if null then will be inferred)
   */
  public static NumberParser createDecimalParser(
      boolean allowSymbol,
      boolean allowLeadingZeroes,
      boolean trimValues,
      boolean allowExponentialNotation,
      String decimalPoint,
      String thousandSeparator) {
    return new NumberParser(
        null,
        allowSymbol,
        allowLeadingZeroes,
        trimValues,
        allowExponentialNotation,
        decimalPoint,
        thousandSeparator);
  }

  private final IntegerType integerTargetType;

  private final FormatDetectingNumberParser parser;

  private NumberParser(
      IntegerType integerTargetType,
      boolean allowSymbol,
      boolean allowLeadingZeroes,
      boolean allowLeadingTrailingWhitespace,
      boolean allowExponentialNotation,
      String decimalPoint,
      String thousandSeparator) {
    this.integerTargetType = integerTargetType;

    var numberWithSeparators = NumberWithSeparators.fromSeparators(thousandSeparator, decimalPoint);
    this.parser =
        new FormatDetectingNumberParser(
            allowSymbol,
            allowLeadingZeroes,
            allowLeadingTrailingWhitespace,
            allowExponentialNotation,
            NegativeSign.UNKNOWN,
            numberWithSeparators);
  }

  private boolean isInteger() {
    return integerTargetType != null;
  }

  @Override
  protected Builder makeBuilderWithCapacity(int capacity, ProblemAggregator problemAggregator) {
    return isInteger()
        ? NumericBuilder.createLongBuilder(capacity, integerTargetType, problemAggregator)
        : NumericBuilder.createDoubleBuilder(capacity, problemAggregator);
  }

  @Override
  public Storage<?> parseColumn(
      Storage<String> sourceStorage, CommonParseProblemAggregator problemAggregator) {
    Builder builder =
        makeBuilderWithCapacity(sourceStorage.size(), problemAggregator.createSimpleChild());

    var context = Context.getCurrent();
    for (int i = 0; i < sourceStorage.size(); i++) {
      var text = sourceStorage.getItemBoxed(i);

      // Check if in unknown state
      var mightBeEuropean = !isInteger() && parser.numberWithSeparators().mightBeEuropean();

      // Try and parse the value
      var result = text == null ? null : parseSingleValue(text, problemAggregator);

      // Do we need to rescan?
      if (mightBeEuropean && parser.numberWithSeparators() != NumberWithSeparators.DOT_COMMA) {
        builder =
            makeBuilderWithCapacity(sourceStorage.size(), problemAggregator.createSimpleChild());
        for (int j = 0; j < i; j++) {
          var subText = sourceStorage.getItemBoxed(j);
          var subResult = subText == null ? null : parseSingleValue(subText, problemAggregator);
          if (subResult == null) {
            builder.appendNulls(1);
          } else {
            builder.append(subResult);
          }
        }
      }

      // Append the result
      if (result == null) {
        builder.appendNulls(1);
      } else {
        builder.append(result);
      }

      context.safepoint();
    }

    return builder.seal();
  }

  @Override
  public Object parseSingleValue(String text, ParseProblemAggregator problemAggregator) {
    var result = parser.parse(text, isInteger());

    // TODO: Capture the message into the problem aggregator.
    if (result instanceof FormatDetectingNumberParser.NumberParseFailure) {
      problemAggregator.reportInvalidFormat(text);
      return null;
    }

    return switch (result) {
      case FormatDetectingNumberParser.NumberParseDouble doubleResult -> doubleResult.number();
      case FormatDetectingNumberParser.NumberParseLong longResult -> longResult.number();
      default -> throw new IllegalStateException("Unexpected result type: " + result.getClass());
    };
  }
}
