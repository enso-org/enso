package org.enso.base.parser;

/**
 * Parse a String into a Number. It supports the following patterns:
 *
 * <ul>
 *   <li>SIGN + NUMBER
 *   <li>SYMBOL + SIGN + NUMBER
 *   <li>SIGN + NUMBER + SYMBOL
 *   <li>SIGN + BRACKETS + NUMBER + BRACKET_CLOSE
 *   <li>BRACKETS + SYMBOL + NUMBER + BRACKET_CLOSE
 *   <li>BRACKETS + NUMBER + SYMBOL + BRACKET_CLOSE
 * </ul>
 */
public class FormatDetectingNumberParser {
  public interface NumberParseResult {}

  public interface NumberParseResultSuccess extends NumberParseResult {
    NumberParseResultSuccess negate();

    NumberParseResultSuccess withSymbol(String symbol);
  }

  public record NumberParseLong(long number, String symbol, boolean negated)
      implements NumberParseResultSuccess {
    @Override
    public NumberParseResultSuccess negate() {
      return new NumberParseLong(-number, symbol, true);
    }

    @Override
    public NumberParseResultSuccess withSymbol(String symbol) {
      return new NumberParseLong(number, symbol, negated);
    }
  }

  public record NumberParseDouble(double number, String symbol)
      implements NumberParseResultSuccess {
    @Override
    public NumberParseResultSuccess negate() {
      return new NumberParseDouble(-number, symbol);
    }

    @Override
    public NumberParseResultSuccess withSymbol(String symbol) {
      return new NumberParseDouble(number, symbol);
    }
  }

  public record NumberParseFailure(String message) implements NumberParseResult {}

  private final boolean allowSymbol;
  private final boolean allowLeadingZeroes;
  private final boolean allowLeadingTrailingWhitespace;
  private final boolean allowExponentialNotation;
  private NegativeSign negativeSign;
  private NumberWithSeparators numberWithSeparators;

  public FormatDetectingNumberParser(
      boolean allowSymbol,
      boolean allowLeadingZeroes,
      boolean allowLeadingTrailingWhitespace,
      boolean allowExponentialNotation,
      NegativeSign negativeSign,
      NumberWithSeparators numberWithSeparators) {
    this.allowSymbol = allowSymbol;
    this.allowLeadingZeroes = allowLeadingZeroes;
    this.allowLeadingTrailingWhitespace = allowLeadingTrailingWhitespace;
    this.allowExponentialNotation = allowExponentialNotation;
    this.negativeSign = negativeSign;
    this.numberWithSeparators = numberWithSeparators;
  }

  public NegativeSign negativeSign() {
    return negativeSign;
  }

  public void setNegativeSign(NegativeSign newNegativeSign) {
    if (negativeSign != NegativeSign.UNKNOWN) {
      throw new IllegalStateException("Negative Sign Already Set.");
    }
    negativeSign = newNegativeSign;
  }

  public NumberWithSeparators numberWithSeparators() {
    return numberWithSeparators;
  }

  /**
   * Parse a string into a number.
   *
   * @param value the string to parse.
   * @param integer whether to parse a Long or a Double.
   * @return the parsed number, or a failure if the parse was unsuccessful.
   */
  public NumberParseResult parse(CharSequence value, boolean integer) {
    // Ensure that if we are allowing exponential notation, we are not parsing an integer.
    assert !(allowExponentialNotation && integer);

    // State
    boolean lastWasWhitespace = false;
    boolean encounteredContent = false;
    boolean encounteredSign = false;
    boolean needsNegating = false;
    NumberParseResultSuccess number = null;
    String symbol = "";

    // Scan the value
    int idx = 0;
    int length = value.length();
    while (idx < length) {
      char c = value.charAt(idx);

      if (Character.isWhitespace(c)) {
        if (!allowLeadingTrailingWhitespace && !encounteredContent) {
          return new NumberParseFailure("Unexpected leading Whitespace.");
        }

        if (idx > 0 && (value.charAt(idx - 1) == '-' || value.charAt(idx - 1) == '+')) {
          return new NumberParseFailure("Unexpected whitespace after sign.");
        }

        idx++;
        lastWasWhitespace = true;
      } else {
        encounteredContent = true;
        lastWasWhitespace = false;

        if (NumberWithSeparators.isDigit(c) || Separators.isSeparator(c)) {
          if (number != null) {
            return new NumberParseFailure("Multiple Number Sections.");
          }

          var numberPart =
              numberWithSeparators.parse(value, idx, integer, allowExponentialNotation);

          // If the format changed, catch new format and unwrap result.
          if (numberPart instanceof NumberWithSeparators.NumberParseResultWithFormat newFormat) {
            numberWithSeparators = newFormat.format();
            numberPart = newFormat.result();
          }

          // Result should either be a new index or a failure.
          // If it is a new index, update the index and unwrap the result.
          if (numberPart instanceof NumberWithSeparators.NumberParseResultWithIndex newIndex) {
            // Check for leading zeroes (0 or 0<decimal> is acceptable).
            if (!allowLeadingZeroes
                && c == '0'
                && newIndex.endIdx() > idx + 1
                && value.charAt(idx + 1) != numberWithSeparators.getDecimal()) {
              return new NumberParseFailure("Leading Zero.");
            }

            idx = newIndex.endIdx();
            numberPart = newIndex.result();
          }

          if (numberPart instanceof NumberParseResultSuccess numberSuccess) {
            number = numberSuccess;
          } else {
            return numberPart;
          }
        } else if (NegativeSign.isOpenSign(c)) {
          if (encounteredSign || number != null) {
            return new NumberParseFailure("Unexpected sign character.");
          }

          var signOk = negativeSign.checkValid(c);
          if (signOk.isEmpty()) {
            return new NumberParseFailure("Inconsistent negative format.");
          }

          negativeSign = signOk.get();
          encounteredSign = true;
          needsNegating = c != '+';
          idx++;
        } else if (c == ')') {
          if (!needsNegating || negativeSign != NegativeSign.BRACKET_OPEN || number == null) {
            return new NumberParseFailure("Unexpected bracket close.");
          }

          // Should only be whitespace left.
          idx++;
          while (idx < length) {
            if (!Character.isWhitespace(value.charAt(idx))) {
              return new NumberParseFailure("Unexpected characters after bracket close.");
            }
            idx++;
            lastWasWhitespace = true;
          }

          // Negate here so can tell finished.
          number = number.negate();
          needsNegating = false;
        } else if (!integer
            && number == null
            && isSameSequence(value, idx, "infinity", "INFINITY")) {
          // Identify Infinity
          number = new NumberParseDouble(Double.POSITIVE_INFINITY, "");
          idx += 8;
        } else if (!integer
            && number == null
            && !encounteredSign
            && !needsNegating
            && isSameSequence(value, idx, "nan", "NAN")) {
          // Identify NaN
          number = new NumberParseDouble(Double.NaN, "");
          idx += 3;
        } else {
          if (!symbol.isEmpty()) {
            return new NumberParseFailure("Multiple Symbol Sections.");
          }

          if (!allowSymbol) {
            return new NumberParseFailure("Symbols not allowed.");
          }

          // ToDo: Locking symbol position within text parts.
          int endIdx = idx;
          while (endIdx < length
              && !NumberWithSeparators.isDigit(c)
              && !Separators.isSeparator(c)
              && !NegativeSign.isSign(c)
              && !Character.isWhitespace(c)) {
            endIdx++;
            if (endIdx < length) {
              c = value.charAt(endIdx);
            }
          }

          symbol = value.subSequence(idx, endIdx).toString();
          idx = endIdx;
        }
      }
    }

    // Check for trailing whitespace.
    if (!allowLeadingTrailingWhitespace && lastWasWhitespace) {
      return new NumberParseFailure("Trailing Whitespace.");
    }

    // Special check for unclosed bracket.
    if (negativeSign == NegativeSign.BRACKET_OPEN && needsNegating) {
      return new NumberParseFailure("Unclosed bracket.");
    }

    // Fail if no number found.
    if (number == null) {
      return new NumberParseFailure("No Number Found.");
    }

    // Return Result
    number = needsNegating ? number.negate() : number;

    // Handle Special Case of Negated 0 If Not An Integer
    if (!integer
        && number instanceof NumberParseLong longNumber
        && longNumber.number() == 0
        && longNumber.negated()) {
      // Catch -0 double.
      number = new NumberParseDouble(-0.0, longNumber.symbol());
    }

    return symbol.isEmpty() ? number : number.withSymbol(symbol);
  }

  public Long parseLong(CharSequence value) {
    var result = parse(value, true);
    if (result instanceof NumberParseLong numberSuccess) {
      return numberSuccess.number();
    }
    return null;
  }

  public Double parseDouble(CharSequence value) {
    var result = parse(value, false);
    if (result instanceof NumberParseDouble numberSuccess) {
      return numberSuccess.number();
    } else if (result instanceof NumberParseLong longNumber) {
      return (double) longNumber.number();
    }
    return null;
  }

  public NumberParseResult[] parseMany(CharSequence[] values, boolean integer) {
    var results = new NumberParseResult[values.length];

    int i = 0;
    while (i < values.length) {
      var previous = numberWithSeparators;
      results[i] = parse(values[i], integer);

      if (numberWithSeparators != previous
          && ((previous == NumberWithSeparators.DOT_UNKNOWN
                  && numberWithSeparators != NumberWithSeparators.DOT_COMMA)
              || (previous == NumberWithSeparators.COMMA_UNKNOWN
                  && numberWithSeparators != NumberWithSeparators.DOT_COMMA))) {
        // Start scan over, as format was incorrect.
        i = 0;
      } else {
        i++;
      }
    }

    return results;
  }

  private static boolean isSameSequence(
      CharSequence sequence, int index, CharSequence toMatchLower, CharSequence toMatchUpper) {
    assert toMatchLower.length() == toMatchUpper.length();
    if (index + toMatchLower.length() > sequence.length()) {
      return false;
    }

    for (int i = 0; i < toMatchLower.length(); i++) {
      char c = sequence.charAt(index + i);
      if (c != toMatchLower.charAt(i) && c != toMatchUpper.charAt(i)) {
        return false;
      }
    }

    return true;
  }
}
