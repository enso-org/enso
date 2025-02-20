package org.enso.base.parser;

import static org.enso.base.parser.NumberWithSeparators.isDigit;

import java.nio.CharBuffer;

/**
 * Record to hold information about the separators found in a number.
 *
 * @param first - the first encountered separator or Constants.NONE if none found.
 * @param second - the second distinct separator or Constants.NONE if none found.
 * @param count - the number of separators found.
 * @param endIdx - the index of the last character in the number.
 * @param lastSeparatorIdx - the index of the last separator found.
 * @param exponential - whether the number is in exponential notation.
 */
public record Separators(
    char first, char second, int count, int endIdx, int lastSeparatorIdx, boolean exponential) {
  /**
   * Strip out the specified separators and replace with just full stop for decimal. If any
   * character other than a digit, thousands or decimal separator is encountered then return null.
   * If multiple decimal separators are encountered then return null.
   */
  static CharSequence strip(
      CharSequence value, int startIdx, int endIdx, char thousands, char decimal) {
    int lastThousand = -1;
    boolean foundDecimal = false;
    char[] results = new char[endIdx - startIdx];
    int resultIdx = 0;
    for (int i = startIdx; i < endIdx; i++) {
      char c = value.charAt(i);
      if (c == decimal) {
        if (foundDecimal) {
          return null;
        }
        if (lastThousand != -1 && i != lastThousand + 4) {
          return null;
        }
        results[resultIdx++] = '.';
        foundDecimal = true;
      } else if (isDigit(c)) {
        results[resultIdx++] = c;
      } else if (c == thousands) {
        // Cannot have thousands post decimal separator.
        if (foundDecimal) {
          return null;
        }

        // Must be 4 away from last thousand separator.
        if (lastThousand != -1) {
          if (i != lastThousand + 4) {
            return null;
          }
        }

        lastThousand = i;
      } else {
        return null;
      }
    }

    if (!foundDecimal && lastThousand != -1 && endIdx != lastThousand + 4) {
      return null;
    }

    return CharBuffer.wrap(results, 0, resultIdx);
  }

  /** Check if the character is a separator. */
  static boolean isSeparator(char c) {
    return c == '.' || c == ',' || c == ' ' || c == '\'' || c == '_';
  }

  /** Check if the character is a decimal separator. */
  private static boolean isDecimalSeparator(char c) {
    return c == '.' || c == ',';
  }

  /** Check if the character is part of the current number. */
  private static boolean validChar(ExponentState exponentState, char c, char first, char second) {
    if (isDigit(c)) {
      return true;
    }

    // If scientific notation is allowed then check for 'e' or 'E'.
    // Can then be followed by a +/- sign.
    if (exponentState == ExponentState.START && (c == 'e' || c == 'E')) {
      return true;
    }

    // Sign can only be encountered after an E/e in scientific notation.
    if (exponentState == ExponentState.E_SIGN && (c == '+' || c == '-')) {
      return true;
    }

    // Separators not valid in scientific notation if not in start.
    if (exponentState != ExponentState.START && exponentState != ExponentState.NOT_ALLOWED) {
      return false;
    }

    // We haven't encountered a separator yet, so valid if it is a separator.
    if (first == NumberWithSeparators.Constants.NONE) {
      return isSeparator(c);
    }

    // We have encountered the first separator, so valid if it is the same as
    // the first or a decimal separator.
    if (second == NumberWithSeparators.Constants.NONE) {
      return c == first || isDecimalSeparator(c);
    }

    // We have encountered the second separator, so invalid to encounter another
    // separator.
    return false;
  }

  /**
   * Find the number and separators section. Validate the spacing of separators. Return the
   * separators found or null if invalid.
   *
   * @param value the value to parse.
   * @param idx the index to start parsing from.
   * @param integer if the number is an integer.
   * @param allowExponentialNotation is exponential notation allowed.
   */
  static Separators parse(
      CharSequence value, int idx, boolean integer, boolean allowExponentialNotation) {
    int endIdx = idx;
    char firstSeparator = NumberWithSeparators.Constants.NONE;
    char secondSeparator = NumberWithSeparators.Constants.NONE;

    boolean firstWasSeparator = false;
    int lastSeparator = -1;
    int separatorCount = 0;

    // Set initial state for exponential notation.
    ExponentState exponentState =
        !integer && allowExponentialNotation ? ExponentState.START : ExponentState.NOT_ALLOWED;

    // Scan the text, find and validate spacing of separators.
    // Space and ' are both valid thousands separators, but can't be second separator.
    for (endIdx = idx; endIdx < value.length(); endIdx++) {
      char c = value.charAt(endIdx);
      if (!validChar(exponentState, c, firstSeparator, secondSeparator)) {
        break;
      }

      // Cope with digits or scientific notation.
      if (isDigit(c) || c == 'e' || c == 'E' || c == '+' || c == '-') {
        // Update Exponent State.
        if (c == 'e' || c == 'E') {
          exponentState = ExponentState.E_SIGN;
        } else if (c == '+' || c == '-') {
          exponentState = ExponentState.SIGN;
        } else if (exponentState == ExponentState.SIGN || exponentState == ExponentState.E_SIGN) {
          exponentState = ExponentState.EXPONENT;
        }

        continue;
      }

      // If first digit is a separator then only valid if a decimal separator.
      if (endIdx == idx) {
        if (integer || !isDecimalSeparator(c)) {
          return null;
        }
        firstWasSeparator = true;
      }

      if (firstSeparator == NumberWithSeparators.Constants.NONE) {
        // Found the first separator.
        firstSeparator = c;
      } else {
        // TODO: This check is probably now redundant as strip does it as well.
        // Encountered another separator -  must be 4 away from last separator.
        if (endIdx != lastSeparator + 4) {
          // Special case if last was a space as could be separating symbol.
          if (c == ' ') {
            break;
          }
          return null;
        }

        // Must have been a decimal separator.
        if (firstWasSeparator) {
          return null;
        }

        // Encountered a second separator, only valid if !integer.
        if (firstSeparator != c) {
          if (!integer) {
            secondSeparator = c;
          } else {
            return null;
          }
        }
      }

      lastSeparator = endIdx;
      separatorCount++;
    }

    // Special case when firstSeparator is a space and no secondSeparator and ending with a space.
    if (firstSeparator == ' ' && value.charAt(endIdx - 1) == ' ') {
      separatorCount--;
      endIdx--;
      lastSeparator -= 4;
      if (separatorCount == 0) {
        firstSeparator = NumberWithSeparators.Constants.NONE;
      }
    }

    // If in integer mode then must be a thousand separator, validate final spacing.
    if (integer && separatorCount > 0 && lastSeparator != endIdx - 4) {
      return null;
    }

    return new Separators(
        firstSeparator,
        secondSeparator,
        separatorCount,
        endIdx,
        lastSeparator,
        exponentState == ExponentState.EXPONENT);
  }

  private enum ExponentState {
    /** Scientific notation not allowed. */
    NOT_ALLOWED,
    /** Have not encountered an E/e yet. */
    START,
    /** Have encountered an E/e. */
    E_SIGN,
    /** Have encountered an E/e and a sign. */
    SIGN,
    /** Have encountered an E/e, a sign and a digit. */
    EXPONENT
  }
}
