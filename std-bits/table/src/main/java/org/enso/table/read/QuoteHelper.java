package org.enso.table.read;

import java.util.function.Consumer;

/**
 * A helper class providing functionality of parsing optionally quoted strings and detecting
 * mismatched quotes.
 *
 * <p>The tool is meant for use with the output of the {@code CsvParser} with the setting {@code
 * keepQuotes} set to true. The strings which were quoted will keep the enclosing quotes, but any
 * escaped quotes within the string are correctly parsed (for example, if the escaped quote is `\"`,
 * it will appear as already parsed `"` quote inside of the string, so no escape processing is
 * performed at this stage, because it was done earlier by the parser.
 */
public class QuoteHelper {

  /**
   * Strips enclosing quotes from the string, if they are present.
   *
   * <p>If the quote appears only at one end of the string, a mismatched quote callback is called to
   * report the issue.
   */
  public static String stripQuotes(
      char quoteCharacter, Consumer<String> mismatchedQuoteCallback, String text) {
    int last = text.length() - 1;
    if (text.isEmpty()
        || (text.charAt(0) != quoteCharacter && text.charAt(last) != quoteCharacter)) {
      // There are no quotes to strip.
      return text;
    }

    boolean hasLeadingQuote = text.charAt(0) == quoteCharacter;

    // Trailing quote is counted only if it is not the same string position as the leading quote.
    boolean hasTrailingQuote = text.length() >= 2 && text.charAt(last) == quoteCharacter;

    if (hasLeadingQuote && hasTrailingQuote) {
      // Strip quotes.
      return text.substring(1, text.length() - 1);
    } else {
      assert hasLeadingQuote || hasTrailingQuote;
      // We report mismatched quotes only at the beginning, if it is at the end it is treated as
      // literal quote character.
      if (hasLeadingQuote && !hasTrailingQuote) {
        mismatchedQuoteCallback.accept(text);
      }
      return text;
    }
  }

  public static boolean hasMismatchedQuotes(char quoteCharacter, String text) {
    if (text.isEmpty()) {
      return false;
    }

    boolean hasLeadingQuote = text.charAt(0) == quoteCharacter;
    boolean hasTrailingQuote =
        text.length() >= 2 && text.charAt(text.length() - 1) == quoteCharacter;

    return hasLeadingQuote && !hasTrailingQuote;
  }
}
