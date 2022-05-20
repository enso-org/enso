package org.enso.table.read;

import java.util.function.Consumer;

public class QuoteHelper {
  private final Consumer<String> mismatchedQuoteCallback;
  private final char quoteCharacter;

  public QuoteHelper(Consumer<String> mismatchedQuoteCallback, char quoteCharacter) {
    this.mismatchedQuoteCallback = mismatchedQuoteCallback;
    this.quoteCharacter = quoteCharacter;
  }

  public String stripQuotes(String text) {
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
      // If only leading or only trailing quote is left, it means we have a mismatched quote and
      // need to report it.
      mismatchedQuoteCallback.accept(text);
      return text;
    }
  }
}
