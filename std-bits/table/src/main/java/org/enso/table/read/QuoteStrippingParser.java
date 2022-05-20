package org.enso.table.read;

import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.builder.object.StringBuilder;
import org.enso.table.parsing.IncrementalDatatypeParser;
import org.enso.table.parsing.problems.ProblemAggregator;

public class QuoteStrippingParser extends IncrementalDatatypeParser {

  private final char quoteCharacter;

  public QuoteStrippingParser(String quote) {
    if (quote.isEmpty()) {
      throw new IllegalArgumentException(
          "Empty quotes are not supported. Set the quote to `Nothing` to disable quotes.");
    }
    if (quote.length() > 1) {
      throw new IllegalArgumentException(
          "Quotes consisting of multiple characters or code units are not supported.");
    }

    quoteCharacter = quote.charAt(0);
  }

  @Override
  protected Object parseSingleValue(String text, ProblemAggregator problemAggregator) {
    QuoteHelper quoteHelper =
        new QuoteHelper(unused -> problemAggregator.reportMismatchedQuote(), quoteCharacter);
    return quoteHelper.stripQuotes(text);
  }

  @Override
  protected Builder makeBuilderWithCapacity(long capacity) {
    return new StringBuilder((int) capacity);
  }
}
