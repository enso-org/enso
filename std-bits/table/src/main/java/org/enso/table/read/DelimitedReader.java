package org.enso.table.read;

import java.io.InputStream;
import org.enso.table.data.table.Table;

public class DelimitedReader {
  public static enum Headers {
    INFER,
    USE_FIRST_ROW_AS_HEADERS,
    GENERATE_HEADERS
  }

  private final char delimiter;
  private final char quoteCharacter;
  private final Headers headers;

  private static final char noQuoteCharacter = '\0';

  boolean hasQuote() {
    return quoteCharacter != noQuoteCharacter;
  }

  public DelimitedReader(String delimiter, String quote, Headers headers, int skipRows, int rowLimit) {
    if (delimiter.isEmpty()) {
      throw new IllegalArgumentException("Empty delimiters are not supported.");
    }
    if (delimiter.length() > 1) {
      throw new IllegalArgumentException(
          "Delimiters consisting of multiple characters or code units are not supported.");
    }

    this.delimiter = delimiter.charAt(0);

    if (quote != null) {
      if (quote.isEmpty()) {
        throw new IllegalArgumentException(
            "Empty quotes are not supported. Set the quote to `Nothing` to disable quotes.");
      }
      if (quote.length() > 1) {
        throw new IllegalArgumentException(
            "Quotes consisting of multiple characters or code units are not supported.");
      }

      quoteCharacter = quote.charAt(0);
      if (quoteCharacter == noQuoteCharacter) {
        throw new IllegalArgumentException("Illegal quote character.");
      }
    } else {
      quoteCharacter = noQuoteCharacter;
    }

    this.headers = headers;

    if (headers == Headers.INFER) {
      throw new IllegalStateException("Inferring headers is not yet implemented");
    }
  }

  public Table read(InputStream inputStream) {
    throw new RuntimeException("TODO");
  }
}
