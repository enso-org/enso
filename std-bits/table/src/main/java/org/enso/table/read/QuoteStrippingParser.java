package org.enso.table.read;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import org.enso.table.data.column.builder.object.StringBuilder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.parsing.DatatypeParser;
import org.enso.table.parsing.problems.MismatchedQuote;
import org.enso.table.parsing.problems.ParsingProblem;

public class QuoteStrippingParser implements DatatypeParser {

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
  public WithProblems<Storage> parseColumn(String columnName, StringStorage sourceStorage) {
    AtomicInteger mismatchedQuotes = new AtomicInteger();
    QuoteHelper quoteHelper = new QuoteHelper(unused -> mismatchedQuotes.getAndIncrement(), quoteCharacter);

    StringBuilder builder = new StringBuilder(sourceStorage.size());
    for (int i = 0; i < sourceStorage.size(); ++i) {
      String item = sourceStorage.getItem(i);
      String transformed = quoteHelper.stripQuotes(item);
      builder.appendNoGrow(transformed);
    }

    List<ParsingProblem> problems;
    if (mismatchedQuotes.get() == 0) {
      problems = List.of();
    } else {
      problems = new ArrayList<>(mismatchedQuotes.get());
      for (int i = 0; i < mismatchedQuotes.getAndIncrement(); ++i) {
        problems.add(new MismatchedQuote());
      }
    }

    return new WithProblems<>(builder.seal(), problems);
  }
}
