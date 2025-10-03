package org.enso.table.parsing;

import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.table.Column;
import org.enso.table.parsing.problems.CommonParseProblemAggregator;
import org.enso.table.parsing.problems.ParseProblemAggregator;

/** A base type for a parser capable of parsing a column of text values into some other type. */
public abstract class DatatypeParser {
  /**
   * Parses a single cell.
   *
   * @param text the text contents to parse, it will never be null in the default implementation -
   *     null values are just passed as-is without any parsing attempts by default
   * @param problemAggregator an instance of the problem aggregator, used for reporting parsing
   *     problems
   * @return the parsed value or null if the value could not be parsed or could be parsed but should
   *     be treated as missing value
   */
  public abstract Object parseSingleValue(String text, ParseProblemAggregator problemAggregator);

  /**
   * Parses a column of texts (represented as a {@code ColumnStorage<String>}) and returns a new
   * storage, containing the parsed elements.
   */
  public final ColumnStorage<?> parseColumn(
      Column sourceColumn, CommonParseProblemAggregator problemAggregator) {
    var storage = sourceColumn.getStorage();
    if (!(storage.getType() instanceof TextType textType)) {
      throw new IllegalArgumentException(
          "Expected a column of text type, got: " + storage.getType());
    }
    return parseColumn(textType.asTypedStorage(storage), problemAggregator);
  }

  /**
   * Parses a column of texts (represented as a {@code ColumnStorage<String>}) and returns a new
   * storage, containing the parsed elements.
   */
  public abstract ColumnStorage<?> parseColumn(
      ColumnStorage<String> sourceStorage, CommonParseProblemAggregator problemAggregator);
}
