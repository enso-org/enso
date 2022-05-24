package org.enso.table.parsing;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.read.WithProblems;

/** A base type for a parser capable of parsing a column of text values into some other type. */
public interface DatatypeParser {
  /**
   * Parses a column of texts (represented as a {@code StringStorage}) and returns a new storage,
   * containing the parsed elements.
   */
  WithProblems<Storage> parseColumn(String columnName, StringStorage sourceStorage);
}
