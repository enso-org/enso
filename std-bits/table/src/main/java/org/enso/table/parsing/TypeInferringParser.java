package org.enso.table.parsing;

import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.read.WithProblems;

/**
 * The type inferring parser tries to parse the given column using a set of provided parsers. It
 * returns the result of the first parser that succeeds without reporting any problems.
 *
 * <p>At least one parser must always be provided. The last parser on the list is used as a fallback
 * - its result is returned regardless of whether it contained problems or not.
 */
public class TypeInferringParser implements DatatypeParser {

  private final DatatypeParser[] baseParsers;

  public TypeInferringParser(DatatypeParser[] baseParsers) {
    if (baseParsers.length == 0) {
      throw new IllegalArgumentException("At least one parser must be provided.");
    }
    this.baseParsers = baseParsers;
  }

  @Override
  public WithProblems<Storage> parseColumn(StringStorage sourceStorage) {
    for (int i = 0; i < baseParsers.length - 1; ++i) {
      WithProblems<Storage> res = baseParsers[i].parseColumn(sourceStorage);
      if (res.problems().isEmpty()) return res;
    }

    return baseParsers[baseParsers.length - 1].parseColumn(sourceStorage);
  }
}
