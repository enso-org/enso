package org.enso.table.data.column.builder;

import org.enso.table.data.column.Storage;

/** A builder used by the parser to add items into a column. */
public abstract class StorageBuilder {
  /**
   * Called by the parser to notify the builder about the next value being appended. The value is
   * passed in a String form and the builder is responsible for parsing it into its own format. The
   * value may be null, in which case it should be considered missing.
   *
   * @param value the value to parse and append
   * @return a storage builder instance to use for future calls
   */
  public abstract StorageBuilder parseAndAppend(String value);

  /**
   * Closes the storage builder and returns a fully parsed column.
   *
   * @return the storage resulting from this builder's operation.
   */
  public abstract Storage seal();
}
