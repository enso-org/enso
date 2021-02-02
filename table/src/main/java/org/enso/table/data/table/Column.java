package org.enso.table.data.table;

import java.util.BitSet;
import java.util.List;
import org.enso.table.data.column.builder.object.InferredBuilder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.DefaultIndex;
import org.enso.table.data.index.Index;
import org.enso.table.error.UnexpectedColumnTypeException;

/** A representation of a column. Consists of a column name and the underlying storage. */
public class Column {
  private final String name;
  private final Storage storage;
  private final Index index;

  /**
   * Creates a new column.
   *
   * @param name the column name
   * @param storage the underlying storage
   */
  public Column(String name, Index index, Storage storage) {
    this.name = name;
    this.storage = storage;
    this.index = index;
  }

  /**
   * Converts this column to a single-column table.
   *
   * @return a table containing only this column
   */
  public Table toTable() {
    return new Table(new Column[] {this}, index);
  }

  /** @return the column name */
  public String getName() {
    return name;
  }

  /** @return the underlying storage */
  public Storage getStorage() {
    return storage;
  }

  /** @return the number of items in this column. */
  public long getSize() {
    return getStorage().size();
  }

  /**
   * Return a new column, containing only the items marked true in the mask.
   *
   * @param mask the mask to use
   * @param cardinality the number of true values in mask
   * @return a new column, masked with the given mask
   */
  public Column mask(Index maskedIndex, BitSet mask, int cardinality) {
    return new Column(name, maskedIndex, storage.mask(mask, cardinality));
  }

  /**
   * Returns a column resulting from selecting only the rows corresponding to true entries in the
   * provided column.
   *
   * @param maskCol the masking column
   * @return the result of masking this column with the provided column
   */
  public Column mask(Column maskCol) {
    if (!(maskCol.getStorage() instanceof BoolStorage)) {
      throw new UnexpectedColumnTypeException("Boolean");
    }

    BoolStorage storage = (BoolStorage) maskCol.getStorage();
    var mask = BoolStorage.toMask(storage);
    var localStorageMask = new BitSet();
    localStorageMask.set(0, getStorage().size());
    mask.and(localStorageMask);
    int cardinality = mask.cardinality();
    Index newIx = index.mask(mask, cardinality);
    return mask(newIx, mask, cardinality);
  }

  /**
   * Renames the column.
   *
   * @param name the new name
   * @return a new column with the given name
   */
  public Column rename(String name) {
    return new Column(name, index, storage);
  }

  /**
   * Creates a new column with given name and elements.
   *
   * @param name the name to use
   * @param items the items contained in the column
   * @return a column with given name and items
   */
  public static Column fromItems(String name, List<Object> items) {
    InferredBuilder builder = new InferredBuilder(items.size());
    for (Object item : items) {
      builder.append(item);
    }
    return new Column(name, new DefaultIndex(items.size()), builder.seal());
  }

  /**
   * Changes the index of this column.
   *
   * @param ix the index to use
   * @return a column indexed by {@code ix}
   */
  public Column withIndex(Index ix) {
    return new Column(name, ix, storage);
  }

  /** @return the index of this column */
  public Index getIndex() {
    return index;
  }
}
