package org.enso.table.data.table;

import org.enso.table.data.column.builder.object.InferredBuilder;
import org.enso.table.data.column.operation.aggregate.Aggregator;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.index.DefaultIndex;
import org.enso.table.data.index.HashIndex;
import org.enso.table.data.index.Index;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.enso.table.error.UnexpectedColumnTypeException;
import org.graalvm.polyglot.Value;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.BitSet;
import java.util.List;
import java.util.function.Function;
import java.util.stream.IntStream;

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
   * Creates a new column.
   *
   * @param name the column name
   * @param storage the underlying storage
   */
  public Column(String name, Storage storage) {
    this(name, new DefaultIndex(storage.size()), storage);
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
  public int getSize() {
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
    if (!(maskCol.getStorage() instanceof BoolStorage storage)) {
      throw new UnexpectedColumnTypeException("Boolean");
    }

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
  public static Column fromItems(String name, List<Value> items) {
    InferredBuilder builder = new InferredBuilder(items.size());
    for (Value item : items) {
      Object converted = convertDateOrTime(item);
      builder.appendNoGrow(converted);
    }
    var storage = builder.seal();
    return new Column(name, new DefaultIndex(items.size()), storage);
  }

  private static Object convertDateOrTime(Value item) {
    if (item.isDate()) {
      LocalDate d = item.asDate();
      if (item.isTime()) {
        LocalDateTime dtime = d.atTime(item.asTime());
        if (item.isTimeZone()) {
          return dtime.atZone(item.asTimeZone());
        } else {
          return dtime;
        }
      } else {
        return d;
      }
    } else if (item.isTime()) {
      return item.asTime();
    }
    return item.as(Object.class);
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

  /**
   * Sets the index of this column to the provided column.
   *
   * @param col the column to use as the index.
   * @return a column indexed by {@code col}
   */
  public Column setIndex(Column col) {
    Storage storage = col.getStorage();
    Index ix = HashIndex.fromStorage(col.getName(), storage);
    return this.withIndex(ix);
  }

  /** @return the index of this column */
  public Index getIndex() {
    return index;
  }

  /**
   * Aggregates the values in this column, using a given aggregation operation.
   *
   * @param aggName name of a vectorized operation that can be used if possible. If null is passed,
   *     this parameter is unused.
   * @param aggregatorFunction the function to use if a vectorized operation is not available.
   * @param skipNa whether missing values should be passed to the {@code fallback} function.
   * @return a column indexed by the unique index of this aggregate, storing results of applying the
   *     specified operation.
   */
  public Object aggregate(
      String aggName, Function<List<Object>, Object> aggregatorFunction, boolean skipNa) {
    Aggregator aggregator = storage.getAggregator(aggName, aggregatorFunction, skipNa, 1);

    IntStream ixes = IntStream.range(0, storage.size());
    aggregator.nextGroup(ixes);
    return aggregator.seal().getItemBoxed(0);
  }

  /**
   * @param mask the reordering to apply
   * @return a new column, resulting from reordering this column according to {@code mask}.
   */
  public Column applyMask(OrderMask mask) {
    Index newIndex = index.applyMask(mask);
    Storage newStorage = storage.applyMask(mask);
    return new Column(name, newIndex, newStorage);
  }

  /** @return a copy of the Column containing a slice of the original data */
  public Column slice(int offset, int limit) {
    return new Column(name, index.slice(offset, limit), storage.slice(offset, limit));
  }

  /** @return a copy of the Column consisting of slices of the original data */
  public Column slice(List<SliceRange> ranges) {
    return new Column(name, index.slice(ranges), storage.slice(ranges));
  }

  /** @return a column counting value repetitions in this column. */
  public Column duplicateCount() {
    return new Column(name + "_duplicate_count", index, storage.duplicateCount());
  }
}
