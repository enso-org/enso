package org.enso.table.data.table;

import org.enso.base.Text_Utils;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.InferredBuilder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.index.DefaultIndex;
import org.enso.table.data.index.Index;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.enso.table.error.InvalidColumnNameException;
import org.enso.table.error.UnexpectedColumnTypeException;
import org.enso.table.problems.WithAggregatedProblems;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.List;

/** A representation of a column. Consists of a column name and the underlying storage. */
public class Column {
  private final String name;
  private final Storage<?> storage;

  /**
   * Creates a new column.
   *
   * @param name the column name
   * @param storage the underlying storage
   */
  public Column(String name, Storage<?> storage) {
    ensureNameIsValid(name);
    this.name = name;
    this.storage = storage;
  }

  public static boolean isColumnNameValid(String name) {
    boolean invalid = (name == null) || name.isEmpty() || (name.indexOf('\0') >= 0);
    return !invalid;
  }

  public static void ensureNameIsValid(String name) {
    if (!isColumnNameValid(name)) {
      String extraMessage = switch (name) {
        case null -> "Column name cannot be Nothing.";
        case "" -> "Column name cannot be empty.";
        default ->
            (name.indexOf('\0') >= 0) ? "Column name cannot contain the NUL character." : null;
      };
      throw new InvalidColumnNameException(name, extraMessage);
    }
  }

  /**
   * Converts this column to a single-column table.
   *
   * @return a table containing only this column
   */
  public Table toTable() {
    return new Table(new Column[] {this});
  }

  /** @return the column name */
  public String getName() {
    return name;
  }

  /** @return the underlying storage */
  public Storage<?> getStorage() {
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
  public Column mask(BitSet mask, int cardinality) {
    return new Column(name, storage.mask(mask, cardinality));
  }

  /**
   * Returns a column resulting from selecting only the rows corresponding to true entries in the
   * provided column.
   *
   * @param maskCol the masking column
   * @return the result of masking this column with the provided column
   */
  public Column mask(Column maskCol) {
    if (!(maskCol.getStorage() instanceof BoolStorage boolStorage)) {
      throw new UnexpectedColumnTypeException("Boolean");
    }

    var mask = BoolStorage.toMask(boolStorage);
    var localStorageMask = new BitSet();
    localStorageMask.set(0, getStorage().size());
    mask.and(localStorageMask);
    int cardinality = mask.cardinality();
    return mask(mask, cardinality);
  }

  /**
   * Renames the column.
   *
   * @param name the new name
   * @return a new column with the given name
   */
  public Column rename(String name) {
    return new Column(name, storage);
  }

  /** Creates a column from an Enso array, ensuring Enso dates are converted to Java dates. */
  public static WithAggregatedProblems<Column> fromItems(String name, List<Value> items, StorageType expectedType) throws ClassCastException {
    Context context = Context.getCurrent();
    int n = items.size();
    Builder builder = expectedType == null ? new InferredBuilder(n) : Builder.getForType(expectedType, n);

    // ToDo: This a workaround for an issue with polyglot layer. #5590 is related.
    for (Object item : items) {
      if (item instanceof Value v) {
        Object converted = Polyglot_Utils.convertPolyglotValue(v);
        builder.appendNoGrow(converted);
      } else {
        builder.appendNoGrow(item);
      }

      context.safepoint();
    }

    var result = new Column(name, builder.seal());
    return new WithAggregatedProblems<>(result, builder.getProblems());
  }

  /**
   * Creates a column from an Enso array. No polyglot conversion happens.
   * <p>
   * If a date value is passed to this function, it may not be recognized as such due to the lack of conversion. So this
   * is only safe if we guarantee that the method will not get a Date value, or will reject it right after processing
   * it.
   */
  public static Column fromItemsNoDateConversion(String name, List<Object> items, StorageType expectedType) throws ClassCastException {
    Context context = Context.getCurrent();
    int n = items.size();
    Builder builder = expectedType == null ? new InferredBuilder(n) : Builder.getForType(expectedType, n);

    for (Object item : items) {
      builder.appendNoGrow(item);
      context.safepoint();
    }
    var storage = builder.seal();
    return new Column(name, storage);
  }

  /**
   * Creates a new column with given name and elements.
   *
   * @param name the name to use
   * @param items the items contained in the column
   * @return a column with given name and items
   */
  public static WithAggregatedProblems<Column> fromRepeatedItems(String name, List<Value> items, int repeat) {
    if (repeat < 1) {
      throw new IllegalArgumentException("Repeat count must be positive.");
    }

    if (repeat == 1) {
      return fromItems(name, items, null);
    }

    Context context = Context.getCurrent();
    var totalSize = items.size() * repeat;

    var values = new ArrayList<Object>(items.size());
    // ToDo: This a workaround for an issue with polyglot layer. #5590 is related.
    // to revert replace with: for (Value item : items) {
    for (Object item : items) {
      Object converted = item instanceof Value v ? Polyglot_Utils.convertPolyglotValue(v) : item;
      values.add(converted);
      context.safepoint();
    }

    var builder = new InferredBuilder(totalSize);
    for (int i = 0; i < totalSize; i++) {
      var item = values.get(i % items.size());
      builder.appendNoGrow(item);
      context.safepoint();
    }

    var result = new Column(name, builder.seal());
    return new WithAggregatedProblems<>(result, builder.getProblems());
  }

  /** @return the index of this column */
  public Index getIndex() {
    return new DefaultIndex(getSize());
  }

  /**
   * @param mask the reordering to apply
   * @return a new column, resulting from reordering this column according to {@code mask}.
   */
  public Column applyMask(OrderMask mask) {
    Storage<?> newStorage = storage.applyMask(mask);
    return new Column(name, newStorage);
  }

  /** @return a copy of the Column containing a slice of the original data */
  public Column slice(int offset, int limit) {
    return new Column(name, storage.slice(offset, limit));
  }

  /** @return a copy of the Column consisting of slices of the original data */
  public Column slice(List<SliceRange> ranges) {
    return new Column(name, storage.slice(ranges));
  }

  /** @return a column counting value repetitions in this column. */
  public Column duplicateCount() {
    return new Column(name + "_duplicate_count", storage.duplicateCount());
  }

  /** Resizes the given column to the provided new length.
   * <p>
   * If the new length is smaller than the current length, the column is truncated.
   * If the new length is larger than the current length, the column is padded with nulls.
   */
  public Column resize(int newSize) {
    if (newSize == getSize()) {
      return this;
    } else if (newSize < getSize()) {
      return slice(0, newSize);
    } else {
      int nullsToAdd = newSize - getSize();
      return new Column(name, storage.appendNulls(nullsToAdd));
    }
  }
}
