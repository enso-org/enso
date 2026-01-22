package org.enso.table.data.table;

import java.lang.reflect.Proxy;
import java.util.List;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.masks.IndexMapper;
import org.enso.table.data.column.operation.masks.MaskOperation;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.StorageListView;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.SliceRange;
import org.enso.table.error.InvalidColumnNameException;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** A representation of a column. Consists of a column name and the underlying storage. */
public final class Column {
  private static final Logger LOGGER = LoggerFactory.getLogger(Column.class);
  private final String name;
  private final ColumnStorage<?> storage;

  /**
   * Creates a new column.
   *
   * @param name the column name
   * @param storage the underlying storage
   */
  public Column(String name, ColumnStorage<?> storage) {
    ensureNameIsValid(name);
    this.name = name;
    var isProxy = Proxy.isProxyClass(storage.getClass());
    this.storage = isProxy ? Builder.makeLocal(storage) : storage;
    var type = this.storage.getType();
    LOGGER.trace(
        "Column[{}] of {}:{} type with size: {}",
        name,
        type.typeChar(),
        type.size(),
        storage.getSize());
  }

  public static boolean isColumnNameValid(String name) {
    boolean invalid = (name == null) || name.isEmpty() || (name.indexOf('\0') >= 0);
    return !invalid;
  }

  public static void ensureNameIsValid(String name) {
    if (!isColumnNameValid(name)) {
      String extraMessage =
          switch (name) {
            case null -> "Column name cannot be Nothing.";
            case "" -> "Column name cannot be empty.";
            default ->
                (name.indexOf('\0') >= 0) ? "Column name cannot contain the NUL character." : null;
          };
      throw new InvalidColumnNameException(name, extraMessage);
    }
  }

  /**
   * @return the column name
   */
  public String getName() {
    return name;
  }

  /**
   * @return the underlying storage
   */
  public ColumnStorage<?> getStorage() {
    return storage;
  }

  /* Gets the value at a given index. */
  public Object getItem(long index) {
    return storage.getItemBoxed(index);
  }

  /**
   * @return the type of the underlying storage
   */
  public StorageType<?> getType() {
    return storage.getType();
  }

  /**
   * @return the number of items in this column.
   */
  public int getSize() {
    // ToDo: Work through changing to long.
    return Math.toIntExact(getStorage().getSize());
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
  public static Column fromItems(
      String name,
      List<Value> items,
      StorageType<?> expectedType,
      ProblemAggregator problemAggregator)
      throws ClassCastException {
    Context context = Context.getCurrent();
    int n = items.size();
    var builder = Builder.getForType(expectedType, n, problemAggregator);

    // ToDo: This a workaround for an issue with polyglot layer. #5590 is related.
    for (Object item : items) {
      if (item instanceof Value v) {
        Object converted = Polyglot_Utils.convertPolyglotValue(v);
        builder.append(converted);
      } else {
        builder.append(item);
      }

      context.safepoint();
    }

    return new Column(name, builder.seal());
  }

  /**
   * Creates a column from an Enso array. No polyglot conversion happens.
   *
   * <p>If a date value is passed to this function, it may not be recognized as such due to the lack
   * of conversion. So this is only safe if we guarantee that the method will not get a Date value,
   * or will reject it right after processing it.
   */
  public static Column fromItemsNoDateConversion(
      String name,
      List<Object> items,
      StorageType<?> expectedType,
      ProblemAggregator problemAggregator)
      throws ClassCastException {
    Context context = Context.getCurrent();
    int n = items.size();
    var builder = Builder.getForType(expectedType, n, problemAggregator);

    for (Object item : items) {
      builder.append(item);
      context.safepoint();
    }

    return new Column(name, builder.seal());
  }

  /**
   * Creates a new column with given name and an element to repeat.
   *
   * @param name the name to use
   * @param item the item repeated in the column
   * @return a column with given name and items
   */
  public static Column fromRepeatedItem(String name, Value item, int repeat) {
    Object converted = Polyglot_Utils.convertPolyglotValue(item);
    return new Column(name, Builder.fromRepeatedItem(converted, repeat));
  }

  /**
   * Create a new column with a slice of the original data.
   *
   * @return a sliced column.
   */
  public Column slice(long offset, long limit) {
    return offset >= getSize()
        ? MaskOperation.slice(this, 0, 0)
        : MaskOperation.slice(this, offset, limit);
  }

  /**
   * Creates a new column with a set of slices of the original data.
   *
   * @return a sliced column.
   */
  public Column slice(List<SliceRange> ranges) {
    if (ranges.isEmpty()) {
      // Creates an empty table
      return slice(0, 0);
    }

    if (ranges.size() == 1) {
      // If there is only one range, we can use the existing slice method
      SliceRange range = ranges.get(0);
      return slice(range.start(), range.end() - range.start());
    }

    // If there are multiple ranges, we need to create a mask
    long[] mask = SliceRange.createMask(ranges);
    return mask(mask);
  }

  public Column mask(long[] mask) {
    return MaskOperation.mask(this, mask);
  }

  public Column reverse() {
    return mask(new IndexMapper.Reversed(0, getSize()));
  }

  /**
   * Creates a column with the same name and storage, but with the order of items changed according
   * to the given index mapper. This is an internal method used by the table for efficiency.
   *
   * @param indexMapper the index mapper to use for reordering
   * @return a new column with reordered items
   */
  Column mask(IndexMapper indexMapper) {
    var storage = getStorage();
    var newStorage = MaskOperation.getSlicedStorage(storage, indexMapper);
    return new Column(getName(), newStorage);
  }

  /**
   * @return a list view of the column
   */
  public List<?> asList() {
    return new StorageListView(this.getStorage());
  }
}
