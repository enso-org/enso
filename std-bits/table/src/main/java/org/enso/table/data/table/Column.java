package org.enso.table.data.table;

import java.util.BitSet;
import java.util.List;
import org.enso.base.polyglot.Polyglot_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StorageListView;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.mask.OrderMask;
import org.enso.table.data.mask.SliceRange;
import org.enso.table.error.InvalidColumnNameException;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

/** A representation of a column. Consists of a column name and the underlying storage. */
public final class Column {
  private final String name;
  private final Storage<?> storage;

  /**
   * Creates a new column.
   *
   * @param name the column name
   * @param storage the underlying storage
   */
  public Column(String name, ColumnStorage<?> storage) {
    ensureNameIsValid(name);
    this.name = name;
    this.storage = (Storage<?>) storage;
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
            default -> (name.indexOf('\0') >= 0)
                ? "Column name cannot contain the NUL character."
                : null;
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
  public Storage<?> getStorage() {
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
   * Return a new column, containing only the items marked true in the mask.
   *
   * @param filterMask the mask to use
   * @param newLength the number of true values in mask
   * @return a new column, masked with the given mask
   */
  Column applyFilter(BitSet filterMask, int newLength) {
    return new Column(name, storage.applyFilter(filterMask, newLength));
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
   * @param mask the reordering to apply
   * @return a new column, resulting from reordering this column according to {@code mask}.
   */
  public Column applyMask(OrderMask mask) {
    var newStorage = storage.applyMask(mask);
    return new Column(name, newStorage);
  }

  /**
   * @return a list view of the column
   */
  public List<?> asList() {
    return new StorageListView(this.getStorage());
  }

  /**
   * @return a copy of the Column containing a slice of the original data
   */
  public Column slice(int offset, int limit) {
    return new Column(name, storage.slice(offset, limit));
  }

  /**
   * @return a copy of the Column consisting of slices of the original data
   */
  public Column slice(List<SliceRange> ranges) {
    return new Column(name, storage.slice(ranges));
  }
}
