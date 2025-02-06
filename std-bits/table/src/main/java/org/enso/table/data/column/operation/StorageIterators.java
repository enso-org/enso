package org.enso.table.data.column.operation;

import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;

/** Set of typed storage iterators for operations. * */
public class StorageIterators {
  @FunctionalInterface
  public interface MapOperation<T, S> {
    T apply(long index, S value);
  }

  @FunctionalInterface
  public interface LongMapOperation<T> {
    T apply(long index, long value, boolean isNothing);
  }

  @FunctionalInterface
  public interface DoubleMapOperation<T> {
    T apply(long index, double value, boolean isNothing);
  }

  @FunctionalInterface
  public interface BooleanMapOperation<T> {
    T apply(long index, boolean value, boolean isNothing);
  }

  /**
   * Iterates over every value of a source Storage, calling an operation for each step. The result
   * of the operation is appended to the builder. Nothing values are appended automatically as
   * Nothing (use the override to control this).
   *
   * @param source the source storage to read from and iterate over.
   * @param builder the output builder.
   * @param operation a callback to process a single value.
   * @return a built ColumnStorage from sealing the builder.
   * @param <S> Input Java type.
   * @param <T> Output Java type.
   */
  public static <S, T> ColumnStorage<T> mapOverStorage(
      ColumnStorage<S> source, BuilderForType<T> builder, MapOperation<T, S> operation) {
    return mapOverStorage(source, true, builder, operation);
  }

  /**
   * Iterates over every value of a source Storage, calling an operation for each step. The result
   * of the operation is appended to the builder.
   *
   * @param source the source storage to read from and iterate over.
   * @param preserveNothing if True then Nothing is appended straight to builder otherwise passed to
   *     the operation.
   * @param builder the output builder.
   * @param operation a callback to process a single value.
   * @return a built ColumnStorage from sealing the builder.
   * @param <S> Input Java type.
   * @param <T> Output Java type.
   */
  public static <S, T> ColumnStorage<T> mapOverStorage(
      ColumnStorage<S> source,
      boolean preserveNothing,
      BuilderForType<T> builder,
      MapOperation<T, S> operation) {
    long size = source.getSize();
    for (long index = 0; index < size; index++) {
      if (preserveNothing && source.isNothing(index)) {
        builder.appendNulls(1);
      } else {
        var result = operation.apply(index, source.getItemBoxed(index));
        builder.append(result);
      }
    }
    return builder.seal();
  }

  /**
   * Iterates over every value of a source long Storage, calling an operation for each step. The
   * result of the operation is appended to the builder. Nothing values are appended automatically
   * as Nothing (use the override to control this).
   *
   * @param source the source storage to read from and iterate over.
   * @param builder the output builder.
   * @param operation a callback to process a single value.
   * @return a built ColumnStorage from sealing the builder.
   * @param <T> Output Java type.
   */
  public static <T> ColumnStorage<T> mapOverLongStorage(
      ColumnLongStorage source, BuilderForType<T> builder, LongMapOperation<T> operation) {
    return mapOverLongStorage(source, true, builder, operation);
  }

  /**
   * Iterates over every value of a source long Storage, calling an operation for each step. The
   * result of the operation is appended to the builder.
   *
   * @param source the source storage to read from and iterate over.
   * @param preserveNothing if True then Nothing is appended straight to builder otherwise passed to
   *     the operation.
   * @param builder the output builder.
   * @param operation a callback to process a single value.
   * @return a built ColumnStorage from sealing the builder.
   * @param <T> Output Java type.
   */
  public static <T> ColumnStorage<T> mapOverLongStorage(
      ColumnLongStorage source,
      boolean preserveNothing,
      BuilderForType<T> builder,
      LongMapOperation<T> operation) {
    long size = source.getSize();
    for (long index = 0; index < size; index++) {
      if (preserveNothing && source.isNothing(index)) {
        builder.appendNulls(1);
      } else {
        var result = operation.apply(index, source.getItemAsLong(index), source.isNothing(index));
        builder.append(result);
      }
    }
    return builder.seal();
  }

  /**
   * Iterates over every value of a source double Storage, calling an operation for each step. The
   * result of the operation is appended to the builder. Nothing values are appended automatically
   * as Nothing (use the override to control this).
   *
   * @param source the source storage to read from and iterate over.
   * @param builder the output builder.
   * @param operation a callback to process a single value.
   * @return a built ColumnStorage from sealing the builder.
   * @param <T> Output Java type.
   */
  public static <T> ColumnStorage<T> mapOverDoubleStorage(
      ColumnDoubleStorage source, BuilderForType<T> builder, DoubleMapOperation<T> operation) {
    return mapOverDoubleStorage(source, true, builder, operation);
  }

  /**
   * Iterates over every value of a source double Storage, calling an operation for each step. The
   * result of the operation is appended to the builder.
   *
   * @param source the source storage to read from and iterate over.
   * @param preserveNothing if True then Nothing is appended straight to builder otherwise passed to
   *     the operation.
   * @param builder the output builder.
   * @param operation a callback to process a single value.
   * @return a built ColumnStorage from sealing the builder.
   * @param <T> Output Java type.
   */
  public static <T> ColumnStorage<T> mapOverDoubleStorage(
      ColumnDoubleStorage source,
      boolean preserveNothing,
      BuilderForType<T> builder,
      DoubleMapOperation<T> operation) {
    long size = source.getSize();
    for (long index = 0; index < size; index++) {
      if (preserveNothing && source.isNothing(index)) {
        builder.appendNulls(1);
      } else {
        var result = operation.apply(index, source.getItemAsDouble(index), source.isNothing(index));
        builder.append(result);
      }
    }
    return builder.seal();
  }

  /**
   * Iterates over every value of a source boolean Storage, calling an operation for each step. The
   * result of the operation is appended to the builder. Nothing values are appended automatically
   * as Nothing (use the override to control this).
   *
   * @param source the source storage to read from and iterate over.
   * @param builder the output builder.
   * @param operation a callback to process a single value.
   * @return a built ColumnStorage from sealing the builder.
   * @param <T> Output Java type.
   */
  public static <T> ColumnStorage<T> mapOverBooleanStorage(
      ColumnBooleanStorage source, BuilderForType<T> builder, BooleanMapOperation<T> operation) {
    return mapOverBooleanStorage(source, true, builder, operation);
  }

  /**
   * Iterates over every value of a source boolean Storage, calling an operation for each step. The
   * result of the operation is appended to the builder.
   *
   * @param source the source storage to read from and iterate over.
   * @param preserveNothing if True then Nothing is appended straight to builder otherwise passed to
   *     the operation.
   * @param builder the output builder.
   * @param operation a callback to process a single value.
   * @return a built ColumnStorage from sealing the builder.
   * @param <T> Output Java type.
   */
  public static <T> ColumnStorage<T> mapOverBooleanStorage(
      ColumnBooleanStorage source,
      boolean preserveNothing,
      BuilderForType<T> builder,
      BooleanMapOperation<T> operation) {
    long size = source.getSize();
    for (long index = 0; index < size; index++) {
      if (preserveNothing && source.isNothing(index)) {
        builder.appendNulls(1);
      } else {
        var result =
            operation.apply(index, source.getItemAsBoolean(index), source.isNothing(index));
        builder.append(result);
      }
    }
    return builder.seal();
  }
}
