package org.enso.table.data.column.operation;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.graalvm.polyglot.Context;

/** Set of typed storage iterators for operations. * */
public class StorageIterators {
  @FunctionalInterface
  public interface BuildObjectOperation<S> {
    void apply(Builder builder, long index, S value);
  }

  @FunctionalInterface
  public interface DoubleBuildObjectOperation {
    void apply(Builder builder, long index, double value, boolean isNothing);
  }

  /** Generally best to use a typed builder, but if not possible fall back to this. */
  public static <S> ColumnStorage<?> buildObjectOverStorage(
      ColumnStorage<S> source,
      boolean preserveNothing,
      Builder builder,
      BuildObjectOperation<S> operation) {
    long size = source.getSize();
    Context context = Context.getCurrent();
    for (long index = 0; index < size; index++) {
      if (preserveNothing && source.isNothing(index)) {
        builder.appendNulls(1);
      } else {
        operation.apply(builder, index, source.getItemBoxed(index));
      }
      context.safepoint();
    }
    return builder.seal();
  }

  /** Generally best to use a typed builder, but if not possible fall back to this. */
  public static ColumnStorage<?> buildObjectOverDoubleStorage(
      ColumnDoubleStorage source,
      boolean preserveNothing,
      Builder builder,
      DoubleBuildObjectOperation operation) {
    long size = source.getSize();
    Context context = Context.getCurrent();
    for (long index = 0; index < size; index++) {
      if (preserveNothing && source.isNothing(index)) {
        builder.appendNulls(1);
      } else {
        operation.apply(builder, index, source.getItemAsDouble(index), source.isNothing(index));
      }
      context.safepoint();
    }
    return builder.seal();
  }

  @FunctionalInterface
  public interface BuildOperation<B extends BuilderForType<?>, S> {
    void apply(B builder, long index, S value);
  }

  @FunctionalInterface
  public interface LongBuildOperation<B extends BuilderForType<?>> {
    void apply(B builder, long index, long value, boolean isNothing);
  }

  @FunctionalInterface
  public interface DoubleBuildOperation<B extends BuilderForType<?>> {
    void apply(B builder, long index, double value, boolean isNothing);
  }

  @FunctionalInterface
  public interface BooleanBuildOperation<B extends BuilderForType<?>> {
    void apply(B builder, long index, boolean value, boolean isNothing);
  }

  /**
   * Iterates over every value of a source Storage, calling an operation for each step. The
   * operation is expected to append the result to the builder. Nothing values are appended
   * automatically as Nothing (use the override to control this). Use this when wanting to avoid
   * boxing for Long, Boolean or Double builders.
   *
   * @param source the source storage to read from and iterate over.
   * @param builder the output builder.
   * @param operation a callback to process a single value.
   * @return a built ColumnStorage from sealing the builder.
   * @param <B> Builder type.
   * @param <S> Input Java type.
   * @param <T> Output Java type.
   */
  public static <B extends BuilderForType<T>, S, T> ColumnStorage<T> buildOverStorage(
      ColumnStorage<S> source, B builder, BuildOperation<B, S> operation) {
    return buildOverStorage(source, true, builder, operation);
  }

  /**
   * Iterates over every value of a source Storage, calling an operation for each step. The
   * operation is expected to append the result to the builder. Use this when wanting to avoid
   * boxing for Long, Boolean or Double builders.
   *
   * @param source the source storage to read from and iterate over.
   * @param preserveNothing if True then Nothing is appended straight to builder otherwise passed to
   *     the operation.
   * @param builder the output builder.
   * @param operation a callback to process a single value.
   * @return a built ColumnStorage from sealing the builder.
   * @param <B> Builder type.
   * @param <S> Input Java type.
   * @param <T> Output Java type.
   */
  public static <B extends BuilderForType<T>, S, T> ColumnStorage<T> buildOverStorage(
      ColumnStorage<S> source, boolean preserveNothing, B builder, BuildOperation<B, S> operation) {
    long size = source.getSize();
    Context context = Context.getCurrent();
    for (long index = 0; index < size; index++) {
      if (preserveNothing && source.isNothing(index)) {
        builder.appendNulls(1);
      } else {
        operation.apply(builder, index, source.getItemBoxed(index));
      }
      context.safepoint();
    }
    return builder.seal();
  }

  /**
   * Iterates over every value of a source long Storage, calling an operation for each step. The
   * operation is expected to append the result to the builder. Nothing values are appended
   * automatically as Nothing (use the override to control this). Use this when wanting to avoid
   * boxing for Long, Boolean or Double builders.
   *
   * @param source the source storage to read from and iterate over.
   * @param builder the output builder.
   * @param operation a callback to process a single value.
   * @return a built ColumnStorage from sealing the builder.
   * @param <B> Builder type.
   * @param <T> Output Java type.
   */
  public static <B extends BuilderForType<T>, T> ColumnStorage<T> buildOverLongStorage(
      ColumnLongStorage source, B builder, LongBuildOperation<B> operation) {
    return buildOverLongStorage(source, true, builder, operation);
  }

  /**
   * Iterates over every value of a source long Storage, calling an operation for each step. The
   * operation is expected to append the result to the builder. Use this when wanting to avoid
   * boxing for Long, Boolean or Double builders.
   *
   * @param source the source storage to read from and iterate over.
   * @param preserveNothing if True then Nothing is appended straight to builder otherwise passed to
   *     the operation.
   * @param builder the output builder.
   * @param operation a callback to process a single value.
   * @return a built ColumnStorage from sealing the builder.
   * @param <B> Builder type.
   * @param <T> Output Java type.
   */
  public static <B extends BuilderForType<T>, T> ColumnStorage<T> buildOverLongStorage(
      ColumnLongStorage source,
      boolean preserveNothing,
      B builder,
      LongBuildOperation<B> operation) {
    long size = source.getSize();
    Context context = Context.getCurrent();
    for (long index = 0; index < size; index++) {
      if (preserveNothing && source.isNothing(index)) {
        builder.appendNulls(1);
      } else {
        operation.apply(builder, index, source.getItemAsLong(index), source.isNothing(index));
      }
      context.safepoint();
    }
    return builder.seal();
  }

  /**
   * Iterates over every value of a source double Storage, calling an operation for each step. The
   * operation is expected to append the result to the builder. Nothing values are appended
   * automatically as Nothing (use the override to control this). Use this when wanting to avoid
   * boxing for Long, Boolean or Double builders.
   *
   * @param source the source storage to read from and iterate over.
   * @param builder the output builder.
   * @param operation a callback to process a single value.
   * @return a built ColumnStorage from sealing the builder.
   * @param <B> Builder type.
   * @param <T> Output Java type.
   */
  public static <B extends BuilderForType<T>, T> ColumnStorage<T> buildOverDoubleStorage(
      ColumnDoubleStorage source, B builder, DoubleBuildOperation<B> operation) {
    return buildOverDoubleStorage(source, true, builder, operation);
  }

  /**
   * Iterates over every value of a source double Storage, calling an operation for each step. The
   * operation is expected to append the result to the builder. Use this when wanting to avoid
   * boxing for Long, Boolean or Double builders.
   *
   * @param source the source storage to read from and iterate over.
   * @param preserveNothing if True then Nothing is appended straight to builder otherwise passed to
   *     the operation.
   * @param builder the output builder.
   * @param operation a callback to process a single value.
   * @return a built ColumnStorage from sealing the builder.
   * @param <B> Builder type.
   * @param <T> Output Java type.
   */
  public static <B extends BuilderForType<T>, T> ColumnStorage<T> buildOverDoubleStorage(
      ColumnDoubleStorage source,
      boolean preserveNothing,
      B builder,
      DoubleBuildOperation<B> operation) {
    long size = source.getSize();
    Context context = Context.getCurrent();
    for (long index = 0; index < size; index++) {
      if (preserveNothing && source.isNothing(index)) {
        builder.appendNulls(1);
      } else {
        operation.apply(builder, index, source.getItemAsDouble(index), source.isNothing(index));
      }
      context.safepoint();
    }
    return builder.seal();
  }

  /**
   * Iterates over every value of a source double Storage, calling an operation for each step. The
   * operation is expected to append the result to the builder. Nothing values are appended
   * automatically as Nothing (use the override to control this). Use this when wanting to avoid
   * boxing for Long, Boolean or Double builders.
   *
   * @param source the source storage to read from and iterate over.
   * @param builder the output builder.
   * @param operation a callback to process a single value.
   * @return a built ColumnStorage from sealing the builder.
   * @param <B> Builder type.
   * @param <T> Output Java type.
   */
  public static <B extends BuilderForType<T>, T> ColumnStorage<T> buildOverBooleanStorage(
      ColumnBooleanStorage source, B builder, BooleanBuildOperation<B> operation) {
    return buildOverBooleanStorage(source, true, builder, operation);
  }

  /**
   * Iterates over every value of a source double Storage, calling an operation for each step. The
   * operation is expected to append the result to the builder. Use this when wanting to avoid
   * boxing for Long, Boolean or Double builders.
   *
   * @param source the source storage to read from and iterate over.
   * @param preserveNothing if True then Nothing is appended straight to builder otherwise passed to
   *     the operation.
   * @param builder the output builder.
   * @param operation a callback to process a single value.
   * @return a built ColumnStorage from sealing the builder.
   * @param <B> Builder type.
   * @param <T> Output Java type.
   */
  public static <B extends BuilderForType<T>, T> ColumnStorage<T> buildOverBooleanStorage(
      ColumnBooleanStorage source,
      boolean preserveNothing,
      B builder,
      BooleanBuildOperation<B> operation) {
    long size = source.getSize();
    Context context = Context.getCurrent();
    for (long index = 0; index < size; index++) {
      if (preserveNothing && source.isNothing(index)) {
        builder.appendNulls(1);
      } else {
        operation.apply(builder, index, source.getItemAsBoolean(index), source.isNothing(index));
      }
      context.safepoint();
    }
    return builder.seal();
  }

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
    Context context = Context.getCurrent();
    for (long index = 0; index < size; index++) {
      if (preserveNothing && source.isNothing(index)) {
        builder.appendNulls(1);
      } else {
        var result = operation.apply(index, source.getItemBoxed(index));
        builder.append(result);
      }
      context.safepoint();
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
    Context context = Context.getCurrent();
    for (long index = 0; index < size; index++) {
      if (preserveNothing && source.isNothing(index)) {
        builder.appendNulls(1);
      } else {
        var result = operation.apply(index, source.getItemAsLong(index), source.isNothing(index));
        builder.append(result);
      }
      context.safepoint();
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
    Context context = Context.getCurrent();
    for (long index = 0; index < size; index++) {
      if (preserveNothing && source.isNothing(index)) {
        builder.appendNulls(1);
      } else {
        var result = operation.apply(index, source.getItemAsDouble(index), source.isNothing(index));
        builder.append(result);
      }
      context.safepoint();
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
    Context context = Context.getCurrent();
    for (long index = 0; index < size; index++) {
      if (preserveNothing && source.isNothing(index)) {
        builder.appendNulls(1);
      } else {
        var result =
            operation.apply(index, source.getItemAsBoolean(index), source.isNothing(index));
        builder.append(result);
      }
      context.safepoint();
    }
    return builder.seal();
  }
}
