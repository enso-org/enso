package org.enso.table.data.column.operation;

import java.util.function.LongFunction;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorageWithArray;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnLongStorageWithArray;
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
    if (source instanceof ColumnLongStorageWithArray longArrayStorage) {
      return buildOverLongArrayStorage(longArrayStorage, preserveNothing, builder, operation);
    }

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

  private static <B extends BuilderForType<T>, T> ColumnStorage<T> buildOverLongArrayStorage(
      ColumnLongStorageWithArray source,
      boolean preserveNothing,
      B builder,
      LongBuildOperation<B> operation) {
    var data = source.getArray();
    Context context = Context.getCurrent();
    assert source.getSize() < Integer.MAX_VALUE;
    for (int i = 0; i < source.getSize(); i++) {
      boolean isNothing = source.isNothing(i);
      if (preserveNothing && isNothing) {
        builder.appendNulls(1);
      } else {
        operation.apply(builder, i, data[i], isNothing);
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
    if (source instanceof ColumnDoubleStorageWithArray doubleArrayStorage) {
      return buildOverDoubleArrayStorage(doubleArrayStorage, preserveNothing, builder, operation);
    }

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

  private static <B extends BuilderForType<T>, T> ColumnStorage<T> buildOverDoubleArrayStorage(
      ColumnDoubleStorageWithArray source,
      boolean preserveNothing,
      B builder,
      DoubleBuildOperation<B> operation) {
    var data = source.getArray();
    Context context = Context.getCurrent();
    assert source.getSize() < Integer.MAX_VALUE;
    for (int i = 0; i < source.getSize(); i++) {
      boolean isNothing = source.isNothing(i);
      if (preserveNothing && isNothing) {
        builder.appendNulls(1);
      } else {
        operation.apply(builder, i, data[i], isNothing);
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
  public interface MapOperation<S, T> {
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
      ColumnStorage<S> source, BuilderForType<T> builder, MapOperation<S, T> operation) {
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
      MapOperation<S, T> operation) {
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
    if (source instanceof ColumnLongStorageWithArray longArrayStorage) {
      return mapOverLongArrayStorage(longArrayStorage, preserveNothing, builder, operation);
    }

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

  private static <T> ColumnStorage<T> mapOverLongArrayStorage(
      ColumnLongStorageWithArray source,
      boolean preserveNothing,
      BuilderForType<T> builder,
      LongMapOperation<T> operation) {
    var data = source.getArray();
    Context context = Context.getCurrent();
    assert source.getSize() < Integer.MAX_VALUE;
    for (int index = 0; index < source.getSize(); index++) {
      if (preserveNothing && source.isNothing(index)) {
        builder.appendNulls(1);
      } else {
        var result = operation.apply(index, data[index], source.isNothing(index));
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
    if (source instanceof ColumnDoubleStorageWithArray doubleArrayStorage) {
      return mapOverDoubleArrayStorage(doubleArrayStorage, preserveNothing, builder, operation);
    }

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

  private static <T> ColumnStorage<T> mapOverDoubleArrayStorage(
      ColumnDoubleStorageWithArray source,
      boolean preserveNothing,
      BuilderForType<T> builder,
      DoubleMapOperation<T> operation) {
    var data = source.getArray();
    Context context = Context.getCurrent();
    assert source.getSize() < Integer.MAX_VALUE;
    for (int index = 0; index < source.getSize(); index++) {
      if (preserveNothing && source.isNothing(index)) {
        builder.appendNulls(1);
      } else {
        var result = operation.apply(index, data[index], source.isNothing(index));
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

  @FunctionalInterface
  public interface ZipOperation<R, S, T> {
    T apply(long index, R value1, S value2);
  }

  @FunctionalInterface
  public interface LongZipOperation<T> {
    T apply(long index, long value1, boolean isNothing1, long value2, boolean isNothing2);
  }

  @FunctionalInterface
  public interface DoubleZipOperation<T> {
    T apply(long index, double value1, boolean isNothing1, double value2, boolean isNothing2);
  }

  /**
   * Zips two storages together, applying an operation to each pair of values. The operation's
   * result is appended to the builder. The builderConstructor will be passed the expected size to
   * create a new builder. If skipNothing is true, then if either value is Nothing, the result will
   * be Nothing and appended automatically.
   *
   * @param source1 the first source storage to read from and iterate over.
   * @param source2 the second source storage to read from and iterate over.
   * @param builderConstructor a function to create a new builder of the correct type.
   * @param skipNothing if true, then if either value is Nothing, the result will be Nothing.
   * @param operation a callback to process a pair of values.
   * @return a built ColumnStorage from sealing the builder.
   * @param <R> Input Java type for the first source.
   * @param <S> Input Java type for the second source.
   * @param <T> Output Java type for the storage.
   */
  public static <R, S, T> ColumnStorage<T> zipOverStorages(
      ColumnStorage<R> source1,
      ColumnStorage<S> source2,
      LongFunction<BuilderForType<T>> builderConstructor,
      boolean skipNothing,
      ZipOperation<R, S, T> operation) {
    long size1 = source1.getSize();
    long size2 = source2.getSize();

    long size = Math.max(size1, size2);
    var builder = builderConstructor.apply(size);

    Context context = Context.getCurrent();

    for (long index = 0; index < size; index++) {
      R value1 = index < size1 ? source1.getItemBoxed(index) : null;
      S value2 = index < size2 ? source2.getItemBoxed(index) : null;

      if (skipNothing && (value1 == null || value2 == null)) {
        builder.appendNulls(1);
      } else {
        var result = operation.apply(index, value1, value2);
        builder.append(result);
      }

      context.safepoint();
    }

    return builder.seal();
  }

  /**
   * Zips two long storages together, applying an operation to each pair of values. The operation's
   * result is appended to the builder. The builderConstructor will be passed the expected size to
   * create a new builder. If skipNothing is true, then if either value is Nothing, the result will
   * be Nothing and appended automatically.
   *
   * @param source1 the first source storage to read from and iterate over.
   * @param source2 the second source storage to read from and iterate over.
   * @param builderConstructor a function to create a new builder of the correct type.
   * @param skipNothing if true, then if either value is Nothing, the result will be Nothing.
   * @param operation a callback to process a pair of values.
   * @return a built ColumnStorage from sealing the builder.
   * @param <T> Output Java type for the storage.
   */
  public static <T> ColumnStorage<T> zipOverLongStorages(
      ColumnLongStorage source1,
      ColumnLongStorage source2,
      LongFunction<BuilderForType<T>> builderConstructor,
      boolean skipNothing,
      LongZipOperation<T> operation) {
    if (source1 instanceof ColumnLongStorageWithArray longArrayStorage1
        && source2 instanceof ColumnLongStorageWithArray longArrayStorage2) {
      return zipOverLongArrayStorages(
          longArrayStorage1, longArrayStorage2, builderConstructor, skipNothing, operation);
    }

    long size1 = source1.getSize();
    long size2 = source2.getSize();

    long size = Math.max(size1, size2);
    var builder = builderConstructor.apply(size);

    Context context = Context.getCurrent();

    for (long index = 0; index < size; index++) {
      boolean isNothing1 = index >= size1 || source1.isNothing(index);
      boolean isNothing2 = index >= size2 || source2.isNothing(index);
      if (skipNothing && (isNothing1 || isNothing2)) {
        builder.appendNulls(1);
      } else {
        long value1 = isNothing1 ? 0 : source1.getItemAsLong(index);
        long value2 = isNothing2 ? 0 : source2.getItemAsLong(index);
        var result = operation.apply(index, value1, isNothing1, value2, isNothing2);
        builder.append(result);
      }

      context.safepoint();
    }

    return builder.seal();
  }

  private static <T> ColumnStorage<T> zipOverLongArrayStorages(
      ColumnLongStorageWithArray source1,
      ColumnLongStorageWithArray source2,
      LongFunction<BuilderForType<T>> builderConstructor,
      boolean skipNothing,
      LongZipOperation<T> operation) {
    var data1 = source1.getArray();
    long size1 = source1.getSize();
    var data2 = source2.getArray();
    long size2 = source2.getSize();

    long size = Math.max(size1, size2);
    assert size < Integer.MAX_VALUE;
    var builder = builderConstructor.apply(size);

    Context context = Context.getCurrent();

    for (int index = 0; index < size; index++) {
      boolean isNothing1 = index >= size1 || source1.isNothing(index);
      boolean isNothing2 = index >= size2 || source2.isNothing(index);
      if (skipNothing && (isNothing1 || isNothing2)) {
        builder.appendNulls(1);
      } else {
        long value1 = isNothing1 ? 0 : data1[index];
        long value2 = isNothing2 ? 0 : data2[index];
        var result = operation.apply(index, value1, isNothing1, value2, isNothing2);
        builder.append(result);
      }

      context.safepoint();
    }

    return builder.seal();
  }

  /**
   * Zips two double storages together, applying an operation to each pair of values. The
   * operation's result is appended to the builder. The builderConstructor will be passed the
   * expected size to create a new builder. If skipNothing is true, then if either value is Nothing,
   * the result will be Nothing and appended automatically.
   *
   * @param source1 the first source storage to read from and iterate over.
   * @param source2 the second source storage to read from and iterate over.
   * @param builderConstructor a function to create a new builder of the correct type.
   * @param skipNothing if true, then if either value is Nothing, the result will be Nothing.
   * @param operation a callback to process a pair of values.
   * @return a built ColumnStorage from sealing the builder.
   * @param <T> Output Java type for the storage.
   */
  public static <T> ColumnStorage<T> zipOverDoubleStorages(
      ColumnDoubleStorage source1,
      ColumnDoubleStorage source2,
      LongFunction<BuilderForType<T>> builderConstructor,
      boolean skipNothing,
      DoubleZipOperation<T> operation) {
    if (source1 instanceof ColumnDoubleStorageWithArray doubleArrayStorage1
        && source2 instanceof ColumnDoubleStorageWithArray doubleArrayStorage2) {
      return zipOverDoubleArrayStorages(
          doubleArrayStorage1, doubleArrayStorage2, builderConstructor, skipNothing, operation);
    }

    long size1 = source1.getSize();
    long size2 = source2.getSize();

    long size = Math.max(size1, size2);
    var builder = builderConstructor.apply(size);

    Context context = Context.getCurrent();

    for (long index = 0; index < size; index++) {
      boolean isNothing1 = index >= size1 || source1.isNothing(index);
      boolean isNothing2 = index >= size2 || source2.isNothing(index);
      if (skipNothing && (isNothing1 || isNothing2)) {
        builder.appendNulls(1);
      } else {
        double value1 = isNothing1 ? 0 : source1.getItemAsDouble(index);
        double value2 = isNothing2 ? 0 : source2.getItemAsDouble(index);
        var result = operation.apply(index, value1, isNothing1, value2, isNothing2);
        builder.append(result);
      }

      context.safepoint();
    }

    return builder.seal();
  }

  private static <T> ColumnStorage<T> zipOverDoubleArrayStorages(
      ColumnDoubleStorageWithArray source1,
      ColumnDoubleStorageWithArray source2,
      LongFunction<BuilderForType<T>> builderConstructor,
      boolean skipNothing,
      DoubleZipOperation<T> operation) {
    var data1 = source1.getArray();
    long size1 = source1.getSize();
    var data2 = source2.getArray();
    long size2 = source2.getSize();

    long size = Math.max(size1, size2);
    assert size < Integer.MAX_VALUE;
    var builder = builderConstructor.apply(size);

    Context context = Context.getCurrent();

    for (int index = 0; index < size; index++) {
      boolean isNothing1 = index >= size1 || source1.isNothing(index);
      boolean isNothing2 = index >= size2 || source2.isNothing(index);
      if (skipNothing && (isNothing1 || isNothing2)) {
        builder.appendNulls(1);
      } else {
        double value1 = isNothing1 ? 0 : data1[index];
        double value2 = isNothing2 ? 0 : data2[index];
        var result = operation.apply(index, value1, isNothing1, value2, isNothing2);
        builder.append(result);
      }

      context.safepoint();
    }

    return builder.seal();
  }
}
