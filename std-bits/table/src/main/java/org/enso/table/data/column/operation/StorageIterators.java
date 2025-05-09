package org.enso.table.data.column.operation;

import java.util.function.LongFunction;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.BuilderForType;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.util.ProgressHandler;

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
    try (var progressHandle = ProgressHandler.init("buildObjectOverStorage", source.getSize())) {
      long idx = 0;
      for (S item : source) {
        if (preserveNothing && item == null) {
          builder.appendNulls(1);
        } else {
          operation.apply(builder, idx, item);
        }
        progressHandle.advance();
        idx++;
      }
    }
    return builder.seal();
  }

  /** Generally best to use a typed builder, but if not possible fall back to this. */
  public static ColumnStorage<?> buildObjectOverDoubleStorage(
      ColumnDoubleStorage source,
      boolean preserveNothing,
      Builder builder,
      DoubleBuildObjectOperation operation) {
    try (var progressHandle =
        ProgressHandler.init("buildObjectOverDoubleStorage", source.getSize())) {
      var iterator = source.iterator();
      while (iterator.moveNext()) {
        if (iterator.isNothing()) {
          if (preserveNothing) {
            builder.appendNulls(1);
          } else {
            operation.apply(builder, iterator.getIndex(), Double.NaN, true);
          }
        } else {
          operation.apply(builder, iterator.getIndex(), iterator.getItemAsDouble(), false);
        }
        progressHandle.advance();
      }
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
   * @param <B> Builder type.
   * @param <S> Input Java type.
   * @param <T> Output Java type.
   * @return a built ColumnStorage from sealing the builder.
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
   * @param <B> Builder type.
   * @param <S> Input Java type.
   * @param <T> Output Java type.
   * @return a built ColumnStorage from sealing the builder.
   */
  public static <B extends BuilderForType<T>, S, T> ColumnStorage<T> buildOverStorage(
      ColumnStorage<S> source, boolean preserveNothing, B builder, BuildOperation<B, S> operation) {
    try (var progressHandle = ProgressHandler.init("buildOverStorage", source.getSize())) {
      long idx = 0;
      for (S item : source) {
        if (preserveNothing && item == null) {
          builder.appendNulls(1);
        } else {
          operation.apply(builder, idx, item);
        }
        progressHandle.advance();
        idx++;
      }
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
   * @param <B> Builder type.
   * @param <T> Output Java type.
   * @return a built ColumnStorage from sealing the builder.
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
   * @param <B> Builder type.
   * @param <T> Output Java type.
   * @return a built ColumnStorage from sealing the builder.
   */
  public static <B extends BuilderForType<T>, T> ColumnStorage<T> buildOverLongStorage(
      ColumnLongStorage source,
      boolean preserveNothing,
      B builder,
      LongBuildOperation<B> operation) {
    try (var progressHandle = ProgressHandler.init("buildOverStorage", source.getSize())) {
      var iterator = source.iterator();
      while (iterator.moveNext()) {
        if (iterator.isNothing()) {
          if (preserveNothing) {
            builder.appendNulls(1);
          } else {
            operation.apply(builder, iterator.getIndex(), 0, true);
          }
        } else {
          operation.apply(builder, iterator.getIndex(), iterator.getItemAsLong(), false);
        }
        progressHandle.advance();
      }
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
   * @param <B> Builder type.
   * @param <T> Output Java type.
   * @return a built ColumnStorage from sealing the builder.
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
   * @param <B> Builder type.
   * @param <T> Output Java type.
   * @return a built ColumnStorage from sealing the builder.
   */
  public static <B extends BuilderForType<T>, T> ColumnStorage<T> buildOverDoubleStorage(
      ColumnDoubleStorage source,
      boolean preserveNothing,
      B builder,
      DoubleBuildOperation<B> operation) {
    try (var progressHandle = ProgressHandler.init("buildOverDoubleStorage", source.getSize())) {
      var iterator = source.iterator();
      while (iterator.moveNext()) {
        if (iterator.isNothing()) {
          if (preserveNothing) {
            builder.appendNulls(1);
          } else {
            operation.apply(builder, iterator.getIndex(), Double.NaN, true);
          }
        } else {
          operation.apply(builder, iterator.getIndex(), iterator.getItemAsDouble(), false);
        }
        progressHandle.advance();
      }
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
   * @param <B> Builder type.
   * @param <T> Output Java type.
   * @return a built ColumnStorage from sealing the builder.
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
   * @param <B> Builder type.
   * @param <T> Output Java type.
   * @return a built ColumnStorage from sealing the builder.
   */
  public static <B extends BuilderForType<T>, T> ColumnStorage<T> buildOverBooleanStorage(
      ColumnBooleanStorage source,
      boolean preserveNothing,
      B builder,
      BooleanBuildOperation<B> operation) {
    try (var progressHandle = ProgressHandler.init("buildOverBooleanStorage", source.getSize())) {
      var iterator = source.iterator();
      while (iterator.moveNext()) {
        if (iterator.isNothing()) {
          if (preserveNothing) {
            builder.appendNulls(1);
          } else {
            operation.apply(builder, iterator.getIndex(), false, true);
          }
        } else {
          operation.apply(builder, iterator.getIndex(), iterator.getItemAsBoolean(), false);
        }
        progressHandle.advance();
      }
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
   * @param <S> Input Java type.
   * @param <T> Output Java type.
   * @return a built ColumnStorage from sealing the builder.
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
   * @param <S> Input Java type.
   * @param <T> Output Java type.
   * @return a built ColumnStorage from sealing the builder.
   */
  public static <S, T> ColumnStorage<T> mapOverStorage(
      ColumnStorage<S> source,
      boolean preserveNothing,
      BuilderForType<T> builder,
      MapOperation<S, T> operation) {
    try (var progressHandle = ProgressHandler.init("mapOverStorage", source.getSize())) {
      long idx = 0;
      for (S item : source) {
        if (preserveNothing && item == null) {
          builder.appendNulls(1);
        } else {
          var result = operation.apply(idx, item);
          builder.append(result);
        }

        progressHandle.advance();
        idx++;
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
   * @param <T> Output Java type.
   * @return a built ColumnStorage from sealing the builder.
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
   * @param <T> Output Java type.
   * @return a built ColumnStorage from sealing the builder.
   */
  public static <T> ColumnStorage<T> mapOverLongStorage(
      ColumnLongStorage source,
      boolean preserveNothing,
      BuilderForType<T> builder,
      LongMapOperation<T> operation) {
    try (var progressHandle = ProgressHandler.init("mapOverLongStorage", source.getSize())) {
      var iterator = source.iterator();
      while (iterator.moveNext()) {
        if (iterator.isNothing()) {
          if (preserveNothing) {
            builder.appendNulls(1);
          } else {
            var result = operation.apply(iterator.getIndex(), 0, true);
            builder.append(result);
          }
        } else {
          var result = operation.apply(iterator.getIndex(), iterator.getItemAsLong(), false);
          builder.append(result);
        }
        progressHandle.advance();
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
   * @param <T> Output Java type.
   * @return a built ColumnStorage from sealing the builder.
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
   * @param <T> Output Java type.
   * @return a built ColumnStorage from sealing the builder.
   */
  public static <T> ColumnStorage<T> mapOverDoubleStorage(
      ColumnDoubleStorage source,
      boolean preserveNothing,
      BuilderForType<T> builder,
      DoubleMapOperation<T> operation) {
    try (var progressHandle = ProgressHandler.init("mapOverDoubleStorage", source.getSize())) {
      var iterator = source.iterator();
      while (iterator.moveNext()) {
        if (iterator.isNothing()) {
          if (preserveNothing) {
            builder.appendNulls(1);
          } else {
            var result = operation.apply(iterator.getIndex(), Double.NaN, true);
            builder.append(result);
          }
        } else {
          var result = operation.apply(iterator.getIndex(), iterator.getItemAsDouble(), false);
          builder.append(result);
        }
        progressHandle.advance();
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
   * @param <T> Output Java type.
   * @return a built ColumnStorage from sealing the builder.
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
   * @param <T> Output Java type.
   * @return a built ColumnStorage from sealing the builder.
   */
  public static <T> ColumnStorage<T> mapOverBooleanStorage(
      ColumnBooleanStorage source,
      boolean preserveNothing,
      BuilderForType<T> builder,
      BooleanMapOperation<T> operation) {
    try (var progressHandle = ProgressHandler.init("mapOverBooleanStorage", source.getSize())) {
      var iterator = source.iterator();
      while (iterator.moveNext()) {
        if (iterator.isNothing()) {
          if (preserveNothing) {
            builder.appendNulls(1);
          } else {
            var result = operation.apply(iterator.getIndex(), false, true);
            builder.append(result);
          }
        } else {
          var result = operation.apply(iterator.getIndex(), iterator.getItemAsBoolean(), false);
          builder.append(result);
        }
        progressHandle.advance();
      }
    }

    return builder.seal();
  }

  @FunctionalInterface
  public interface ZipOperation<R, S, T> {
    T apply(long index, R value1, S value2);
  }

  @FunctionalInterface
  public interface LongZipOperation<T> {
    // Note if isNothing1 is true then value1 is undefined, likewise for isNothing2 and value2.
    T apply(long index, long value1, boolean isNothing1, long value2, boolean isNothing2);
  }

  @FunctionalInterface
  public interface LongDoubleZipOperation<T> {
    // Note if isNothing1 is true then value1 is undefined, likewise for isNothing2 and value2.
    T apply(long index, long value1, boolean isNothing1, double value2, boolean isNothing2);
  }

  @FunctionalInterface
  public interface DoubleLongZipOperation<T> {
    // Note if isNothing1 is true then value1 is undefined, likewise for isNothing2 and value2.
    T apply(long index, double value1, boolean isNothing1, long value2, boolean isNothing2);
  }

  @FunctionalInterface
  public interface DoubleZipOperation<T> {
    // Note if isNothing1 is true then value1 is undefined, likewise for isNothing2 and value2.
    T apply(long index, double value1, boolean isNothing1, double value2, boolean isNothing2);
  }

  @FunctionalInterface
  public interface BooleanZipOperation<T> {
    // Note if isNothing1 is true then value1 is undefined, likewise for isNothing2 and value2.
    T apply(long index, boolean value1, boolean isNothing1, boolean value2, boolean isNothing2);
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
   * @param <R> Input Java type for the first source.
   * @param <S> Input Java type for the second source.
   * @param <T> Output Java type for the storage.
   * @return a built ColumnStorage from sealing the builder.
   */
  public static <R, S, T> ColumnStorage<T> zipOverStorages(
      ColumnStorage<R> source1,
      ColumnStorage<S> source2,
      LongFunction<BuilderForType<T>> builderConstructor,
      boolean skipNothing,
      ZipOperation<R, S, T> operation) {
    var iterator1 = source1.iterator();
    var iterator2 = source2.iterator();

    long size = Math.max(source1.getSize(), source2.getSize());
    var builder = builderConstructor.apply(size);

    try (var progressHandle = ProgressHandler.init("zipOverStorages", size)) {
      for (long idx = 0; idx < size; idx++) {
        R value1 = iterator1.hasNext() ? iterator1.next() : null;
        S value2 = iterator2.hasNext() ? iterator2.next() : null;
        if (skipNothing && (value1 == null || value2 == null)) {
          builder.appendNulls(1);
        } else {
          var result = operation.apply(idx, value1, value2);
          builder.append(result);
        }
        progressHandle.advance();
      }
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
   * @param <T> Output Java type for the storage.
   * @return a built ColumnStorage from sealing the builder.
   */
  public static <T> ColumnStorage<T> zipOverLongStorages(
      ColumnLongStorage source1,
      ColumnLongStorage source2,
      LongFunction<BuilderForType<T>> builderConstructor,
      boolean skipNothing,
      LongZipOperation<T> operation) {
    long size = Math.max(source1.getSize(), source2.getSize());
    var builder = builderConstructor.apply(size);

    try (var progressHandle = ProgressHandler.init("zipOverLongStorages", size)) {
      source1
          .iterator()
          .zip(
              source2,
              (idx, value1, isNothing1, value2, isNothing2) -> {
                if (skipNothing && (isNothing1 || isNothing2)) {
                  builder.appendNulls(1);
                } else {
                  var result = operation.apply(idx, value1, isNothing1, value2, isNothing2);
                  builder.append(result);
                }
                progressHandle.advance();
              });
    }

    return builder.seal();
  }

  /**
   * Zips a long and a double storages together, applying an operation to each pair of values. The
   * operation's result is appended to the builder. The builderConstructor will be passed the
   * expected size to create a new builder. If skipNothing is true, then if either value is Nothing,
   * the result will be Nothing and appended automatically.
   *
   * @param source1 the first source storage to read from and iterate over.
   * @param source2 the second source storage to read from and iterate over.
   * @param builderConstructor a function to create a new builder of the correct type.
   * @param skipNothing if true, then if either value is Nothing, the result will be Nothing.
   * @param operation a callback to process a pair of values.
   * @param <T> Output Java type for the storage.
   * @return a built ColumnStorage from sealing the builder.
   */
  public static <T> ColumnStorage<T> zipOverLongDoubleStorages(
      ColumnLongStorage source1,
      ColumnDoubleStorage source2,
      LongFunction<BuilderForType<T>> builderConstructor,
      boolean skipNothing,
      LongDoubleZipOperation<T> operation) {
    long size = Math.max(source1.getSize(), source2.getSize());
    var builder = builderConstructor.apply(size);

    try (var progressHandle = ProgressHandler.init("zipOverLongDoubleStorages", size)) {
      source1
          .iterator()
          .zip(
              source2,
              (idx, value1, isNothing1, value2, isNothing2) -> {
                if (skipNothing && (isNothing1 || isNothing2)) {
                  builder.appendNulls(1);
                } else {
                  var result = operation.apply(idx, value1, isNothing1, value2, isNothing2);
                  builder.append(result);
                }
                progressHandle.advance();
              });
    }

    return builder.seal();
  }

  /**
   * Zips a long and a double storages together, applying an operation to each pair of values. The
   * operation's result is appended to the builder. The builderConstructor will be passed the
   * expected size to create a new builder. If skipNothing is true, then if either value is Nothing,
   * the result will be Nothing and appended automatically.
   *
   * @param source1 the first source storage to read from and iterate over.
   * @param source2 the second source storage to read from and iterate over.
   * @param builderConstructor a function to create a new builder of the correct type.
   * @param skipNothing if true, then if either value is Nothing, the result will be Nothing.
   * @param operation a callback to process a pair of values.
   * @param <T> Output Java type for the storage.
   * @return a built ColumnStorage from sealing the builder.
   */
  public static <T> ColumnStorage<T> zipOverDoubleLongStorages(
      ColumnDoubleStorage source1,
      ColumnLongStorage source2,
      LongFunction<BuilderForType<T>> builderConstructor,
      boolean skipNothing,
      DoubleLongZipOperation<T> operation) {
    long size = Math.max(source1.getSize(), source2.getSize());
    var builder = builderConstructor.apply(size);

    try (var progressHandle = ProgressHandler.init("zipOverDoubleLongStorages", size)) {
      source1
          .iterator()
          .zip(
              source2,
              (idx, value1, isNothing1, value2, isNothing2) -> {
                if (skipNothing && (isNothing1 || isNothing2)) {
                  builder.appendNulls(1);
                } else {
                  var result = operation.apply(idx, value1, isNothing1, value2, isNothing2);
                  builder.append(result);
                }
                progressHandle.advance();
              });
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
   * @param <T> Output Java type for the storage.
   * @return a built ColumnStorage from sealing the builder.
   */
  public static <T> ColumnStorage<T> zipOverDoubleStorages(
      ColumnDoubleStorage source1,
      ColumnDoubleStorage source2,
      LongFunction<BuilderForType<T>> builderConstructor,
      boolean skipNothing,
      DoubleZipOperation<T> operation) {
    long size = Math.max(source1.getSize(), source2.getSize());
    var builder = builderConstructor.apply(size);

    try (var progressHandle = ProgressHandler.init("zipOverDoubleStorages", size)) {
      source1
          .iterator()
          .zip(
              source2,
              (idx, value1, isNothing1, value2, isNothing2) -> {
                if (skipNothing && (isNothing1 || isNothing2)) {
                  builder.appendNulls(1);
                } else {
                  var result = operation.apply(idx, value1, isNothing1, value2, isNothing2);
                  builder.append(result);
                }
                progressHandle.advance();
              });
    }

    return builder.seal();
  }

  /**
   * Zips two boolean storages together, applying an operation to each pair of values. The
   * operation's result is appended to the builder. The builderConstructor will be passed the
   * expected size to create a new builder. If skipNothing is true, then if either value is Nothing,
   * the result will be Nothing and appended automatically.
   *
   * @param source1 the first source storage to read from and iterate over.
   * @param source2 the second source storage to read from and iterate over.
   * @param builderConstructor a function to create a new builder of the correct type.
   * @param skipNothing if true, then if either value is Nothing, the result will be Nothing.
   * @param operation a callback to process a pair of values.
   * @param <T> Output Java type for the storage.
   * @return a built ColumnStorage from sealing the builder.
   */
  public static <T> ColumnStorage<T> zipOverBooleanStorages(
      ColumnBooleanStorage source1,
      ColumnBooleanStorage source2,
      LongFunction<BuilderForType<T>> builderConstructor,
      boolean skipNothing,
      BooleanZipOperation<T> operation) {
    long size = Math.max(source1.getSize(), source2.getSize());
    var builder = builderConstructor.apply(size);

    try (var progressHandle = ProgressHandler.init("zipOverBooleanStorages", size)) {
      source1
          .iterator()
          .zip(
              source2,
              (idx, value1, isNothing1, value2, isNothing2) -> {
                if (skipNothing && (isNothing1 || isNothing2)) {
                  builder.appendNulls(1);
                } else {
                  var result = operation.apply(idx, value1, isNothing1, value2, isNothing2);
                  builder.append(result);
                }
                progressHandle.advance();
              });
    }

    return builder.seal();
  }
}
