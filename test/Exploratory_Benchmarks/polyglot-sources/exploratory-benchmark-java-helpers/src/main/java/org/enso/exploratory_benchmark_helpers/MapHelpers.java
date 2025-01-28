package org.enso.exploratory_benchmark_helpers;

import java.util.function.Function;
import org.enso.base.Text_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.data.column.storage.datetime.DateStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.problems.BlackholeProblemAggregator;
import org.enso.table.problems.ProblemAggregator;

public class MapHelpers {
  public static Storage<String> stringConcatBimap(StringStorage storage1, StringStorage storage2) {
    if (storage1.getSize() != storage2.getSize()) {
      throw new IllegalArgumentException("Storage sizes must match");
    }

    long n = storage1.getSize();
    var builder = Builder.getForText(TextType.VARIABLE_LENGTH, n);
    for (long i = 0; i < n; i++) {
      if (!storage1.isNothing(i) && !storage2.isNothing(i)) {
        builder.append(storage1.getItemBoxed(i) + storage2.getItemBoxed(i));
      } else {
        builder.appendNulls(1);
      }
    }
    return builder.seal();
  }

  public static Storage<Long> longAddBimap(LongStorage storage1, LongStorage storage2) {
    if (storage1.getSize() != storage2.getSize()) {
      throw new IllegalArgumentException("Storage sizes must match");
    }

    long n = storage1.getSize();
    var builder = Builder.getForLong(IntegerType.INT_64, n, BlackholeProblemAggregator.INSTANCE);
    for (long i = 0; i < n; i++) {
      if (!storage1.isNothing(i) && !storage2.isNothing(i)) {
        builder.appendLong(storage1.getItemAsLong(i) + storage2.getItemAsLong(i));
      } else {
        builder.appendNulls(1);
      }
    }
    return builder.seal();
  }

  public static Storage<Boolean> textEndsWith(StringStorage storage, String suffix) {
    long n = storage.getSize();
    var builder = Builder.getForBoolean(n);
    for (long i = 0; i < n; i++) {
      if (storage.isNothing(i)) {
        builder.appendNulls(1);
      } else {
        builder.appendBoolean(Text_Utils.ends_with(storage.getItemBoxed(i), suffix));
      }
    }
    return builder.seal();
  }

  public static Storage<Long> longAdd(LongStorage storage, long shift) {
    long n = storage.getSize();
    var builder = Builder.getForLong(IntegerType.INT_64, n, BlackholeProblemAggregator.INSTANCE);
    for (long i = 0; i < n; i++) {
      if (!storage.isNothing(i)) {
        builder.appendLong(storage.getItemAsLong(i) + shift);
      } else {
        builder.appendNulls(1);
      }
    }
    return builder.seal();
  }

  public static Storage<Long> getYear(DateStorage storage) {
    long n = storage.getSize();
    var builder = Builder.getForLong(IntegerType.INT_64, n, BlackholeProblemAggregator.INSTANCE);
    for (long i = 0; i < n; i++) {
      if (!storage.isNothing(i)) {
        builder.appendLong(storage.getItemBoxed(i).getYear());
      } else {
        builder.appendNulls(1);
      }
    }
    return builder.seal();
  }

  public static Storage<?> mapCallback(
      Storage<?> storage,
      Function<Object, Object> fn,
      StorageType expectedType,
      ProblemAggregator problemAggregator) {
    long n = storage.getSize();
    Builder builder = Builder.getForType(expectedType, n, problemAggregator);
    for (long i = 0; i < n; i++) {
      if (!storage.isNothing(i)) {
        builder.append(fn.apply(storage.getItemBoxed(i)));
      } else {
        builder.appendNulls(1);
      }
    }
    return builder.seal();
  }
}
