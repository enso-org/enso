package org.enso.exploratory_benchmark_helpers;

import java.util.BitSet;
import java.util.function.Function;
import org.enso.base.Text_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.BoolStorage;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.data.column.storage.datetime.DateStorage;
import org.enso.table.data.column.storage.numeric.LongStorage;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.problems.ProblemAggregator;

public class MapHelpers {
  public static Storage<String> stringConcatBimap(StringStorage storage1, StringStorage storage2) {
    if (storage1.size() != storage2.size()) {
      throw new IllegalArgumentException("Storage sizes must match");
    }

    int n = storage1.size();
    var builder = Builder.getForText(n, TextType.VARIABLE_LENGTH);
    for (int i = 0; i < n; i++) {
      if (!storage1.isNothing(i) && !storage2.isNothing(i)) {
        builder.append(storage1.getBoxed(i) + storage2.getBoxed(i));
      } else {
        builder.appendNulls(1);
      }
    }
    return builder.seal();
  }

  public static Storage<Long> longAddBimap(LongStorage storage1, LongStorage storage2) {
    if (storage1.size() != storage2.size()) {
      throw new IllegalArgumentException("Storage sizes must match");
    }

    long n = storage1.size();
    var builder = Builder.getForLong(IntegerType.INT_64, n, null);
    for (long i = 0; i < n; i++) {
      if (!storage1.isNothing(i) && !storage2.isNothing(i)) {
        builder.appendLong(storage1.getPrimitive(i) + storage2.getPrimitive(i));
      } else {
        builder.appendNulls(1);
      }
    }
    return builder.seal();
  }

  public static BoolStorage textEndsWith(StringStorage storage, String suffix) {
    int n = storage.size();
    BitSet result = new BitSet();
    BitSet isNothing = new BitSet();
    for (int i = 0; i < n; i++) {
      if (storage.isNothing(i)) {
        isNothing.set(i);
      } else {
        if (Text_Utils.ends_with(storage.getBoxed(i), suffix)) {
          result.set(i);
        }
      }
    }
    return new BoolStorage(result, isNothing, n, false);
  }

  public static Storage<Long> longAdd(LongStorage storage, long shift) {
    int n = storage.size();
    var builder = Builder.getForLong(IntegerType.INT_64, n, null);
    for (int i = 0; i < n; i++) {
      if (!storage.isNothing(i)) {
        builder.appendLong(storage.getPrimitive(i) + shift);
      } else {
        builder.appendNulls(1);
      }
    }
    return builder.seal();
  }

  public static Storage<Long> getYear(DateStorage storage) {
    long n = storage.getSize();
    var builder = Builder.getForLong(IntegerType.INT_64, n, null);
    for (int i = 0; i < n; i++) {
      if (!storage.isNothing(i)) {
        builder.appendLong(storage.getBoxed(i).getYear());
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
        builder.append(fn.apply(storage.getBoxed(i)));
      } else {
        builder.appendNulls(1);
      }
    }
    return builder.seal();
  }
}
