package org.enso.exploratory_benchmark_helpers;

import java.util.BitSet;
import java.util.function.Function;
import org.enso.base.Text_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.InferredBuilder;
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
  public static StringStorage stringConcatBimap(StringStorage storage1, StringStorage storage2) {
    if (storage1.size() != storage2.size()) {
      throw new IllegalArgumentException("Storage sizes must match");
    }

    int n = storage1.size();
    String[] result = new String[n];
    for (int i = 0; i < n; i++) {
      if (!storage1.isNothing(i) && !storage2.isNothing(i)) {
        result[i] = storage1.getItem(i) + storage2.getItem(i);
      } else {
        result[i] = null;
      }
    }
    return new StringStorage(result, n, TextType.VARIABLE_LENGTH);
  }

  public static LongStorage longAddBimap(LongStorage storage1, LongStorage storage2) {
    if (storage1.size() != storage2.size()) {
      throw new IllegalArgumentException("Storage sizes must match");
    }

    int n = storage1.size();
    long[] result = new long[n];
    BitSet isNothing = new BitSet();
    for (int i = 0; i < n; i++) {
      if (!storage1.isNothing(i) && !storage2.isNothing(i)) {
        result[i] = storage1.getItem(i) + storage2.getItem(i);
      } else {
        isNothing.set(i);
      }
    }
    return new LongStorage(result, n, isNothing, IntegerType.INT_64);
  }

  public static BoolStorage textEndsWith(StringStorage storage, String suffix) {
    int n = storage.size();
    BitSet result = new BitSet();
    BitSet isNothing = new BitSet();
    for (int i = 0; i < n; i++) {
      if (storage.isNothing(i)) {
        isNothing.set(i);
      } else {
        if (Text_Utils.ends_with(storage.getItem(i), suffix)) {
          result.set(i);
        }
      }
    }
    return new BoolStorage(result, isNothing, n, false);
  }

  public static LongStorage longAdd(LongStorage storage, long shift) {
    int n = storage.size();
    long[] result = new long[n];
    BitSet isNothing = new BitSet();
    for (int i = 0; i < n; i++) {
      if (!storage.isNothing(i)) {
        result[i] = storage.getItem(i) + shift;
      } else {
        isNothing.set(i);
      }
    }
    return new LongStorage(result, n, isNothing, IntegerType.INT_64);
  }

  public static LongStorage getYear(DateStorage storage) {
    int n = storage.size();
    long[] result = new long[n];
    BitSet isNothing = new BitSet();
    for (int i = 0; i < n; i++) {
      if (!storage.isNothing(i)) {
        result[i] = storage.getItem(i).getYear();
      } else {
        isNothing.set(i);
      }
    }
    return new LongStorage(result, n, isNothing, IntegerType.INT_64);
  }

  public static Storage<?> mapCallback(
      Storage<?> storage,
      Function<Object, Object> fn,
      StorageType expectedType,
      ProblemAggregator problemAggregator) {
    int n = storage.size();
    Builder builder =
        expectedType == null
            ? new InferredBuilder(n, problemAggregator)
            : Builder.getForType(expectedType, n, problemAggregator);
    for (int i = 0; i < n; i++) {
      if (!storage.isNothing(i)) {
        builder.append(fn.apply(storage.getItemBoxed(i)));
      } else {
        builder.appendNulls(1);
      }
    }
    return builder.seal();
  }
}
