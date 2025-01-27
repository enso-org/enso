package org.enso.table.data.column.storage;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import org.enso.base.CompareException;
import org.enso.base.Text_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.operation.CountUntrimmed;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.text.CoalescingStringStringOp;
import org.enso.table.data.column.operation.map.text.LikeOp;
import org.enso.table.data.column.operation.map.text.StringBooleanOp;
import org.enso.table.data.column.operation.map.text.StringIsInOp;
import org.enso.table.data.column.operation.map.text.StringLongToStringOp;
import org.enso.table.data.column.operation.map.text.StringStringOp;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.graalvm.polyglot.Context;
import org.slf4j.Logger;

/** A column storing strings. */
public final class StringStorage extends SpecializedStorage<String> {
  private static final Logger LOGGER = org.slf4j.LoggerFactory.getLogger(StringStorage.class);
  private Future<Long> untrimmedCount;

  /**
   * @param data the underlying data
   * @param type the type of the column
   */
  public StringStorage(String[] data, TextType type) {
    super(type, data, buildOps());

    untrimmedCount =
        CompletableFuture.supplyAsync(
            () -> CountUntrimmed.compute(this, CountUntrimmed.DEFAULT_SAMPLE_SIZE, null));
  }

  public static StringStorage makeEmpty(TextType type, long size) {
    int intSize = Builder.checkSize(size);
    return new StringStorage(new String[intSize], type);
  }

  @Override
  public TextType getType() {
    // As the type is fixed, we can safely cast it.
    return (TextType) super.getType();
  }

  @Override
  protected SpecializedStorage<String> newInstance(String[] data) {
    return new StringStorage(data, getType());
  }

  @Override
  protected String[] newUnderlyingArray(int size) {
    return new String[size];
  }

  /**
   * Counts the number of cells in the columns with whitespace. If the calculation fails then it
   * returns null.
   *
   * @return the number of cells with whitespace
   */
  public Long cachedUntrimmedCount() throws InterruptedException {
    if (untrimmedCount.isCancelled()) {
      // Need to recompute the value, as was cancelled.
      untrimmedCount =
          CompletableFuture.completedFuture(
              CountUntrimmed.compute(
                  this, CountUntrimmed.DEFAULT_SAMPLE_SIZE, Context.getCurrent()));
    }

    try {
      return untrimmedCount.get();
    } catch (ExecutionException e) {
      LOGGER.error("Failed to compute untrimmed count", e);
      return null;
    }
  }

  private static MapOperationStorage<String, SpecializedStorage<String>> buildOps() {
    MapOperationStorage<String, SpecializedStorage<String>> t = new MapOperationStorage<>();
    t.add(
        new BinaryMapOperation<>(Maps.EQ) {
          @Override
          public Storage<Boolean> runBinaryMap(
              SpecializedStorage<String> storage,
              Object arg,
              MapOperationProblemAggregator problemAggregator) {
            long size = storage.getSize();
            var builder = Builder.getForBoolean(size);
            Context context = Context.getCurrent();
            for (long i = 0; i < size; i++) {
              if (storage.getItemBoxed(i) == null || arg == null) {
                builder.appendNulls(1);
              } else {
                builder.appendBoolean(
                    arg instanceof String s && Text_Utils.equals(storage.getItemBoxed(i), s));
              }
              context.safepoint();
            }
            return builder.seal();
          }

          @Override
          public Storage<Boolean> runZip(
              SpecializedStorage<String> storage,
              Storage<?> arg,
              MapOperationProblemAggregator problemAggregator) {
            long size = storage.getSize();
            var builder = Builder.getForBoolean(size);
            Context context = Context.getCurrent();
            for (long i = 0; i < size; i++) {
              if (storage.getItemBoxed(i) == null || i >= arg.getSize() || arg.isNothing(i)) {
                builder.appendNulls(1);
              } else {
                builder.appendBoolean(
                    arg.getItemBoxed(i) instanceof String s
                        && Text_Utils.equals(storage.getItemBoxed(i), s));
              }
              context.safepoint();
            }
            return builder.seal();
          }
        });
    t.add(
        new StringBooleanOp(Maps.STARTS_WITH) {
          @Override
          protected boolean doString(String a, String b) {
            return Text_Utils.starts_with(a, b);
          }
        });
    t.add(
        new StringBooleanOp(Maps.ENDS_WITH) {
          @Override
          protected boolean doString(String a, String b) {
            return Text_Utils.ends_with(a, b);
          }
        });
    t.add(
        new StringLongToStringOp(Maps.TEXT_LEFT) {
          @Override
          protected String doOperation(String a, long b) {
            return Text_Utils.take_prefix(a, b);
          }
        });
    t.add(
        new StringLongToStringOp(Maps.TEXT_RIGHT) {
          @Override
          protected String doOperation(String a, long b) {
            return Text_Utils.take_suffix(a, b);
          }
        });
    t.add(
        new StringBooleanOp(Maps.CONTAINS) {
          @Override
          protected boolean doString(String a, String b) {
            return Text_Utils.contains(a, b);
          }
        });
    t.add(
        new StringComparisonOp(Maps.LT) {
          @Override
          protected boolean doString(String a, String b) {
            return Text_Utils.compare_normalized(a, b) < 0;
          }
        });
    t.add(
        new StringComparisonOp(Maps.LTE) {
          @Override
          protected boolean doString(String a, String b) {
            return Text_Utils.compare_normalized(a, b) <= 0;
          }
        });
    t.add(
        new StringComparisonOp(Maps.GT) {
          @Override
          protected boolean doString(String a, String b) {
            return Text_Utils.compare_normalized(a, b) > 0;
          }
        });
    t.add(
        new StringComparisonOp(Maps.GTE) {
          @Override
          protected boolean doString(String a, String b) {
            return Text_Utils.compare_normalized(a, b) >= 0;
          }
        });
    t.add(new LikeOp());
    t.add(new StringIsInOp<>());
    t.add(
        new StringStringOp(Maps.ADD) {
          @Override
          protected String doString(String a, String b) {
            return a + b;
          }

          @Override
          protected TextType computeResultType(TextType a, TextType b) {
            return TextType.concatTypes(a, b);
          }
        });
    t.add(
        new CoalescingStringStringOp(Maps.MIN) {
          @Override
          protected String doString(String a, String b) {
            if (Text_Utils.compare_normalized(a, b) < 0) {
              return a;
            } else {
              return b;
            }
          }

          @Override
          protected TextType computeResultType(TextType a, TextType b) {
            return TextType.maxType(a, b);
          }
        });
    t.add(
        new CoalescingStringStringOp(Maps.MAX) {
          @Override
          protected String doString(String a, String b) {
            if (Text_Utils.compare_normalized(a, b) > 0) {
              return a;
            } else {
              return b;
            }
          }

          @Override
          protected TextType computeResultType(TextType a, TextType b) {
            return TextType.maxType(a, b);
          }
        });
    return t;
  }

  @Override
  public StorageType inferPreciseTypeShrunk() {
    var type = getType();
    if (type.fixedLength()) {
      return type;
    }

    long minLength = Long.MAX_VALUE;
    long maxLength = Long.MIN_VALUE;
    for (long i = 0; i < getSize(); i++) {
      String s = getItemBoxed(i);
      if (s != null) {
        long length = Text_Utils.grapheme_length(s);
        minLength = Math.min(minLength, length);
        maxLength = Math.max(maxLength, length);
      }
    }

    // maxLength will be <0 if all values were null and will be ==0 if all values were empty
    // strings.
    // In both of these cases, we avoid shrinking the type and return the original type instead.
    if (maxLength <= 0) {
      return getType();
    }

    final long SHORT_LENGTH_THRESHOLD = 255;
    if (minLength == maxLength) {
      return TextType.fixedLength(minLength);
    } else if (maxLength <= SHORT_LENGTH_THRESHOLD
        && (type.maxLength() < 0 || SHORT_LENGTH_THRESHOLD < type.maxLength())) {
      // If the string was unbounded or the bound was larger than 255, we shrink it to 255.
      return TextType.variableLengthWithLimit(SHORT_LENGTH_THRESHOLD);
    } else {
      // Otherwise, we return the original type (because it was either smaller than the proposed 255
      // bound, or the
      // existing elements to do not fit into the 255 bound).
      return getType();
    }
  }

  private abstract static class StringComparisonOp extends StringBooleanOp {
    public StringComparisonOp(String name) {
      super(name);
    }

    @Override
    protected boolean doObject(String a, Object o) {
      throw new CompareException(a, o);
    }
  }
}
