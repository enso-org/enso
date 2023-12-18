package org.enso.table.data.column.storage;

import java.util.BitSet;
import org.enso.base.Text_Utils;
import org.enso.table.data.column.builder.StringBuilder;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemAggregator;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.UnaryMapOperation;
import org.enso.table.data.column.operation.map.text.LikeOp;
import org.enso.table.data.column.operation.map.text.StringBooleanOp;
import org.enso.table.data.column.operation.map.text.StringIsInOp;
import org.enso.table.data.column.operation.map.text.StringStringOp;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

/** A column storing strings. */
public final class StringStorage extends SpecializedStorage<String> {

  private final TextType type;

  /**
   * @param data the underlying data
   * @param size the number of items stored
   * @param type the type of the column
   */
  public StringStorage(String[] data, int size, TextType type) {
    super(data, size, buildOps());
    this.type = type;
  }

  @Override
  protected SpecializedStorage<String> newInstance(String[] data, int size) {
    return new StringStorage(data, size, type);
  }

  @Override
  protected String[] newUnderlyingArray(int size) {
    return new String[size];
  }

  @Override
  public TextType getType() {
    return type;
  }

  @Override
  public Storage<?> fillMissing(
      Value arg, StorageType commonType, ProblemAggregator problemAggregator) {
    if (arg.isString()) {
      TextType newType = TextType.maxType(type, TextType.preciseTypeForValue(arg.asString()));
      return fillMissingHelper(arg, new StringBuilder(size(), newType));
    } else {
      return super.fillMissing(arg, commonType, problemAggregator);
    }
  }

  private static MapOperationStorage<String, SpecializedStorage<String>> buildOps() {
    MapOperationStorage<String, SpecializedStorage<String>> t = ObjectStorage.buildObjectOps();
    t.add(
        new BinaryMapOperation<>(Maps.EQ) {
          @Override
          public BoolStorage runBinaryMap(
              SpecializedStorage<String> storage,
              Object arg,
              MapOperationProblemAggregator problemAggregator) {
            BitSet r = new BitSet();
            BitSet missing = new BitSet();
            Context context = Context.getCurrent();
            for (int i = 0; i < storage.size(); i++) {
              if (storage.getItem(i) == null) {
                missing.set(i);
              } else if (arg instanceof String s && Text_Utils.equals(storage.getItem(i), s)) {
                r.set(i);
              }

              context.safepoint();
            }
            return new BoolStorage(r, missing, storage.size(), false);
          }

          @Override
          public BoolStorage runZip(
              SpecializedStorage<String> storage,
              Storage<?> arg,
              MapOperationProblemAggregator problemAggregator) {
            BitSet r = new BitSet();
            BitSet missing = new BitSet();
            Context context = Context.getCurrent();
            for (int i = 0; i < storage.size(); i++) {
              if (storage.getItem(i) == null || i >= arg.size() || arg.isNa(i)) {
                missing.set(i);
              } else if (arg.getItemBoxed(i) instanceof String s
                  && Text_Utils.equals(storage.getItem(i), s)) {
                r.set(i);
              }

              context.safepoint();
            }
            return new BoolStorage(r, missing, storage.size(), false);
          }
        });
    t.add(
        new UnaryMapOperation<>(Maps.IS_EMPTY) {
          @Override
          protected BoolStorage runUnaryMap(
              SpecializedStorage<String> storage, MapOperationProblemAggregator problemAggregator) {
            BitSet r = new BitSet();
            Context context = Context.getCurrent();
            for (int i = 0; i < storage.size; i++) {
              String s = storage.data[i];
              if (s == null || s.isEmpty()) {
                r.set(i);
              }

              context.safepoint();
            }
            return new BoolStorage(r, new BitSet(), storage.size, false);
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
        new StringBooleanOp(Maps.CONTAINS) {
          @Override
          protected boolean doString(String a, String b) {
            return Text_Utils.contains(a, b);
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
    return t;
  }

  @Override
  public StorageType inferPreciseTypeShrunk() {
    if (type.fixedLength()) {
      return type;
    }

    long minLength = Long.MAX_VALUE;
    long maxLength = Long.MIN_VALUE;
    for (int i = 0; i < size(); i++) {
      String s = getItem(i);
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
}
