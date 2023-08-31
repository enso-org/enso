package org.enso.table.data.column.storage;

import org.enso.base.Text_Utils;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.builder.StringBuilder;
import org.enso.table.data.column.operation.map.MapOperationStorage;
import org.enso.table.data.column.operation.map.BinaryMapOperation;
import org.enso.table.data.column.operation.map.MapOperationProblemBuilder;
import org.enso.table.data.column.operation.map.UnaryMapOperation;
import org.enso.table.data.column.operation.map.text.LikeOp;
import org.enso.table.data.column.operation.map.text.StringBooleanOp;
import org.enso.table.data.column.operation.map.text.StringIsInOp;
import org.enso.table.data.column.operation.map.text.StringStringOp;
import org.enso.table.data.column.storage.type.TextType;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

import java.util.BitSet;

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
  public Storage<?> fillMissing(Value arg) {
    if (arg.isString()) {
      TextType newType = TextType.maxType(type, TextType.preciseTypeForValue(arg.asString()));
      return fillMissingHelper(arg, new StringBuilder(size(), newType));
    } else {
      return super.fillMissing(arg);
    }
  }

  @Override
  public Builder createDefaultBuilderOfSameType(int capacity) {
    return new StringBuilder(capacity, type);
  }

  private static MapOperationStorage<String, SpecializedStorage<String>> buildOps() {
    MapOperationStorage<String, SpecializedStorage<String>> t = ObjectStorage.buildObjectOps();
    t.add(
        new BinaryMapOperation<>(Maps.EQ) {
          @Override
          public BoolStorage runBinaryMap(
              SpecializedStorage<String> storage,
              Object arg,
              MapOperationProblemBuilder problemBuilder) {
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
              MapOperationProblemBuilder problemBuilder) {
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
          protected BoolStorage runUnaryMap(SpecializedStorage<String> storage, MapOperationProblemBuilder problemBuilder) {
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
}
