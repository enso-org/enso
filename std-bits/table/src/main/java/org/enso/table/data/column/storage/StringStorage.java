package org.enso.table.data.column.storage;

import java.util.BitSet;
import org.enso.base.Text_Utils;
import org.enso.table.data.column.builder.object.StringBuilder;
import org.enso.table.data.column.operation.map.MapOpStorage;
import org.enso.table.data.column.operation.map.MapOperation;
import org.enso.table.data.column.operation.map.UnaryMapOperation;
import org.enso.table.data.column.operation.map.text.LikeOp;
import org.enso.table.data.column.operation.map.text.StringBooleanOp;
import org.graalvm.polyglot.Value;

/** A column storing strings. */
public class StringStorage extends SpecializedStorage<String> {

  /**
   * @param data the underlying data
   * @param size the number of items stored
   */
  public StringStorage(String[] data, int size) {
    super(data, size, ops);
  }

  @Override
  protected SpecializedStorage<String> newInstance(String[] data, int size) {
    return new StringStorage(data, size);
  }

  @Override
  protected String[] newUnderlyingArray(int size) {
    return new String[size];
  }

  @Override
  public int getType() {
    return Type.STRING;
  }

  private static final MapOpStorage<SpecializedStorage<String>> ops = buildOps();

  @Override
  protected boolean isOpVectorized(String name) {
    return ops.isSupported(name);
  }

  @Override
  protected Storage runVectorizedMap(String name, Object argument) {
    return ops.runMap(name, this, argument);
  }

  @Override
  protected Storage runVectorizedZip(String name, Storage argument) {
    return ops.runZip(name, this, argument);
  }

  @Override
  public Storage fillMissing(Value arg) {
    if (arg.isString()) {
      return fillMissingHelper(arg, new StringBuilder(size()));
    } else {
      return super.fillMissing(arg);
    }
  }

  private static MapOpStorage<SpecializedStorage<String>> buildOps() {
    MapOpStorage<SpecializedStorage<String>> t = ObjectStorage.buildObjectOps();
    t.add(
        new MapOperation<>(Maps.EQ) {
          @Override
          public Storage runMap(SpecializedStorage<String> storage, Object arg) {
            BitSet r = new BitSet();
            BitSet missing = new BitSet();
            for (int i = 0; i < storage.size(); i++) {
              if (storage.getItem(i) == null) {
                missing.set(i);
              } else if (arg instanceof String s && Text_Utils.equals(storage.getItem(i), s)) {
                r.set(i);
              }
            }
            return new BoolStorage(r, missing, storage.size(), false);
          }

          @Override
          public Storage runZip(SpecializedStorage<String> storage, Storage arg) {
            BitSet r = new BitSet();
            BitSet missing = new BitSet();
            for (int i = 0; i < storage.size(); i++) {
              if (storage.getItem(i) == null || i >= arg.size() || arg.isNa(i)) {
                missing.set(i);
              } else if (arg.getItemBoxed(i) instanceof String s
                  && Text_Utils.equals(storage.getItem(i), s)) {
                r.set(i);
              }
            }
            return new BoolStorage(r, missing, storage.size(), false);
          }
        });
    t.add(
        new UnaryMapOperation<>(Maps.IS_EMPTY) {
          @Override
          protected Storage run(SpecializedStorage<String> storage) {
            BitSet r = new BitSet();
            for (int i = 0; i < storage.size; i++) {
              String s = storage.data[i];
              if (s == null || s.isEmpty()) {
                r.set(i);
              }
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
    return t;
  }
}
