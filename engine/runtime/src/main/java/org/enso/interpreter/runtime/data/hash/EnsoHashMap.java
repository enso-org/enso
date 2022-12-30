package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownKeyException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.util.function.Supplier;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.hash.EnsoHashMapBuilder.ValueWithIndex;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

/**
 * Effectively a snapshot of {@link EnsoHashMapBuilder}.
 */
@ExportLibrary(TypesLibrary.class)
@ExportLibrary(InteropLibrary.class)
@Builtin(stdlibName = "Standard.Base.Data.Hash_Map.Hash_Map", name = "Hash_Map")
public final class EnsoHashMap implements TruffleObject {
  private final EnsoHashMapBuilder mapBuilder;
  private final int snapshotSize;

  EnsoHashMap(EnsoHashMapBuilder mapBuilder, int snapshotSize) {
    this.mapBuilder = mapBuilder;
    this.snapshotSize = snapshotSize;
    assert snapshotSize < mapBuilder.getCapacity();
  }

  static EnsoHashMap createWithBuilder(EnsoHashMapBuilder mapBuilder, int snapshotSize) {
    return new EnsoHashMap(mapBuilder, snapshotSize);
  }

  static EnsoHashMap createEmpty() {
    return new EnsoHashMap(
        EnsoHashMapBuilder.createWithCapacity(10),
        0
    );
  }

  @Builtin.Method
  public static EnsoHashMap empty() {
    return createEmpty();
  }

  @Builtin.Method
  public EnsoHashMap insert(Object key, Object value) {
    mapBuilder.add(key, value);
    return mapBuilder.build();
  }

  @Builtin.Method
  public long size() {
    return snapshotSize;
  }

  @Builtin.Method
  public Object get(Object key) {
    ValueWithIndex valueWithIdx = mapBuilder.get(key);
    if (isValueInThisMap(valueWithIdx)) {
      return valueWithIdx.value();
    } else {
      throw DataflowError.withoutTrace("No such key '" + key.toString() + "'", null);
    }
  }

  @Builtin.Method(name = "contains_key")
  public boolean containsKey(Object key) {
    return isValueInThisMap(
        mapBuilder.get(key)
    );
  }

  @Builtin.Method(name = "key_set")
  public Object keySet() {
    throw new UnsupportedOperationException("unimplemented");
  }

  @ExportMessage
  boolean hasHashEntries() {
    return true;
  }

  @ExportMessage
  int getHashSize() {
    return snapshotSize;
  }

  @ExportMessage
  boolean isHashEntryReadable(Object key) {
    return containsKey(key);
  }

  @ExportMessage
  Object readHashValue(Object key) throws UnknownKeyException {
    ValueWithIndex valueWithIdx = mapBuilder.get(key);
    if (isValueInThisMap(valueWithIdx)) {
      return valueWithIdx.value();
    } else {
      throw UnknownKeyException.create(key);
    }
  }

  @ExportMessage
  Object getHashEntriesIterator() throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportMessage(library = TypesLibrary.class)
  boolean hasType() {
    return true;
  }

  @ExportMessage(library = TypesLibrary.class)
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().vector();
  }

  private boolean isValueInThisMap(ValueWithIndex valueWithIndex) {
    return valueWithIndex != null && valueWithIndex.index() <= snapshotSize;
  }
}
