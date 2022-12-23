package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownKeyException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.util.ArrayList;
import java.util.List;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.graalvm.collections.EconomicMap;

@ExportLibrary(TypesLibrary.class)
@ExportLibrary(InteropLibrary.class)
@Builtin(stdlibName = "Standard.Base.Data.Hash_Map.Hash_Map", name = "Hash_Map")
public final class EnsoHashMap implements TruffleObject {
  private final EconomicMap<Object, Object> storage;

  private EnsoHashMap(Object vector) {
    InteropLibrary interop = InteropLibrary.getUncached();
    assert interop.hasArrayElements(vector);
    try {
      long arraySize = interop.getArraySize(vector);
      assert arraySize % 2 == 0;

      storage = EconomicMap.create((int) arraySize);
      for (int i = 0; i < arraySize - 1; i++) {
        Object key = interop.readArrayElement(vector, i);
        Object value = interop.readArrayElement(vector, i + 1);
        storage.put(key, value);
      }
    } catch (UnsupportedMessageException | InvalidArrayIndexException e) {
      throw new IllegalStateException(e);
    }
  }

  @Builtin.Method(name = "create_from_vector")
  public static EnsoHashMap createFromVector(Object vector) {
    return new EnsoHashMap(vector);
  }

  @Builtin.Method
  public long size() {
    return storage.size();
  }

  @Builtin.Method
  public Object get(Object key) {
    if (storage.containsKey(key)) {
      return storage.get(key);
    } else {
      throw DataflowError.withoutTrace("No such key '" + key.toString() + "'", null);
    }
  }

  @Builtin.Method(name = "contains_key")
  public boolean containsKey(Object key) {
    return storage.containsKey(key);
  }

  @Builtin.Method(name = "key_set")
  public Object keySet() {
    List<Object> keys = new ArrayList<>();
    for (Object key : storage.getKeys()) {
      keys.add(key);
    }
    return keys;
  }

  @ExportMessage
  boolean hasHashEntries() {
    return true;
  }

  @ExportMessage
  int getHashSize() {
    return storage.size();
  }

  @ExportMessage
  boolean isHashEntryReadable(Object key) {
    return storage.containsKey(key);
  }

  @ExportMessage
  Object readHashValue(Object key) throws UnknownKeyException {
    if (storage.containsKey(key)) {
      return storage.get(key);
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
}
