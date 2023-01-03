package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownKeyException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.data.hash.EnsoHashMapBuilder.ValueWithIndex;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.graalvm.collections.EconomicMap;
import org.graalvm.collections.MapCursor;

/**
 * Effectively a snapshot of {@link EnsoHashMapBuilder}.
 * Caches materialized vector in case {@code to_vector} method is called.
 */
@ExportLibrary(TypesLibrary.class)
@ExportLibrary(InteropLibrary.class)
@Builtin(stdlibName = "Standard.Base.Data.Hash_Map.Hash_Map", name = "Hash_Map")
public final class EnsoHashMap implements TruffleObject {
  private final EnsoHashMapBuilder mapBuilder;
  private final int snapshotSize;
  private Object cachedVectorRepresentation;

  private EnsoHashMap(EnsoHashMapBuilder mapBuilder, int snapshotSize) {
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

  @Builtin.Method(name = "from_flat_vector")
  @Builtin.Specialize
  public static EnsoHashMap fromFlatVector(Object flatVector,
      @CachedLibrary("flatVector") InteropLibrary interop) {
    if (!interop.hasArrayElements(flatVector)) {
      throw new PanicException(
          "Value not a vector: " + interop.toDisplayString(flatVector),
          interop
      );
    }
    var mapBuilder = EnsoHashMapBuilder.createWithCapacity(10);
    int arrSize = 0;
    try {
      arrSize = (int) interop.getArraySize(flatVector);
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
    if (arrSize % 2 != 0) {
      throw new PanicException(
          "Array size (" + arrSize + ") is not even. Has to be in form [key1, val1, key2, val2, ...]",
          interop
      );
    }
    for (int i = 0; i < arrSize; i += 2) {
      if (!interop.isArrayElementReadable(flatVector, i)
          || !interop.isArrayElementReadable(flatVector, i + 1)) {
        throw new PanicException(
            String.format(
                "Array element at %d from array %s not readable",
                i,
                interop.toDisplayString(flatVector)
            ),
            interop);
      }
      try {
        Object key = interop.readArrayElement(flatVector, i);
        Object value = interop.readArrayElement(flatVector, i + 1);
        mapBuilder.add(key, value);
      } catch (UnsupportedMessageException | InvalidArrayIndexException e) {
        throw new IllegalStateException(e);
      }
    }
    int keysCount = arrSize / 2;
    var hashMap = createWithBuilder(mapBuilder, keysCount);
    // Cache vector.
    hashMap.cachedVectorRepresentation = FlatKeyValueVector.fromInteropVector(flatVector, interop);
    return hashMap;
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

  @Builtin.Method(name = "to_flat_vector")
  public Object toVector() {
    return getCachedVectorRepresentation();
  }

  @Builtin.Method(name = "to_text")
  public Object toText() {
    return toDisplayString(false);
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
  boolean isHashEntryExisting(Object key) {
    return containsKey(key);
  }

  @ExportMessage
  boolean isHashEntryReadable(Object key) {
    return isHashEntryExisting(key);
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
  Object getHashEntriesIterator(
      @CachedLibrary(limit = "3") InteropLibrary interop) {
    try {
      return interop.getIterator(getCachedVectorRepresentation());
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException(e);
    }
  }

  @ExportMessage(library = TypesLibrary.class)
  boolean hasType() {
    return true;
  }

  @ExportMessage(library = TypesLibrary.class)
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().hashMap();
  }

  @ExportMessage
  Object toDisplayString(boolean allowSideEffects) {
    InteropLibrary interop = InteropLibrary.getUncached();
    Object vector = getCachedVectorRepresentation();
    var sb = new StringBuilder();
    try {
      long vecSize = interop.getArraySize(vector);
      assert vecSize % 2 == 0;
      sb.append("{");
      int i = 0;
      while (i < vecSize) {
        Object key = interop.readArrayElement(vector, i++);
        Object value = interop.readArrayElement(vector, i++);
        sb.append(key).append("=").append(value).append(", ");
      }
      if (vecSize > 0) {
        // Replace last comma
        sb.delete(sb.length() - 2, sb.length());
      } else {
        sb.append("}");
      }
    } catch (UnsupportedMessageException | InvalidArrayIndexException e) {
      return "Error thrown during evaluation of EnsoHashMap.toDisplayString: " + e;
    }
    return sb.toString();
  }

  private Object getCachedVectorRepresentation() {
    if (cachedVectorRepresentation == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      if (size() == 0) {
        cachedVectorRepresentation = Vector.fromArray(new EmptyKeyValueVector());
      } else {
        cachedVectorRepresentation =
            Vector.fromArray(FlatKeyValueVector.fromBuilderStorage(mapBuilder.getStorage(), snapshotSize));
      }
    }
    return cachedVectorRepresentation;
  }

  private boolean isValueInThisMap(ValueWithIndex valueWithIndex) {
    return valueWithIndex != null && valueWithIndex.index() <= snapshotSize;
  }

  @ExportLibrary(InteropLibrary.class)
  static final class FlatKeyValueVector implements TruffleObject {
    private final Object[] keyValueArray;

    private FlatKeyValueVector(int arrSize) {
      this.keyValueArray = new Object[arrSize];
    }

    static FlatKeyValueVector fromBuilderStorage(EconomicMap<Object, ValueWithIndex> storage, int maxKeyIdx) {
      assert storage.size() > 0;
      MapCursor<Object, ValueWithIndex> entries = storage.getEntries();
      var flatVector = new FlatKeyValueVector(maxKeyIdx * 2);
      int arrIdx = 0;
      while (entries.advance()) {
        if (entries.getValue().index() <= maxKeyIdx) {
          flatVector.keyValueArray[arrIdx++] = entries.getKey();
          flatVector.keyValueArray[arrIdx++] = entries.getValue().value();
        }
      }
      return flatVector;
    }

    static FlatKeyValueVector fromInteropVector(Object vector, InteropLibrary interop) {
      try {
        int arrSize = (int) interop.getArraySize(vector);
        assert arrSize % 2 == 0;
        var flatVector = new FlatKeyValueVector(arrSize);
        for (int i = 0; i < arrSize; i++) {
          flatVector.keyValueArray[i] = interop.readArrayElement(vector, i);
        }
        return flatVector;
      } catch (UnsupportedMessageException | InvalidArrayIndexException e) {
        throw new IllegalStateException(e);
      }
    }

    @ExportMessage
    boolean hasArrayElements() {
      return true;
    }

    @ExportMessage
    long getArraySize() {
      return keyValueArray.length;
    }

    @ExportMessage
    boolean isArrayElementReadable(long idx) {
      return idx < keyValueArray.length;
    }

    @ExportMessage
    boolean isArrayElementModifiable(long idx) {
      return false;
    }

    @ExportMessage
    boolean isArrayElementInsertable(long idx) {
      return false;
    }

    @ExportMessage
    void writeArrayElement(long index, Object value) throws UnsupportedMessageException {
      throw UnsupportedMessageException.create();
    }

    @ExportMessage
    Object readArrayElement(long idx) throws InvalidArrayIndexException {
      if (idx < keyValueArray.length) {
        return keyValueArray[(int) idx];
      } else {
        throw InvalidArrayIndexException.create(idx);
      }
    }
  }

  @ExportLibrary(InteropLibrary.class)
  static final class EmptyKeyValueVector implements TruffleObject {
    @ExportMessage
    boolean hasArrayElements() {
      return true;
    }

    @ExportMessage
    long getArraySize() {
      return 0;
    }

    @ExportMessage
    boolean isArrayElementReadable(long index) {
      return false;
    }

    @ExportMessage
    Object readArrayElement(long index) throws InvalidArrayIndexException {
      throw InvalidArrayIndexException.create(index);
    }
  }
}
