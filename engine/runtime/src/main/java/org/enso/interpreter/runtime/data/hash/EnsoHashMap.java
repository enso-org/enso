package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownKeyException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.meta.EqualsAnyNode;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeAnyNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.data.hash.EnsoHashMapBuilder.StorageEntry;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.graalvm.collections.EconomicMap;

/**
 * Effectively a snapshot of {@link EnsoHashMapBuilder}.
 * Caches materialized vector in case {@code to_vector} method is called.
 */
@ExportLibrary(TypesLibrary.class)
@ExportLibrary(InteropLibrary.class)
@Builtin(stdlibName = "Standard.Base.Data.Hash_Map.Hash_Map", name = "Hash_Map")
public final class EnsoHashMap implements TruffleObject {
  private static final TruffleLogger logger = EnsoHashMapBuilder.logger;
  private final EnsoHashMapBuilder mapBuilder;
  private final int snapshotSize;
  private Object cachedVectorRepresentation;

  private EnsoHashMap(EnsoHashMapBuilder mapBuilder, int snapshotSize) {
    this.mapBuilder = mapBuilder;
    this.snapshotSize = snapshotSize;
    assert snapshotSize <= mapBuilder.getSize();
  }

  static EnsoHashMap createWithBuilder(EnsoHashMapBuilder mapBuilder, int snapshotSize) {
    return new EnsoHashMap(mapBuilder, snapshotSize);
  }

  static EnsoHashMap createEmpty() {
    return new EnsoHashMap(
        EnsoHashMapBuilder.create(),
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
      @CachedLibrary("flatVector") InteropLibrary interop,
      @Cached HashCodeAnyNode hashCodeNode,
      @Cached EqualsAnyNode equalsNode) {
    if (!interop.hasArrayElements(flatVector)) {
      throw new PanicException(
          "Value not a vector: " + interop.toDisplayString(flatVector),
          interop
      );
    }
    var mapBuilder = EnsoHashMapBuilder.create();
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
        mapBuilder.add(key, value, hashCodeNode, equalsNode);
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
  @Builtin.Specialize
  public EnsoHashMap insert(Object key, Object value,
      @Cached HashCodeAnyNode hashCodeNode,
      @Cached EqualsAnyNode equalsNode) {
    mapBuilder.add(key, value, hashCodeNode, equalsNode);
    return mapBuilder.build();
  }

  @Builtin.Method
  public long size() {
    return snapshotSize;
  }

  @Builtin.Method(name = "get_builtin")
  @Builtin.Specialize
  public Object get(Object key,
      @Cached HashCodeAnyNode hashCodeNode,
      @Cached EqualsAnyNode equalsNode) {
    StorageEntry entry = mapBuilder.get(key, hashCodeNode, equalsNode);
    if (isEntryInThisMap(entry)) {
      return entry.value();
    } else {
      throw DataflowError.withoutTrace("No such key '" + key.toString() + "'", hashCodeNode);
    }
  }

  @Builtin.Method(name = "contains_key")
  @Builtin.Specialize
  public boolean containsKey(Object key,
      @Cached HashCodeAnyNode hashCodeNode,
      @Cached EqualsAnyNode equalsNode) {
    return isEntryInThisMap(
        mapBuilder.get(key, hashCodeNode, equalsNode)
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
  boolean isHashEntryExisting(Object key,
      @Cached @Shared("hashCodeNode") HashCodeAnyNode hashCodeNode,
      @Cached @Shared("equalsNode") EqualsAnyNode equalsNode) {
    return containsKey(key, hashCodeNode, equalsNode);
  }

  @ExportMessage
  boolean isHashEntryReadable(Object key,
      @Cached @Shared("hashCodeNode")HashCodeAnyNode hashCodeNode,
      @Cached @Shared("equalsNode") EqualsAnyNode equalsNode) {
    return isHashEntryExisting(key, hashCodeNode, equalsNode);
  }

  @ExportMessage
  Object readHashValue(Object key,
      @Cached @Shared("hashCodeNode") HashCodeAnyNode hashCodeNode,
      @Cached @Shared("equalsNode") EqualsAnyNode equalsNode) throws UnknownKeyException {
    StorageEntry entry = mapBuilder.get(key, hashCodeNode, equalsNode);
    if (isEntryInThisMap(entry)) {
      return entry.value();
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
  @TruffleBoundary
  Object toDisplayString(boolean allowSideEffects) {
    var sb = new StringBuilder();
    sb.append("{");
    for (StorageEntry entry : mapBuilder.getStorage().getValues()) {
      if (isEntryInThisMap(entry)) {
        sb.append(entry.key()).append("=").append(entry.value()).append(", ");
      }
    }
    if (sb.length() > 1) {
      // Delete last comma
      sb.delete(sb.length() - 2, sb.length());
    }
    sb.append("}");
    return sb.toString();
  }

  @Override
  public String toString() {
    return (String) toDisplayString(true);
  }

  private Object getCachedVectorRepresentation() {
    if (cachedVectorRepresentation == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      logger.fine(() -> "Caching vector representation for " + toDisplayString(true));
      if (size() == 0) {
        cachedVectorRepresentation = Vector.fromArray(new EmptyKeyValueVector());
      } else {
        cachedVectorRepresentation =
            Vector.fromArray(FlatKeyValueVector.fromBuilderStorage(mapBuilder.getStorage(), snapshotSize));
      }
    }
    return cachedVectorRepresentation;
  }

  private boolean isEntryInThisMap(StorageEntry entry) {
    return entry != null && entry.index() <= snapshotSize;
  }

  @ExportLibrary(InteropLibrary.class)
  static final class FlatKeyValueVector implements TruffleObject {
    private final Object[] keyValueArray;

    private FlatKeyValueVector(int arrSize) {
      this.keyValueArray = new Object[arrSize];
    }

    static FlatKeyValueVector fromBuilderStorage(EconomicMap<Integer, StorageEntry> storage, int maxKeyIdx) {
      assert storage.size() > 0;
      var flatVector = new FlatKeyValueVector(maxKeyIdx * 2);
      int arrIdx = 0;
      for (StorageEntry entry : storage.getValues()) {
        if (entry.index() <= maxKeyIdx) {
          flatVector.keyValueArray[arrIdx++] = entry.key();
          flatVector.keyValueArray[arrIdx++] = entry.value();
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
