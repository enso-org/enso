package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
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
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

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
    assert snapshotSize <= mapBuilder.getSize();
  }

  static EnsoHashMap createWithBuilder(EnsoHashMapBuilder mapBuilder, int snapshotSize) {
    return new EnsoHashMap(mapBuilder, snapshotSize);
  }

  static EnsoHashMap createEmpty(HashCodeAnyNode hashCodeAnyNode, EqualsAnyNode equalsNode) {
    return new EnsoHashMap(
        EnsoHashMapBuilder.create(hashCodeAnyNode, equalsNode),
        0
    );
  }

  EnsoHashMapBuilder getMapBuilder() {
    return mapBuilder;
  }

  Object getCachedVectorRepresentation() {
    assert cachedVectorRepresentation != null;
    return cachedVectorRepresentation;
  }

  boolean isVectorRepresentationCached() {
    return cachedVectorRepresentation != null;
  }

  @TruffleBoundary
  void cacheVectorRepresentation() {
    assert cachedVectorRepresentation == null : "Caching vector repr should be done at most once";
    Object[] keyValueArr = new Object[snapshotSize * 2];
    int arrIdx = 0;
    for (StorageEntry entry : mapBuilder.getStorage().getValues()) {
      if (entry.index() <= snapshotSize) {
        keyValueArr[arrIdx++] = entry.key();
        keyValueArr[arrIdx++] = entry.value();
      }
    }
    cachedVectorRepresentation =
        Vector.fromArray(new FlatKeyValueVector(keyValueArr));
  }

  @Builtin.Method
  @Builtin.Specialize
  public static EnsoHashMap empty(
      @Cached HashCodeAnyNode hashCodeNode,
      @Cached EqualsAnyNode equalsNode
  ) {
    return createEmpty(hashCodeNode, equalsNode);
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
    var mapBuilder = EnsoHashMapBuilder.create(hashCodeNode, equalsNode);
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
    // TODO: Cache flat vector?
    return hashMap;
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
    return isEntryInThisMap(
        mapBuilder.get(key)
    );
  }

  @ExportMessage
  boolean isHashEntryReadable(Object key) {
    return isHashEntryExisting(key);
  }

  @ExportMessage
  Object readHashValue(Object key) throws UnknownKeyException {
    StorageEntry entry = mapBuilder.get(key);
    if (isEntryInThisMap(entry)) {
      return entry.value();
    } else {
      throw UnknownKeyException.create(key);
    }
  }

  @ExportMessage
  Object getHashEntriesIterator(
      @CachedLibrary(limit = "3") InteropLibrary interop) {
    if (!isVectorRepresentationCached()) {
      cacheVectorRepresentation();
    }
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
    sb.append("EnsoHashMap{");
    boolean empty = true;
    for (StorageEntry entry : mapBuilder.getStorage().getValues()) {
      if (isEntryInThisMap(entry)) {
        empty = false;
        sb.append(entry.key()).append("=").append(entry.value()).append(", ");
      }
    }
    if (!empty) {
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

  private boolean isEntryInThisMap(StorageEntry entry) {
    return entry != null && entry.index() < snapshotSize;
  }
}
