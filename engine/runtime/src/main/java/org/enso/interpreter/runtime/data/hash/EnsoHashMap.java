package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownKeyException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.meta.EqualsAnyNode;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeAnyNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.data.hash.EnsoHashMapBuilder.StorageEntry;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

/**
 * Implementation of a hash map structure, capable of holding any types of keys and values. The
 * actual hash map storage is implemented in {@link EnsoHashMapBuilder}, and every {@link
 * EnsoHashMap} has just a reference to the builder and its size, which allows us to implement
 * {@code insert} operation in constant time. In other words, every map is just a snapshot of its
 * builder.
 *
 * <p>Users should not use Enso objects as keys to Java maps, because equals won't work the same way
 * as it works in Enso.
 */
@ExportLibrary(TypesLibrary.class)
@ExportLibrary(InteropLibrary.class)
@Builtin(stdlibName = "Standard.Base.Data.Map.Map", name = "Map")
public final class EnsoHashMap implements TruffleObject {
  private final EnsoHashMapBuilder mapBuilder;
  /**
   * Size of this Map. Basically an index into {@link EnsoHashMapBuilder}'s storage. See {@link
   * #isEntryInThisMap(StorageEntry)}.
   */
  private final int snapshotSize;
  /**
   * True iff {@code insert} method was already called. If insert was already called, and we are
   * calling {@code insert} again, the {@link #mapBuilder} should be duplicated for the newly
   * created Map.
   */
  private boolean insertCalled;

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
    return new EnsoHashMap(EnsoHashMapBuilder.create(hashCodeAnyNode, equalsNode), 0);
  }

  EnsoHashMapBuilder getMapBuilder() {
    return mapBuilder;
  }

  Object getCachedVectorRepresentation() {
    return getCachedVectorRepresentation(ConditionProfile.getUncached());
  }

  Object getCachedVectorRepresentation(ConditionProfile isNotCachedProfile) {
    if (isNotCachedProfile.profile(cachedVectorRepresentation == null)) {
      Object[] keys = new Object[snapshotSize];
      Object[] values = new Object[snapshotSize];
      int arrIdx = 0;
      for (StorageEntry entry : mapBuilder.getStorage().getValues()) {
        if (entry.index() < snapshotSize) {
          keys[arrIdx] = entry.key();
          values[arrIdx] = entry.value();
          arrIdx++;
        }
      }
      cachedVectorRepresentation =
          Vector.fromArray(HashEntriesVector.createFromKeysAndValues(keys, values));
    }
    return cachedVectorRepresentation;
  }

  public boolean isInsertCalled() {
    return insertCalled;
  }

  public void setInsertCalled() {
    assert !insertCalled : "setInsertCalled should be called at most once";
    insertCalled = true;
  }

  @Builtin.Method
  @Builtin.Specialize
  public static EnsoHashMap empty(
      @Cached HashCodeAnyNode hashCodeNode, @Cached EqualsAnyNode equalsNode) {
    return createEmpty(hashCodeNode, equalsNode);
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
    return isEntryInThisMap(mapBuilder.get(key));
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
  Object getHashEntriesIterator(@CachedLibrary(limit = "3") InteropLibrary interop) {
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
    return EnsoContext.get(thisLib).getBuiltins().map();
  }

  @ExportMessage
  boolean hasMetaObject() {
    return true;
  }

  @ExportMessage
  Type getMetaObject(@CachedLibrary("this") InteropLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().map();
  }

  @ExportMessage
  @TruffleBoundary
  Object toDisplayString(boolean allowSideEffects) {
    var sb = new StringBuilder();
    sb.append("{");
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
