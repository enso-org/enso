package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownKeyException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.node.expression.builtin.meta.EqualsNode;
import org.enso.interpreter.node.expression.builtin.meta.HashCodeNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.hash.EnsoHashMapBuilder.StorageEntry;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.error.PanicException;
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
public final class EnsoHashMap implements EnsoObject {
  private final EnsoHashMapBuilder mapBuilder;
  private final int generation;
  private final int size;

  private Object cachedVectorRepresentation;

  private EnsoHashMap(EnsoHashMapBuilder mapBuilder) {
    this.mapBuilder = mapBuilder;
    this.generation = mapBuilder.generation();
    this.size = mapBuilder.size();
  }

  static EnsoHashMap createWithBuilder(EnsoHashMapBuilder mapBuilder) {
    return new EnsoHashMap(mapBuilder);
  }

  static EnsoHashMap createEmpty() {
    return new EnsoHashMap(EnsoHashMapBuilder.create());
  }

  EnsoHashMapBuilder getMapBuilder(
      boolean readOnly, HashCodeNode hashCodeNode, EqualsNode equalsNode) {
    if (readOnly) {
      return mapBuilder;
    } else {
      return mapBuilder.asModifiable(generation, hashCodeNode, equalsNode);
    }
  }

  Object getCachedVectorRepresentation() {
    return getCachedVectorRepresentation(ConditionProfile.getUncached());
  }

  Object getCachedVectorRepresentation(ConditionProfile isNotCachedProfile) {
    if (isNotCachedProfile.profile(cachedVectorRepresentation == null)) {
      var keys = new Object[size];
      var values = new Object[size];
      var at = 0;
      for (var entry : mapBuilder.getEntries(generation, size)) {
        keys[at] = entry.key();
        values[at] = entry.value();
        at++;
      }
      var pairs = HashEntriesVector.createFromKeysAndValues(keys, values);
      cachedVectorRepresentation = ArrayLikeHelpers.asVectorFromArray(pairs);
    }
    return cachedVectorRepresentation;
  }

  @Builtin.Method
  @Builtin.Specialize
  public static EnsoHashMap empty() {
    return createEmpty();
  }

  @ExportMessage
  boolean hasHashEntries() {
    return true;
  }

  @ExportMessage
  int getHashSize() {
    return size;
  }

  @ExportMessage
  boolean isHashEntryExisting(
      Object key,
      @Shared("hash") @Cached HashCodeNode hashCodeNode,
      @Shared("equals") @Cached EqualsNode equalsNode) {
    var entry = mapBuilder.get(key, generation, hashCodeNode, equalsNode);
    return entry != null;
  }

  @ExportMessage
  boolean isHashEntryReadable(
      Object key,
      @Shared("hash") @Cached HashCodeNode hashCodeNode,
      @Shared("equals") @Cached EqualsNode equalsNode) {
    return isHashEntryExisting(key, hashCodeNode, equalsNode);
  }

  @ExportMessage
  Object readHashValue(
      Object key,
      @Shared("hash") @Cached HashCodeNode hashCodeNode,
      @Shared("equals") @Cached EqualsNode equalsNode)
      throws UnknownKeyException {
    StorageEntry entry = mapBuilder.get(key, generation, hashCodeNode, equalsNode);
    if (entry != null) {
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
      throw new PanicException(Text.create(e.getMessage()), interop);
    }
  }

  @ExportMessage(library = TypesLibrary.class)
  boolean hasType() {
    return true;
  }

  @ExportMessage(library = TypesLibrary.class)
  Type getType(@CachedLibrary("this") TypesLibrary thisLib, @Cached("1") int ignore) {
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
    return toString(true);
  }

  @Override
  public String toString() {
    // We are not using uncached InteropLibrary in this method, as it may substantially
    // slow down Java debugger.
    return toString(false);
  }

  private String toString(boolean useInterop) {
    var sb = new StringBuilder();
    sb.append("{");
    boolean empty = true;
    for (StorageEntry entry : mapBuilder.getEntries(generation, size)) {
      empty = false;
      sb.append(entryToString(entry, useInterop)).append(", ");
    }
    if (!empty) {
      // Delete last comma
      sb.delete(sb.length() - 2, sb.length());
    }
    sb.append("}");
    return sb.toString();
  }

  private static String entryToString(StorageEntry entry, boolean useInterop) {
    String keyStr;
    String valStr;
    if (useInterop) {
      var interop = InteropLibrary.getUncached();
      try {
        keyStr = interop.asString(interop.toDisplayString(entry.key()));
        valStr = interop.asString(interop.toDisplayString(entry.value()));
      } catch (UnsupportedMessageException e) {
        throw new PanicException(Text.create(e.getMessage()), interop);
      }
    } else {
      keyStr = entry.key().toString();
      valStr = entry.value().toString();
    }
    return keyStr + "=" + valStr;
  }
}
