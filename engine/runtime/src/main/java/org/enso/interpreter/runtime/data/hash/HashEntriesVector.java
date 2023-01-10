package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.runtime.data.Vector;

/**
 * A vector used to hold hash map entries, where each entry is represented as a 2-element vector.
 * Used for both Truffle interop, where {@code getHashEntriesIterator} expects this form of vector
 * (array), and for Enso {@code Map.to_vector} method. May be empty.
 */
@ExportLibrary(InteropLibrary.class)
final class HashEntriesVector implements TruffleObject {
  private final Vector[] entryPairs;

  private HashEntriesVector(Object[] keys, Object[] values) {
    assert keys.length == values.length;
    this.entryPairs = new Vector[keys.length];
    for (int i = 0; i < keys.length; i++) {
      entryPairs[i] = Vector.fromArray(new EntryPair(keys[i], values[i]));
    }
  }

  static HashEntriesVector createFromKeysAndValues(Object[] keys, Object[] values) {
    return new HashEntriesVector(keys, values);
  }

  static HashEntriesVector createEmpty() {
    return new HashEntriesVector(new Object[] {}, new Object[] {});
  }

  @ExportMessage
  boolean hasArrayElements() {
    return true;
  }

  @ExportMessage
  long getArraySize() {
    return entryPairs.length;
  }

  @ExportMessage
  boolean isArrayElementReadable(long idx) {
    return idx < entryPairs.length;
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
  Object readArrayElement(long idx) throws InvalidArrayIndexException {
    if (idx < entryPairs.length) {
      return entryPairs[(int) idx];
    } else {
      throw InvalidArrayIndexException.create(idx);
    }
  }

  @ExportMessage
  void writeArrayElement(long index, Object value) throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }

  @ExportLibrary(InteropLibrary.class)
  static final class EntryPair implements TruffleObject {
    private final Object key;
    private final Object value;

    EntryPair(Object key, Object value) {
      this.key = key;
      this.value = value;
    }

    @ExportMessage
    boolean hasArrayElements() {
      return true;
    }

    @ExportMessage
    long getArraySize() {
      return 2;
    }

    @ExportMessage
    boolean isArrayElementReadable(long idx) {
      return idx < 2;
    }

    @ExportMessage
    Object readArrayElement(long idx) throws InvalidArrayIndexException {
      if (idx == 0) {
        return key;
      } else if (idx == 1) {
        return value;
      } else {
        throw InvalidArrayIndexException.create(idx);
      }
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

    @TruffleBoundary
    @ExportMessage
    Object toDisplayString(boolean sideEffectsAllowed) {
      return "(" + key + ", " + value + ")";
    }
  }
}
