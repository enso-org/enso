package org.enso.interpreter.runtime.data.hash;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

@ExportLibrary(InteropLibrary.class)
final class FlatKeyValueVector implements TruffleObject {
  private final Object[] keyValueArray;

  /**
   * @param keyValueArray May be empty. Not null.
   */
  FlatKeyValueVector(Object[] keyValueArray) {
    assert keyValueArray.length % 2 == 0;
    this.keyValueArray = keyValueArray;
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
  Object readArrayElement(long idx) throws InvalidArrayIndexException {
    if (idx < keyValueArray.length) {
      return keyValueArray[(int) idx];
    } else {
      throw InvalidArrayIndexException.create(idx);
    }
  }

  @ExportMessage
  void writeArrayElement(long index, Object value) throws UnsupportedMessageException {
    throw UnsupportedMessageException.create();
  }
}
