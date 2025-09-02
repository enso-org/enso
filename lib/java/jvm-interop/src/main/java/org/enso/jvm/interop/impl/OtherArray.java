package org.enso.jvm.interop.impl;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

@ExportLibrary(InteropLibrary.class)
final class OtherArray implements TruffleObject {
  private final TruffleObject[] arr;

  OtherArray(TruffleObject... arr) {
    this.arr = arr;
  }

  @ExportMessage
  boolean hasArrayElements() {
    return true;
  }

  @ExportMessage
  long getArraySize() {
    return arr.length;
  }

  @ExportMessage
  boolean isArrayElementReadable(long index) {
    return index >= 0 && index < arr.length;
  }

  @ExportMessage
  TruffleObject readArrayElement(long index) throws InvalidArrayIndexException {
    if (!isArrayElementReadable(index)) {
      throw InvalidArrayIndexException.create(index);
    }
    return arr[Math.toIntExact(index)];
  }
}
