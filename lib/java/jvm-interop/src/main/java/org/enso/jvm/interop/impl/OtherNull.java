package org.enso.jvm.interop.impl;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

@ExportLibrary(InteropLibrary.class)
final class OtherNull implements TruffleObject {
  static final OtherNull NULL = new OtherNull();

  private OtherNull() {}

  @ExportMessage
  boolean isNull() {
    return true;
  }
}
