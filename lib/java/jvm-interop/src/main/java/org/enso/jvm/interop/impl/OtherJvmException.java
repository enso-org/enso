package org.enso.jvm.interop.impl;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;

@ExportLibrary(delegateTo = "delegate", value = InteropLibrary.class)
final class OtherJvmException extends AbstractTruffleException {
  final Object delegate;

  OtherJvmException(String message, TruffleObject delegate) {
    super(message);
    this.delegate = delegate == null ? "" : delegate;
  }
}
