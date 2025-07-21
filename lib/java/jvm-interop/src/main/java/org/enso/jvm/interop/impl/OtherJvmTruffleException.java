package org.enso.jvm.interop.impl;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;

@ExportLibrary(delegateTo = "delegate", value = InteropLibrary.class)
final class OtherJvmTruffleException extends AbstractTruffleException {
  final OtherJvmObject delegate;

  OtherJvmTruffleException(String message, OtherJvmObject delegate) {
    super(message);
    assert delegate != null && InteropLibrary.getUncached().isException(delegate);
    this.delegate = delegate;
  }
}
