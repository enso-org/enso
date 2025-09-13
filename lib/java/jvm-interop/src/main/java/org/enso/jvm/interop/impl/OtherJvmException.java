package org.enso.jvm.interop.impl;

import com.oracle.truffle.api.exception.AbstractTruffleException;

final class OtherJvmException extends AbstractTruffleException {
  OtherJvmException(String message) {
    super(message);
  }

  OtherJvmException(Throwable toWrap) {
    super(toWrap.getMessage(), toWrap, UNLIMITED_STACK_TRACE, null);
  }
}
