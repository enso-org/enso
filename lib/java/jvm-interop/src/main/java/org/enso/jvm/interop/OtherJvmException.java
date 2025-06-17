package org.enso.jvm.interop;

import com.oracle.truffle.api.exception.AbstractTruffleException;

final class OtherJvmException extends AbstractTruffleException {
  OtherJvmException(String message) {
    super(message);
  }
}
