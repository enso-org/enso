package org.enso.jvm.interop;

sealed interface OtherResult<R, E extends Exception> // either R or E
    permits OtherMessage.ReturnValue, OtherMessage.ThrowException {
  R value() throws E;
}
