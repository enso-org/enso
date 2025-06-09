package org.enso.jvm.interop;

sealed interface OtherResult<R, E extends Exception> // either R or E
    permits OtherMessage.OtherValue, OtherMessage.OtherException {
  R value() throws E;
}
