package org.enso.interpreter.epb.runtime;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

@ExportLibrary(value = InteropLibrary.class, delegateTo = "delegate")
public class PolyglotExceptionProxy extends AbstractTruffleException {
  final PolyglotProxy delegate;
  final AbstractTruffleException original;

  public PolyglotExceptionProxy(
      AbstractTruffleException prototype,
      GuardedTruffleContext origin,
      GuardedTruffleContext target) {
    super(prototype);
    this.original = prototype;
    this.delegate = new PolyglotProxy(prototype, origin, target);
  }

  @ExportMessage
  boolean isException() {
    return true;
  }

  @ExportMessage
  RuntimeException throwException() {
    throw this;
  }

  public AbstractTruffleException getOriginal() {
    return original;
  }

  public GuardedTruffleContext getOrigin() {
    return delegate.getOrigin();
  }

  public GuardedTruffleContext getTarget() {
    return delegate.getTarget();
  }
}
