package org.enso.interpreter.epb.runtime;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

/**
 * A wrapper for exceptions that cross the polyglot boundary.
 *
 * <p>It is responsible for proxying messages across the polyglot boundary specifically for the case
 * of exceptions. Without this, exceptions (as non-linear control flow) would bubble up into Enso
 * without their context. This would allow them to be caught, but Enso would have no means of
 * executing code on the guest-language exception. Instead, we wrap the exception into this proxy,
 * which holds onto the foreign exception, as well as the two contexts necessary for mediating calls
 * between Enso and the foreign language.
 *
 * <p>This is _separate_ to the {@link PolyglotProxy} as we did not want to make that proxy into an
 * {@link AbstractTruffleException}. This means that we have more control over when foreign objects
 * are represented as exceptions.
 */
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

  public PolyglotProxy getDelegate() {
    return delegate;
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
