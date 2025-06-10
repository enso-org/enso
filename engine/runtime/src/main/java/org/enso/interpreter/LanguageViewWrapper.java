package org.enso.interpreter;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.runtime.data.EnsoObject;

/**
 * Just a wrapper for a value providing {@link
 * com.oracle.truffle.api.interop.InteropLibrary#hasLanguage(Object)} message implementation.
 */
@ExportLibrary(value = InteropLibrary.class, delegateTo = "delegate")
final class LanguageViewWrapper extends EnsoObject {
  final Object delegate;

  LanguageViewWrapper(Object delegate) {
    this.delegate = delegate;
  }

  @ExportMessage
  Object toDisplayString(
      boolean allowSideEffects, @CachedLibrary("this.delegate") InteropLibrary interop) {
    return interop.toDisplayString(delegate, allowSideEffects);
  }

  @Override
  @TruffleBoundary
  @ExportMessage.Ignore
  public Object toDisplayString(boolean allowSideEffects) {
    return toDisplayString(allowSideEffects, InteropLibrary.getUncached());
  }
}
