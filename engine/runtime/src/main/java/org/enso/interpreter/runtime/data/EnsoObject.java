package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.EnsoLanguage;

/** All non-primitive Enso types extends from {@code EnsoObject}. */
@ExportLibrary(InteropLibrary.class)
public abstract class EnsoObject implements TruffleObject {
  @ExportMessage
  public boolean hasLanguage() {
    return true;
  }

  @ExportMessage
  public Class<? extends TruffleLanguage<?>> getLanguage() {
    return EnsoLanguage.class;
  }

  /**
   * This abstract method needs to be declared here with the annotation {@code @ExportMessage} so
   * that the Truffle DSL is satisfied.
   */
  @ExportMessage
  public abstract Object toDisplayString(boolean allowSideEffects);
}
