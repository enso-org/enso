package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.dsl.Suspend;
import org.enso.interpreter.runtime.builtin.BuiltinObject;
import org.enso.interpreter.runtime.callable.function.Function;

/** A mutable reference type. */
@ExportLibrary(InteropLibrary.class)
@Builtin(pkg = "mutable", stdlibName = "Standard.Base.Runtime.Ref.Ref")
public final class Ref extends BuiltinObject {
  private volatile Object value;

  private Ref(Object v) {
    this.value = v;
  }

  @Override
  protected String builtinName() {
    return "Ref";
  }

  /**
   * Creates a new reference.
   *
   * @param value the initial value to store in the reference.
   */
  @Builtin.Method(description = "Creates a new Ref", autoRegister = false)
  public static Ref alloc(@Suspend Object value) {
    return new Ref(value);
  }

  final boolean needsEval() {
    return value instanceof Function fn && fn.isFullyApplied();
  }

  /**
   * Stores a new value in the reference.
   *
   * @param value the value to store.
   * @returns the original value
   */
  @Builtin.Method(name = "put", description = "Stores a new value in the reference")
  @SuppressWarnings("generic-enso-builtin-type")
  public static Object putValue(Ref ref, Object value) {
    Object old = ref.value;
    ref.value = value;
    return old;
  }

  @ExportMessage
  Object toDisplayString(
      boolean allowSideEffects, @CachedLibrary(limit = "3") InteropLibrary interop) {
    return interop.toDisplayString(value, allowSideEffects);
  }

  @TruffleBoundary
  @Override
  @ExportMessage.Ignore
  public Object toDisplayString(boolean allowSideEffects) {
    return toDisplayString(allowSideEffects, InteropLibrary.getUncached());
  }

  final Object value() {
    return value;
  }
}
