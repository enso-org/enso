package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

/** A mutable reference type. */
@ExportLibrary(TypesLibrary.class)
@Builtin(pkg = "mutable", stdlibName = "Standard.Base.Runtime.Ref.Ref")
public class Ref implements TruffleObject {
  private volatile Object value;

  /**
   * Creates a new reference.
   *
   * @param value the initial value to store in the reference.
   */
  @Builtin.Method(description = "Creates a new Ref")
  public Ref(Object value) {
    this.value = value;
  }

  /** @return the current value of the reference. */
  @Builtin.Method(name = "get", description = "Gets the value stored in the reference")
  public Object getValue() {
    return value;
  }

  /**
   * Stores a new value in the reference.
   *
   * @param value the value to store.
   */
  @Builtin.Method(name = "put", description = "Stores a new value in the reference")
  public Object setValue(Object value) {
    Object old = this.value;
    this.value = value;
    return old;
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return Context.get(thisLib).getBuiltins().ref();
  }
}
