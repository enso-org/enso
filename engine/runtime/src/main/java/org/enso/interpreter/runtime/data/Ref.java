package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

/** A mutable reference type. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
@Builtin(pkg = "mutable", stdlibName = "Standard.Base.Runtime.Ref.Ref")
public final class Ref implements EnsoObject {
  private volatile Object value;

  /**
   * Creates a new reference.
   *
   * @param value the initial value to store in the reference.
   */
  @Builtin.Method(description = "Creates a new Ref", autoRegister = false)
  public Ref(Object value) {
    this.value = value;
  }

  /**
   * @return the current value of the reference.
   */
  @Builtin.Method(name = "get", description = "Gets the value stored in the reference")
  @SuppressWarnings("generic-enso-builtin-type")
  public Object getValue() {
    return value;
  }

  /**
   * Stores a new value in the reference.
   *
   * @param value the value to store.
   * @returns the original value
   */
  @Builtin.Method(name = "put", description = "Stores a new value in the reference")
  @SuppressWarnings("generic-enso-builtin-type")
  public Object setValue(Object value) {
    Object old = this.value;
    this.value = value;
    return old;
  }

  @ExportMessage
  Type getMetaObject(@Bind("$node") Node node) {
    return EnsoContext.get(node).getBuiltins().ref();
  }

  @ExportMessage
  boolean hasMetaObject() {
    return true;
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@Bind("$node") Node node) {
    return EnsoContext.get(node).getBuiltins().ref();
  }
}
