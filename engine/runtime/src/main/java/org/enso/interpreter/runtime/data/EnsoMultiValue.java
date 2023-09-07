package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

@ExportLibrary(TypesLibrary.class)
public final class EnsoMultiValue implements EnsoObject {
  private final Type type;

  @CompilationFinal(dimensions = 1)
  private final Object[] values;

  private EnsoMultiValue(Type type, Object[] values) {
    this.type = type;
    this.values = values;
  }

  public static EnsoObject create(Type type, Object[] values) {
    return new EnsoMultiValue(type, values);
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  boolean hasSpecialDispatch() {
    return true;
  }

  @ExportMessage
  public final Type getType() {
    return type;
  }

  public final Object getValue(int index) {
    return values[index];
  }
}
