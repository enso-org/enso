package org.enso.interpreter.runtime.callable.atom;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

@ExportLibrary(StructsLibrary.class)
@ExportLibrary(TypesLibrary.class)
public class BoxingAtom extends Atom {
  private final Object[] fields;
  private final AtomConstructor constructor;

  public BoxingAtom(AtomConstructor constructor, Object... fields) {
    this.constructor = constructor;
    this.fields = fields;
  }

  @ExportMessage(name = "getFields")
  Object[] getFieldsX() {
    return fields;
  }

  @ExportMessage
  Object getField(int index) {
    return fields[index];
  }

  @ExportMessage
  AtomConstructor getConstructor() {
    return constructor;
  }
}
