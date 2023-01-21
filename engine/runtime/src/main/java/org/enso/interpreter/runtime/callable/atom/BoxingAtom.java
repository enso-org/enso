package org.enso.interpreter.runtime.callable.atom;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

/**
 * A version of {@link org.enso.interpreter.runtime.callable.atom.Atom} that stores its fields in an
 * array of objects. This will be slow most of the time, and is the fallback version of {@link
 * org.enso.interpreter.runtime.callable.atom.Atom}. For a better optimized version, see {@link
 * org.enso.interpreter.runtime.callable.atom.unboxing.UnboxingAtom}.
 */
@ExportLibrary(StructsLibrary.class)
public class BoxingAtom extends Atom {
  private final Object[] fields;

  public BoxingAtom(AtomConstructor constructor, Object... fields) {
    super(constructor);
    this.fields = fields;
  }

  @ExportMessage
  Object[] getFields() {
    return fields;
  }

  @ExportMessage
  Object getField(int index) {
    return fields[index];
  }

  @ExportMessage
  void setField(int index, Object value) {
    fields[index] = value;
  }
}
