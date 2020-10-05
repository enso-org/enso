package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

@ExportLibrary(InteropLibrary.class)
public class Resource implements TruffleObject {
  private final Object resource;

  public Resource(Object resource) {
    this.resource = resource;
  }

  public Object getResource() {
    return resource;
  }

  @ExportMessage
  Object toDisplayString(
      boolean allowSideEffects, @CachedLibrary(limit = "3") InteropLibrary resources) {
    return resources.toDisplayString(resource);
  }
}
