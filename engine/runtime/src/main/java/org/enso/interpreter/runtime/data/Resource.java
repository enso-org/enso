package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;

import java.lang.ref.PhantomReference;

@ExportLibrary(InteropLibrary.class)
public class Resource implements TruffleObject {
  private final Object resource;
  private PhantomReference<Resource> phantomReference;

  public Resource(Object resource) {
    this.resource = resource;
    this.phantomReference = null;
  }

  public Object getResource() {
    return resource;
  }

  public PhantomReference<Resource> getPhantomReference() {
    return phantomReference;
  }

  public void setPhantomReference(PhantomReference<Resource> phantomReference) {
    this.phantomReference = phantomReference;
  }

  @ExportMessage
  Object toDisplayString(
      boolean allowSideEffects, @CachedLibrary(limit = "3") InteropLibrary resources) {
    return resources.toDisplayString(resource);
  }
}
