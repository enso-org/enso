package org.enso.interpreter.runtime.execution;

import com.oracle.truffle.api.interop.TruffleObject;
import org.enso.polyglot.RuntimeID;

public final class RefObject extends Ref implements TruffleObject {
  private Object value;

  public RefObject(RuntimeID runtimeID) {
    super(runtimeID);
  }

  public Object get() {
    return value;
  }

  @Override
  public void reset() {
    value = null;
  }

  @Override
  public void update(Object value) {
    this.value = value;
  }

  @Override
  public boolean hasValue() {
    return value != null;
  }

  @Override
  public String toString() {
    return "Ref[runtimeID=" + getRuntimeID() + ", hasValue=" + (value != null) + "]";
  }
}
