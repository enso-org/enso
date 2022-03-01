package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.interop.TruffleObject;
import org.enso.interpreter.runtime.data.ArrayRope;

public class Warning implements TruffleObject {
  private final Object value;
  private final Object origin;
  private final ArrayRope<Reassignment> reassignments;

  public Warning(Object value, Object origin) {
    this.value = value;
    this.origin = origin;
    this.reassignments = new ArrayRope<>();
  }

  public static class Reassignment {
    private final long time;
    private final Object location;

    public Reassignment(long time, Object location) {
      this.time = time;
      this.location = location;
    }
  }

  public Object getValue() {
    return value;
  }

  public Object getOrigin() {
    return origin;
  }
}
