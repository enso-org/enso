package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.data.EnsoSourceSection;
import org.enso.interpreter.runtime.data.text.Text;

public class Warning implements TruffleObject {
  private final Object value;
  private final Object origin;
  private final ArrayRope<Reassignment> reassignments;

  public Warning(Object value, Object origin) {
    this(value, origin, new ArrayRope<>());
  }

  public Warning(Object value, Object origin, ArrayRope<Reassignment> reassignments) {
    this.value = value;
    this.origin = origin;
    this.reassignments = reassignments;
  }

  public static class Reassignment {
    private final long time;
    private final Text methodName;
    private final EnsoSourceSection location;

    public Reassignment(long time, Text methodName, EnsoSourceSection location) {
      this.time = time;
      this.methodName = methodName;
      this.location = location;
    }

    public long getTime() {
      return time;
    }

    public Text getMethodName() {
      return methodName;
    }

    public EnsoSourceSection getLocation() {
      return location;
    }
  }

  public Object getValue() {
    return value;
  }

  public Object getOrigin() {
    return origin;
  }

  public ArrayRope<Reassignment> getReassignments() {
    return reassignments;
  }

  public Warning reassign(Context context, Node location) {
    long time = context.clockTick();
    RootNode root = location.getRootNode();
    Text methodName = Text.create(root.getName());
    SourceSection section = location.getEncapsulatingSourceSection();
    EnsoSourceSection sourceLoc;
    if (section != null) {
      sourceLoc = new EnsoSourceSection(section);
    } else {
      sourceLoc = null;
    }
    Reassignment reassignment = new Reassignment(time, methodName, sourceLoc);
    return new Warning(value, origin, reassignments.prepend(reassignment));
  }
}
