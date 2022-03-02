package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.data.EnsoSourceSection;
import org.enso.interpreter.runtime.data.text.Text;

public class Warning implements TruffleObject {
  private final Object value;
  private final Object origin;
  private final ArrayRope<Reassignment> reassignments;
  private final long creationTime;

  public Warning(Object value, Object origin, long creationTime) {
    this(value, origin, creationTime, new ArrayRope<>());
  }

  public Warning(
      Object value, Object origin, long creationTime, ArrayRope<Reassignment> reassignments) {
    this.value = value;
    this.origin = origin;
    this.reassignments = reassignments;
    this.creationTime = creationTime;
  }

  public static class Reassignment {
    private final Text methodName;
    private final EnsoSourceSection location;

    public Reassignment(Text methodName, EnsoSourceSection location) {
      this.methodName = methodName;
      this.location = location;
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

  public Warning reassign(Node location) {
    RootNode root = location.getRootNode();
    Text methodName = Text.create(root.getName());
    SourceSection section = location.getEncapsulatingSourceSection();
    EnsoSourceSection sourceLoc;
    if (section != null) {
      sourceLoc = new EnsoSourceSection(section);
    } else {
      sourceLoc = null;
    }
    Reassignment reassignment = new Reassignment(methodName, sourceLoc);
    return new Warning(value, origin, creationTime, reassignments.prepend(reassignment));
  }
}
