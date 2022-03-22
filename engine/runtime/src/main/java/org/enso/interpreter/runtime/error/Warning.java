package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.data.EnsoSourceSection;

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

  @ExportLibrary(InteropLibrary.class)
  public static class Reassignment implements TruffleObject {
    private final String methodName;
    private final SourceSection location;

    public Reassignment(String methodName, SourceSection location) {
      this.methodName = methodName;
      this.location = location;
    }

    @ExportMessage
    boolean hasExecutableName() {
      return true;
    }

    @ExportMessage
    String getExecutableName() {
      return methodName;
    }

    @ExportMessage
    boolean hasSourceLocation() {
      return location != null;
    }

    @ExportMessage
    SourceSection getSourceLocation() {
      return location;
    }
  }

  public Object getValue() {
    return value;
  }

  public Object getOrigin() {
    return origin;
  }

  public long getCreationTime() {
    return creationTime;
  }

  public ArrayRope<Reassignment> getReassignments() {
    return reassignments;
  }

  public Warning reassign(Node location) {
    RootNode root = location.getRootNode();
    SourceSection section = location.getEncapsulatingSourceSection();
    Reassignment reassignment = new Reassignment(root.getName(), section);
    return new Warning(value, origin, creationTime, reassignments.prepend(reassignment));
  }
}
