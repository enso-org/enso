package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

import java.util.Arrays;
import java.util.Comparator;

@Builtin(pkg = "error", stdlibName = "Standard.Base.Warning.Warning")
@ExportLibrary(TypesLibrary.class)
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

  @Builtin.Method(name = "value", description = "Gets the payload of the warning.")
  public Object getValue() {
    return value;
  }

  @Builtin.Method(name = "origin", description = "Gets the payload of the warning.")
  public Object getOrigin() {
    return origin;
  }

  @Builtin.Method(
      name = "create",
      description = "Creates a new instance of the primitive warning value.")
  @Builtin.Specialize
  public static Warning create(Context ctx, Object payload, Object origin) {
    return new Warning(payload, origin, ctx.clockTick());
  }

  @Builtin.Method(description = "Gets the list of locations where the warnings was reassigned.")
  public Array getReassignments() {
    Warning.Reassignment[] reassignmentsArray = reassignments.toArray(Warning.Reassignment[]::new);
    return new Array(Arrays.copyOf(reassignmentsArray, reassignmentsArray.length, Object[].class));
  }

  @Builtin.Method(
      name = "attach_with_stacktrace",
      description = "Attaches the given warning to the value.")
  @Builtin.Specialize
  public static WithWarnings attach(
      Context ctx, WithWarnings value, Object warning, Object origin) {
    return value.prepend(new Warning(warning, origin, ctx.clockTick()));
  }

  @Builtin.Method(
      name = "attach_with_stacktrace",
      description = "Attaches the given warning to the value.")
  @Builtin.Specialize(fallback = true)
  public static WithWarnings attach(Context ctx, Object value, Object warning, Object origin) {
    return new WithWarnings(value, new Warning(warning, origin, ctx.clockTick()));
  }

  @Builtin.Method(
      name = "get_all_array",
      description = "Gets all the warnings associated with the value.")
  @Builtin.Specialize
  @CompilerDirectives.TruffleBoundary
  public static Array getAll(WithWarnings value) {
    Warning[] warnings = value.getWarningsArray();
    Arrays.sort(warnings, Comparator.comparing(Warning::getCreationTime).reversed());
    Object[] result = new Object[warnings.length];
    System.arraycopy(warnings, 0, result, 0, warnings.length);
    return new Array(result);
  }

  @Builtin.Method(
      name = "get_all_array",
      description = "Gets all the warnings associated with the value.")
  @Builtin.Specialize(fallback = true)
  public static Array getAll(Object value) {
    return new Array();
  }

  @Builtin.Method(
      name = "set_array",
      description = "Gets all the warnings associated with the value.")
  @Builtin.Specialize
  public static Object set(WithWarnings value, Object warnings, InteropLibrary interop) {
    return setGeneric(value.getValue(), interop, warnings);
  }

  @Builtin.Method(
      name = "set_array",
      description = "Gets all the warnings associated with the value.")
  @Builtin.Specialize(fallback = true)
  public static Object set(Object value, Object warnings, InteropLibrary interop) {
    return setGeneric(value, interop, warnings);
  }

  private static Object setGeneric(Object value, InteropLibrary interop, Object warnings) {
    try {
      var size = interop.getArraySize(warnings);
      if (size == 0) {
        return value;
      }
      Warning[] warningsCast = new Warning[(int) size];
      for (int i = 0; i < warningsCast.length; i++) {
        warningsCast[i] = (Warning) interop.readArrayElement(warnings, i);
      }
      return new WithWarnings(value, warningsCast);
    } catch (UnsupportedMessageException | InvalidArrayIndexException ex) {
      CompilerDirectives.transferToInterpreter();
      throw new IllegalStateException(ex);
    }
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
    SourceSection getSourceLocation() throws UnsupportedMessageException {
      if (location == null) {
        throw UnsupportedMessageException.create();
      }
      return location;
    }
  }

  public long getCreationTime() {
    return creationTime;
  }

  @CompilerDirectives.TruffleBoundary
  public Warning reassign(Node location) {
    RootNode root = location.getRootNode();
    SourceSection section = location.getEncapsulatingSourceSection();
    Reassignment reassignment = new Reassignment(root.getName(), section);
    return new Warning(value, origin, creationTime, reassignments.prepend(reassignment));
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return Context.get(thisLib).getBuiltins().warning();
  }
}
