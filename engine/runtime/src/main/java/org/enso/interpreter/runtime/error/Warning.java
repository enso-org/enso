package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import java.math.BigInteger;
import java.util.Arrays;
import java.util.Comparator;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.ArrayRope;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.vector.ArrayLikeHelpers;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.number.EnsoBigInteger;
import org.graalvm.collections.EconomicSet;

@Builtin(pkg = "error", stdlibName = "Standard.Base.Warning.Warning")
@ExportLibrary(TypesLibrary.class)
public final class Warning implements EnsoObject {
  private final Object value;
  private final Object origin;
  private final ArrayRope<Reassignment> reassignments;
  private final long sequenceId;

  private Warning(Object value, Object origin, long sequenceId) {
    this(value, origin, sequenceId, new ArrayRope<>());
  }

  private Warning(
      Object value, Object origin, long sequenceId, ArrayRope<Reassignment> reassignments) {
    this.value = value;
    this.origin = origin;
    this.reassignments = reassignments;
    this.sequenceId = sequenceId;
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
      description = "Creates a new instance of the primitive warning value.",
      autoRegister = false)
  @Builtin.Specialize
  public static Warning create(EnsoContext ctx, Object payload, Object origin) {
    return new Warning(payload, origin, ctx.nextSequenceId());
  }

  @Builtin.Method(description = "Gets the list of locations where the warnings was reassigned.")
  public EnsoObject getReassignments() {
    Warning.Reassignment[] reassignmentsArray = reassignments.toArray(Warning.Reassignment[]::new);
    return ArrayLikeHelpers.wrapEnsoObjects(reassignmentsArray);
  }

  @Builtin.Method(
      name = "attach_with_stacktrace",
      description = "Attaches the given warning to the value.",
      autoRegister = false)
  @Builtin.Specialize
  public static WithWarnings attach(
      EnsoContext ctx, WithWarnings value, Object warning, Object origin) {
    return value.append(ctx, new Warning(warning, origin, ctx.nextSequenceId()));
  }

  @Builtin.Method(
      name = "attach_with_stacktrace",
      description = "Attaches the given warning to the value.",
      autoRegister = false)
  @Builtin.Specialize(fallback = true)
  public static WithWarnings attach(EnsoContext ctx, Object value, Object warning, Object origin) {
    return WithWarnings.wrap(ctx, value, new Warning(warning, origin, ctx.nextSequenceId()));
  }

  @Builtin.Method(
      name = "get_all_array",
      description = "Gets all the warnings associated with the value.",
      autoRegister = false)
  @Builtin.Specialize
  @CompilerDirectives.TruffleBoundary
  public static EnsoObject getAll(WithWarnings value, WarningsLibrary warningsLib, InteropLibrary interop) {
    //System.out.println("AAA getAll ww " + value.getClass() + " " + value.getValue().getClass());
    Warning[] warnings = value.getWarningsArray(warningsLib);
    sortArray(warnings);
    return ArrayLikeHelpers.wrapEnsoObjects(warnings);
  }

  @Builtin.Method(
      name = "get_all_array",
      description = "Gets all the warnings associated with the value.",
      autoRegister = false)
  @Builtin.Specialize(fallback = true)
  public static EnsoObject getAll(Object value, WarningsLibrary warningsLib, InteropLibrary interop) {
    //System.out.println("AAA getAll obj " + value.getClass() + " " + (value instanceof org.enso.interpreter.runtime.data.vector.Vector));
    if (warningsLib.hasWarnings(value)) {
      try {
        Warning[] warnings = warningsLib.getWarnings(value, null);
        sortArray(warnings);
        return ArrayLikeHelpers.wrapEnsoObjects(warnings);
      } catch (UnsupportedMessageException e) {
        throw EnsoContext.get(warningsLib).raiseAssertionPanic(warningsLib, null, e);
      }
    } else {
      return ArrayLikeHelpers.empty();
    }
  }

  @Builtin.Method(
      name = "get_all_from_vector_elements",
      description = "Gets all the warnings associated with the value.",
      autoRegister = false)
  @Builtin.Specialize
  @CompilerDirectives.TruffleBoundary
  public static EnsoObject getAllFromVectorElements(WithWarnings value, WarningsLibrary warningsLib, InteropLibrary interop) {
    if (value.getValue() instanceof org.enso.interpreter.runtime.data.vector.Vector) {
      Warning[] warnings = getAllElementWarnings(value.getValue(), warningsLib, interop);
      return ArrayLikeHelpers.wrapEnsoObjects(warnings);
    } else {
      throw EnsoContext.get(warningsLib).raiseAssertionPanic(warningsLib, null, null);
    }
  }

  @Builtin.Method(
      name = "get_all_from_vector_elements",
      description = "Gets all the warnings associated with the value.",
      autoRegister = false)
  @Builtin.Specialize(fallback = true)
  public static EnsoObject getAllFromVectorElements(Object value, WarningsLibrary warningsLib, InteropLibrary interop) {
    //System.out.println("AAA getAll obj " + value.getClass() + " " + (value instanceof org.enso.interpreter.runtime.data.vector.Vector));
    if (value instanceof org.enso.interpreter.runtime.data.vector.Vector) {
      Warning[] warnings = getAllElementWarnings(value, warningsLib, interop);
      return ArrayLikeHelpers.wrapEnsoObjects(warnings);
    } else {
      throw EnsoContext.get(warningsLib).raiseAssertionPanic(warningsLib, null, null);
    }
  }

  private static Warning[] getAllElementWarnings(Object value, WarningsLibrary warnings, InteropLibrary interop) {
    EconomicSet<Warning> warningsSet = EconomicSet.create(new WithWarnings.WarningEquivalence());
      //System.out.println("AAA " + value);
    var ctx = EnsoContext.get(warnings);

    try {
      long size = interop.getArraySize(value);

      for (long index = 0; index < size; ++index) {
        final long finalIndex = index;
        Warning[] elementWarnings = warnings.getElementWarnings(value, null, index);
        Warning wrapped[] = Arrays.stream(elementWarnings).map(warning -> {
          var error = warning.getValue();
          var wrappedError = ctx.getBuiltins().error().makeMapError(finalIndex, error);
          var wrappedWarning = Warning.create(ctx, wrappedError, warning.getOrigin());
          return wrappedWarning;
        }).toArray(Warning[]::new);
        warningsSet.addAll(Arrays.asList(wrapped));
      }

      return Warning.fromSetToArray(warningsSet);
    } catch (InvalidArrayIndexException e) {
      throw ctx.raiseAssertionPanic(warnings, null, e);
    } catch (UnsupportedMessageException e) {
      System.out.println("AAA ume " + e + " " + value);
      throw ctx.raiseAssertionPanic(warnings, null, e);
    }
  }

  @Builtin.Method(
      description =
          "Returns `true` if the maximal number of warnings has been reached, `false` otherwise.",
      autoRegister = false)
  @Builtin.Specialize
  public static boolean limitReached(WithWarnings value, WarningsLibrary warnings) {
    return value.isLimitReached();
  }

  @Builtin.Method(
      description =
          "Returns `true` if the maximal number of warnings has been reached, `false` otherwise.",
      autoRegister = false)
  @Builtin.Specialize(fallback = true)
  public static boolean limitReached(Object value, WarningsLibrary warnings) {
    return warnings.hasWarnings(value) ? warnings.isLimitReached(value) : false;
  }

  @CompilerDirectives.TruffleBoundary
  private static void sortArray(Warning[] arr) {
    Arrays.sort(arr, Comparator.comparing(Warning::getSequenceId).reversed());
  }

  /** Converts set to an array behing a truffle boundary. */
  @CompilerDirectives.TruffleBoundary
  public static Warning[] fromSetToArray(EconomicSet<Warning> set) {
    return set.toArray(new Warning[set.size()]);
  }

  @Builtin.Method(
      name = "set_array",
      description = "Sets all the warnings associated with the value.",
      autoRegister = false)
  @Builtin.Specialize
  public static Object set(
      EnsoContext ctx, WithWarnings value, Object warnings, InteropLibrary interop) {
    return setGeneric(ctx, value.getValue(), interop, warnings);
  }

  @Builtin.Method(
      name = "set_array",
      description = "Sets all the warnings associated with the value.",
      autoRegister = false)
  @Builtin.Specialize(fallback = true)
  public static Object set(EnsoContext ctx, Object value, Object warnings, InteropLibrary interop) {
    return setGeneric(ctx, value, interop, warnings);
  }

  @Builtin.Method(
      name = "get_warnings_limit",
      description = "Returns a maximal number of warnings that can be attached to a value.",
      autoRegister = false)
  @Builtin.Specialize
  public static long getWarningsLimit(EnsoContext ctx) {
    return ctx.getWarningsLimit();
  }

  private static Object setGeneric(
      EnsoContext ctx, Object value, InteropLibrary interop, Object warnings) {
    try {
      var size = interop.getArraySize(warnings);
      if (size == 0) {
        return value;
      }
      Warning[] warningsCast = new Warning[(int) size];
      for (int i = 0; i < warningsCast.length; i++) {
        warningsCast[i] = (Warning) interop.readArrayElement(warnings, i);
      }
      return WithWarnings.wrap(ctx, value, warningsCast);
    } catch (UnsupportedMessageException | InvalidArrayIndexException ex) {
      throw EnsoContext.get(interop).raiseAssertionPanic(interop, null, ex);
    }
  }

  @CompilerDirectives.TruffleBoundary
  @Override
  public String toString() {
    return value.toString();
  }

  @ExportLibrary(InteropLibrary.class)
  public static final class Reassignment implements EnsoObject {
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

  public long getSequenceId() {
    return sequenceId;
  }

  @CompilerDirectives.TruffleBoundary
  public Warning reassign(Node location) {
    RootNode root = location.getRootNode();
    SourceSection section = location.getEncapsulatingSourceSection();
    Reassignment reassignment = new Reassignment(root.getName(), section);
    return new Warning(value, origin, sequenceId, reassignments.prepend(reassignment));
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib, @Cached("1") int ignore) {
    return EnsoContext.get(thisLib).getBuiltins().warning();
  }
}
