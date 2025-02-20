package org.enso.interpreter.runtime.warning;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.StopIterationException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import org.enso.interpreter.dsl.AcceptsWarning;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.BuiltinObject;
import org.enso.interpreter.runtime.data.hash.EnsoHashMap;
import org.enso.interpreter.runtime.data.hash.HashMapInsertNode;

@Builtin(pkg = "error", stdlibName = "Standard.Base.Warning.Warning")
@ExportLibrary(value = InteropLibrary.class, delegateTo = "value")
public final class Warning extends BuiltinObject {
  final Object value;
  private final Object origin;
  private final long sequenceId;

  private Warning(Object value, Object origin, long sequenceId) {
    this.value = value;
    this.origin = origin;
    this.sequenceId = sequenceId;
  }

  @Override
  protected String builtinName() {
    return "Warning";
  }

  @Builtin.Method(name = "value", description = "Gets the payload of the warning.")
  @SuppressWarnings("generic-enso-builtin-type")
  public Object getValue() {
    return value;
  }

  @Builtin.Method(name = "origin", description = "Gets the payload of the warning.")
  @SuppressWarnings("generic-enso-builtin-type")
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

  @Builtin.Method(
      name = "attach_with_stacktrace",
      description = "Attaches the given warning to the value.",
      autoRegister = false)
  @Builtin.Specialize
  @SuppressWarnings("generic-enso-builtin-type")
  public static Object attach(
      EnsoContext ctx,
      VirtualFrame frame,
      @AcceptsWarning Object value,
      Object warning,
      Object origin,
      @CachedLibrary WarningsLibrary warnings,
      @Cached AppendWarningNode appendWarningNode) {
    var warn = new Warning(warning, origin, ctx.nextSequenceId());
    return appendWarningNode.executeAppend(frame, value, warn);
  }

  /** Slow version of {@link #fromMapToArray(EnsoHashMap, InteropLibrary)}. */
  @TruffleBoundary
  public static Warning[] fromMapToArray(EnsoHashMap map) {
    return fromMapToArray(map, InteropLibrary.getUncached());
  }

  public static Warning[] fromMapToArray(EnsoHashMap map, InteropLibrary interop) {
    assert interop.hasHashEntries(map);
    Warning[] warns = null;
    try {
      long mapSize = interop.getHashSize(map);
      assert mapSize < Integer.MAX_VALUE;
      warns = new Warning[(int) mapSize];
      var hashValuesIt = interop.getHashValuesIterator(map);
      assert interop.isIterator(hashValuesIt);
      int warnsIdx = 0;
      while (interop.hasIteratorNextElement(hashValuesIt)) {
        var value = interop.getIteratorNextElement(hashValuesIt);
        warns[warnsIdx] = (Warning) value;
        warnsIdx++;
      }
      return warns;
    } catch (UnsupportedMessageException | ClassCastException | ArrayIndexOutOfBoundsException e) {
      throw CompilerDirectives.shouldNotReachHere(e);
    } catch (StopIterationException e) {
      assert warns != null;
      return warns;
    }
  }

  public static EnsoHashMap fromArrayToMap(Warning[] warnings, HashMapInsertNode mapInsertNode) {
    var map = EnsoHashMap.empty();
    for (var warn : warnings) {
      map = mapInsertNode.execute(null, map, warn.getSequenceId(), warn);
    }
    return map;
  }

  @ExportMessage
  @TruffleBoundary
  @Override
  public Object toDisplayString(boolean enableSideEffects) {
    return toString();
  }

  @ExportMessage
  boolean isNull() {
    return false;
  }

  @CompilerDirectives.TruffleBoundary
  @Override
  public String toString() {
    return value.toString();
  }

  public long getSequenceId() {
    return sequenceId;
  }

  public static Warning wrapMapError(WarningsLibrary warningsLib, Warning warning, long index) {
    var ctx = EnsoContext.get(warningsLib);
    var error = warning.getValue();
    var wrappedError = ctx.getBuiltins().error().makeMapError(index, error);
    return Warning.create(ctx, wrappedError, warning.getOrigin());
  }
}
