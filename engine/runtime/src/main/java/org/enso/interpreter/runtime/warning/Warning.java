package org.enso.interpreter.runtime.warning;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.Builtin;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.hash.EnsoHashMap;
import org.enso.interpreter.runtime.data.hash.HashMapInsertNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeAtNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;

@Builtin(pkg = "error", stdlibName = "Standard.Base.Warning.Warning")
@ExportLibrary(TypesLibrary.class)
public final class Warning implements EnsoObject {
  private final Object value;
  private final Object origin;
  private final long sequenceId;

  private Warning(Object value, Object origin, long sequenceId) {
    this.value = value;
    this.origin = origin;
    this.sequenceId = sequenceId;
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
  public static WithWarnings attach(
      EnsoContext ctx,
      Object value,
      Object warning,
      Object origin,
      @Cached AppendWarningNode appendWarningNode) {
    var warn = new Warning(warning, origin, ctx.nextSequenceId());
    return appendWarningNode.execute(null, value, warn);
  }

  /** Slow version of {@link #fromMapToArray(EnsoHashMap, ArrayLikeLengthNode, ArrayLikeAtNode)}. */
  public static Warning[] fromMapToArray(EnsoHashMap map) {
    return fromMapToArray(map, ArrayLikeLengthNode.getUncached(), ArrayLikeAtNode.getUncached());
  }

  public static Warning[] fromMapToArray(
      EnsoHashMap map, ArrayLikeLengthNode lengthNode, ArrayLikeAtNode atNode) {
    var vec = map.getCachedVectorRepresentation();
    var vecLen = Math.toIntExact(lengthNode.executeLength(vec));
    Warning[] warns = new Warning[vecLen];
    try {
      for (int i = 0; i < vecLen; i++) {
        var entry = atNode.executeAt(vec, i);
        assert lengthNode.executeLength(entry) == 2;
        var value = atNode.executeAt(entry, 1);
        warns[i] = (Warning) value;
      }
    } catch (InvalidArrayIndexException | ClassCastException e) {
      throw CompilerDirectives.shouldNotReachHere(e);
    }
    return warns;
  }

  public static EnsoHashMap fromArrayToMap(Warning[] warnings, HashMapInsertNode mapInsertNode) {
    var map = EnsoHashMap.empty();
    for (var warn : warnings) {
      map = mapInsertNode.execute(null, map, warn.getSequenceId(), warn);
    }
    return map;
  }

  @CompilerDirectives.TruffleBoundary
  @Override
  public String toString() {
    return value.toString();
  }

  public long getSequenceId() {
    return sequenceId;
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@Bind("$node") Node node) {
    return EnsoContext.get(node).getBuiltins().warning();
  }

  public static Warning wrapMapError(WarningsLibrary warningsLib, Warning warning, long index) {
    var ctx = EnsoContext.get(warningsLib);
    var error = warning.getValue();
    var wrappedError = ctx.getBuiltins().error().makeMapError(index, error);
    return Warning.create(ctx, wrappedError, warning.getOrigin());
  }
}
