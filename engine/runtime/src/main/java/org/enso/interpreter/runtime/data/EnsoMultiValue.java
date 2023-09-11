package org.enso.interpreter.runtime.data;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import java.util.Arrays;
import java.util.stream.Collectors;
import org.enso.interpreter.node.callable.resolver.MethodResolverNode;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.graalvm.collections.Pair;

@ExportLibrary(TypesLibrary.class)
@ExportLibrary(InteropLibrary.class)
public final class EnsoMultiValue implements EnsoObject {
  @CompilationFinal(dimensions = 1)
  private final Type[] types;

  @CompilationFinal(dimensions = 1)
  private final Object[] values;

  private EnsoMultiValue(Type[] types, Object[] values) {
    this.types = types;
    assert types.length == values.length;
    this.values = values;
  }

  public static EnsoObject create(Type[] types, Object[] values) {
    return new EnsoMultiValue(types, values);
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  boolean hasSpecialDispatch() {
    return true;
  }

  @ExportMessage
  public final Type getType() {
    return types[0];
  }

  @ExportMessage
  String toDisplayString(boolean ignore) {
    return toString();
  }

  @TruffleBoundary
  @Override
  public String toString() {
    return Arrays.stream(types).map(t -> t.getName()).collect(Collectors.joining(" & "));
  }

  /**
   * Casts value in this multi value into specific t.
   *
   * @param type the requested t
   * @return instance of the {@code t} or {@code null} if no suitable value was found
   */
  public final Object castTo(Type type) {
    for (var i = 0; i < types.length; i++) {
      if (types[i] == type) {
        return values[i];
      }
    }
    return null;
  }

  /**
   * Tries to resolve the symbol in one of multi value types.
   *
   * @param node resolution node to use
   * @param symbol symbol to resolve
   * @return {@code null} when no resolution was found or pair of function and type solved
   */
  public final Pair<Function, Type> resolveSymbol(
      MethodResolverNode node, UnresolvedSymbol symbol) {
    for (Type t : types) {
      var fn = node.execute(t, symbol);
      if (fn != null) {
        return Pair.create(fn, t);
      }
    }
    return null;
  }
}
