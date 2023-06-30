package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.PanicException;

public abstract class CopyNode extends Node {
  public static CopyNode build() {
    return CopyNodeGen.create();
  }

  public abstract Object execute(
      Object src, long source_index, Array dest, long dest_index, long count);

  @Specialization
  Object doArray(Array src, long source_index, Array dest, long dest_index, long count) {
    System.arraycopy(
        src.getItems(), (int) source_index, dest.getItems(), (int) dest_index, (int) count);
    return EnsoContext.get(this).getBuiltins().nothing();
  }

  @Specialization(guards = "arrays.hasArrayElements(src)")
  Object doPolyglotArray(
      Object src,
      long source_index,
      Array dest,
      long dest_index,
      long count,
      @CachedLibrary(limit = "3") InteropLibrary arrays,
      @Cached HostValueToEnsoNode hostValueToEnsoNode) {
    try {
      for (int i = 0; i < count; i++) {
        dest.getItems()[(int) dest_index + i] =
            hostValueToEnsoNode.execute(arrays.readArrayElement(src, source_index + i));
      }
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Unreachable");
    } catch (InvalidArrayIndexException e) {
      throw new PanicException(
          EnsoContext.get(this)
              .getBuiltins()
              .error()
              .makeInvalidArrayIndex(src, e.getInvalidIndex()),
          this);
    }
    return EnsoContext.get(this).getBuiltins().nothing();
  }

  @Fallback
  Object doOther(Object src, long source_index, Array dest, long dest_index, long count) {
    Builtins builtins = EnsoContext.get(this).getBuiltins();
    throw new PanicException(builtins.error().makeTypeError(builtins.array(), src, "src"), this);
  }
}
