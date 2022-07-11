package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.interop.syntax.HostValueToEnsoNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(type = "Array", name = "copy", description = "Copies one array to another.")
public abstract class CopyNode extends Node {

  static CopyNode build() {
    return CopyNodeGen.create();
  }

  abstract Object execute(
      Object self, Object src, long source_index, Array dest, long dest_index, long count);

  @Specialization
  Object doArray(
      Object self, Array src, long source_index, Array dest, long dest_index, long count) {
    System.arraycopy(
        src.getItems(), (int) source_index, dest.getItems(), (int) dest_index, (int) count);
    return Context.get(this).getBuiltins().nothing().newInstance();
  }

  @Specialization(guards = "arrays.hasArrayElements(src)")
  Object doPolyglotArray(
      Object self,
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
          Context.get(this)
              .getBuiltins()
              .error()
              .makeInvalidArrayIndexError(src, e.getInvalidIndex()),
          this);
    }
    return Context.get(this).getBuiltins().nothing().newInstance();
  }

  @Fallback
  Object doOther(
      Object self, Object src, long source_index, Array dest, long dest_index, long count) {
    Builtins builtins = Context.get(this).getBuiltins();
    throw new PanicException(
        builtins.error().makeTypeError(builtins.array().newInstance(), src, "src"), this);
  }
}
