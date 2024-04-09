package org.enso.interpreter.runtime.data.vector;

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
import org.enso.interpreter.runtime.data.EnsoObject;

public abstract class ArrayLikeCopyToArrayNode extends Node {
  public static ArrayLikeCopyToArrayNode build() {
    return ArrayLikeCopyToArrayNodeGen.create();
  }

  public abstract Object execute(
      Object src, long srcIndex, EnsoObject destArr, long destIndex, long count);

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
      throw EnsoContext.get(arrays).raiseAssertionPanic(arrays, null, e);
    } catch (InvalidArrayIndexException e) {
      throw ArrayPanics.invalidIndex(this, src, e);
    }
    return EnsoContext.get(this).getBuiltins().nothing();
  }

  @Fallback
  Object doOther(Object src, long source_index, EnsoObject dest, long dest_index, long count) {
    throw ArrayPanics.typeError(this, src, "src");
  }
}
