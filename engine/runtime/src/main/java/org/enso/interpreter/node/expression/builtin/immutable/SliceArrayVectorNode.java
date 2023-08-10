package org.enso.interpreter.node.expression.builtin.immutable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Vector;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(type = "Vector", name = "slice", description = "Returns a slice of this Vector.")
public abstract class SliceArrayVectorNode extends Node {
  SliceArrayVectorNode() {}

  public static SliceArrayVectorNode build() {
    return SliceArrayVectorNodeGen.create();
  }

  abstract Object execute(Object self, long start, long end);

  @Specialization
  Object sliceArray(Array self, long start, long end) {
    return Array.slice(self, start, end, self.length());
  }

  @Specialization
  Object sliceVector(
      Vector self,
      long start,
      long end,
      @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary iop) {
    try {
      return Array.slice(self, start, end, self.length(iop));
    } catch (UnsupportedMessageException ex) {
      CompilerDirectives.transferToInterpreter();
      throw unsupportedMessageException(self);
    }
  }

  @Specialization(replaces = {"sliceArray", "sliceVector"})
  Object sliceArrayLike(
      Object self,
      long start,
      long end,
      @Shared("interop") @CachedLibrary(limit = "3") InteropLibrary iop) {
    try {
      long len = iop.getArraySize(self);
      return Array.slice(self, start, end, len);
    } catch (UnsupportedMessageException ex) {
      CompilerDirectives.transferToInterpreter();
      throw unsupportedMessageException(self);
    }
  }

  private PanicException unsupportedMessageException(Object self) throws PanicException {
    var ctx = EnsoContext.get(this);
    var arrayType = ctx.getBuiltins().array();
    throw new PanicException(
        ctx.getBuiltins().error().makeTypeError(arrayType, self, "self"), this);
  }
}
