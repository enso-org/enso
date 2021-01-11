package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.TruffleLanguage.ContextReference;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
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
      Object _this, Object src, long source_index, Array that, long dest_index, long count);

  @Specialization
  Object doArray(
      Object _this,
      Array src,
      long source_index,
      Array that,
      long dest_index,
      long count,
      @CachedContext(Language.class) Context ctx) {
    System.arraycopy(
        src.getItems(), (int) source_index, that.getItems(), (int) dest_index, (int) count);
    return ctx.getBuiltins().nothing().newInstance();
  }

  @Specialization(guards = "arrays.hasArrayElements(src)")
  Object doPolyglotArray(
      Object _this,
      Object src,
      long source_index,
      Array that,
      long dest_index,
      long count,
      @CachedLibrary(limit = "3") InteropLibrary arrays,
      @CachedContext(Language.class) Context ctx) {
    try {
      for (int i = 0; i < count; i++) {
        that.getItems()[(int) dest_index + i] = arrays.readArrayElement(src, source_index + i);
      }
    } catch (UnsupportedMessageException e) {
      throw new IllegalStateException("Unreachable");
    } catch (InvalidArrayIndexException e) {
      throw new PanicException(
          ctx.getBuiltins().error().makeInvalidArrayIndexError(src, e.getInvalidIndex()), this);
    }
    return ctx.getBuiltins().nothing().newInstance();
  }

  @Fallback
  Object doOther(
      Object _this, Object src, long source_index, Array that, long dest_index, long count) {
    Builtins builtins = lookupContextReference(Language.class).get().getBuiltins();
    throw new PanicException(
        builtins.error().makeTypeError(builtins.mutable().array().newInstance(), src), this);
  }
}
