package org.enso.interpreter.node.expression.builtin.mutable;

import com.oracle.truffle.api.TruffleLanguage.ContextReference;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.Array;

@BuiltinMethod(type = "Array", name = "copy", description = "Copies one array to another.")
public abstract class CopyNode extends Node {

  static CopyNode build() {
    return CopyNodeGen.create();
  }

  abstract Object execute(Array _this, long source_index, Array that, long dest_index, long count);

  @Specialization
  Object doArray(
      Array _this,
      long source_index,
      Array that,
      long dest_index,
      long count,
      @CachedContext(Language.class) ContextReference<Context> ctxRef) {
    System.arraycopy(
        _this.getItems(), (int) source_index, that.getItems(), (int) dest_index, (int) count);
    return ctxRef.get().getBuiltins().nothing().newInstance();
  }
}
