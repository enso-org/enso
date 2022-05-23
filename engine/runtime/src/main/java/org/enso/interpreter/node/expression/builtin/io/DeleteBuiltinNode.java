package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.epb.node.ContextRewrapExceptionNode;
import org.enso.interpreter.epb.runtime.GuardedTruffleContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.data.EnsoFile;

import java.io.IOException;

@BuiltinMethod(type = "File", name = "delete_builtin", description = "")
public abstract class DeleteBuiltinNode extends Node {

  abstract Object execute(Object _this);

  static DeleteBuiltinNode build() {
    return DeleteBuiltinNodeGen.create();
  }

  @Specialization
  Object execute(EnsoFile _this) {
    try {
      _this.delete();
      return Context.get(this).getBuiltins().nothing().newInstance();
    } catch (IOException e) {
      Context context = Context.get(this);
      Builtins builtins = context.getBuiltins();
      Object payload = builtins.error().makePolyglotError(e);
      throw new PanicException(payload, this);
    }
  }
}
