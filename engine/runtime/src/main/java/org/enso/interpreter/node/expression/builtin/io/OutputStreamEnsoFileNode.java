package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;

import java.io.IOException;
import java.nio.file.OpenOption;

import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.EnsoFile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(type = "File", name = "output_stream", description = "")
public abstract class OutputStreamEnsoFileNode extends Node {

  abstract Object execute(EnsoFile _this, Object opts);

  static OutputStreamEnsoFileNode build() {
    return OutputStreamEnsoFileNodeGen.create();
  }

  @Specialization
  Object execute(EnsoFile _this, Array opts) {
    try {
      Context context = Context.get(this);
      Object[] items = opts.getItems();
      OpenOption[] hostOpts = new OpenOption[items.length];
      for (int i = 0; i < hostOpts.length; i++) {
        hostOpts[i] = (OpenOption) context.getEnvironment().asHostObject(items[i]);
      }
      return context
          .getEnvironment()
          .asGuestValue(_this.withTruffleFile(f -> f.newOutputStream(hostOpts)));
    } catch (IOException e) {
      Context context = Context.get(this);
      Builtins builtins = context.getBuiltins();
      Object payload = builtins.error().makePolyglotError(e);
      throw new PanicException(payload, this);
    }
  }
}
