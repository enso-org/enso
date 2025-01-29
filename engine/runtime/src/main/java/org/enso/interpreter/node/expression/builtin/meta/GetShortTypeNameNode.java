package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;

@BuiltinMethod(
    type = "Meta",
    name = "get_short_type_name",
    description = "Gets the short name of a Type.",
    autoRegister = false)
public class GetShortTypeNameNode extends Node {
  Object execute(Object metaObject) {
    if (metaObject instanceof Type type) {
      return Text.create(type.getName());
    } else {
      try {
        var iop = InteropLibrary.getUncached();
        var name = iop.getMetaSimpleName(metaObject);
        return Text.create(iop.asString(name));
      } catch (UnsupportedMessageException ex) {
        var ctx = EnsoContext.get(this);
        var err =
            ctx.getBuiltins()
                .error()
                .makeUnsupportedArgumentsError(
                    new Object[] {metaObject}, "Doesn't represent a type");
        return DataflowError.withDefaultTrace(err, this);
      }
    }
  }
}
