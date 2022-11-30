package org.enso.interpreter.node.expression.builtin;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.*;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.PanicException;

@BuiltinMethod(type = "Integer", name = "parse", description = """
Parse integer number""", autoRegister = false)
public final class ParseIntegerNode extends Node {
  @Node.Child
  ToJavaStringNode toJavaString = ToJavaStringNode.build();

  Object execute(Text value, long radix) {
    try {
      return Long.parseLong(toJavaString.execute(value), Math.toIntExact(radix));
    } catch (NumberFormatException ex) {
      var err = Context.get(this).getBuiltins().error();
      throw new PanicException(
        err.makeNumberParseError(ex.getMessage()),
        this
      );
    }
  }
}

