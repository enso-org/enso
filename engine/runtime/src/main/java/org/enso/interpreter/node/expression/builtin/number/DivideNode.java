package org.enso.interpreter.node.expression.builtin.number;

import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema.CallStrategy;

@BuiltinMethod(type = "Number", name = "/")
@NodeInfo(shortName = "Number./", description = "Division for numbers.")
public class DivideNode extends Node {
  public long execute(long self, long that) {
    return self / that;
  }
}