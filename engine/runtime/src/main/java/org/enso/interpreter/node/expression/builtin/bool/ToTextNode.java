package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Boolean", name = "to_text")
@NodeInfo(shortName = "Boolean.to_text", description = "Boolean to text conversion.")
public class ToTextNode extends Node {
  public String execute(boolean self) {
    return self ? "True" : "False";
  }
}
