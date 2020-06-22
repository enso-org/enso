package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Boolean", name = "||")
@NodeInfo(shortName = "Boolean.||", description = "Computes the logical OR of two booleans")
public class OrNode extends Node {
  public boolean execute(boolean self, boolean that) {
    return self || that;
  }
}