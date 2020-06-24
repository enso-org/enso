package org.enso.interpreter.node.expression.builtin.bool;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;

@BuiltinMethod(type = "Boolean", name = "not", description = "Negates a boolean value")
public class NotNode extends Node {
  boolean execute(boolean self) {
    return !self;
  }
}
