package org.enso.interpreter.node.expression.builtin.special;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.Ref;

@BuiltinMethod(type = "Special", name = "<write_ref>")
public class WriteRefNode extends Node {
  public Object execute(Ref _this, Object value) {
    _this.setValue(value);
    return null;
  }
}
