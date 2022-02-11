package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(
    type = "Prim_Io",
    name = "user_home",
    description = "Get the text path to the user home directory.")
public final class GetUserHomeNode extends Node {
  private final Text home = Text.create(System.getProperty("user.home"));

  Text execute(Object _this) {
    return home;
  }
}
