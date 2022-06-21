package org.enso.interpreter.node.expression.builtin.error.displaytext;

import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Module_Not_In_Package_Error", name = "to_display_text")
public class ModuleNotInPackageErrorToDisplayTextNode extends Node {

  Text execute(Object _this) {
    return Text.create("Module is not a part of a package.");
  }
}
