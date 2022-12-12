package org.enso.interpreter.node.expression.builtin.interop.java;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.runtime.EnsoContext;

@BuiltinMethod(
    type = "Java",
    name = "lookup_class",
    description = "Looks up a Java symbol.",
    autoRegister = false)
public abstract class LookupClassNode extends Node {
  static LookupClassNode build() {
    return LookupClassNodeGen.create();
  }

  @Specialization
  Object doExecute(Object name, @Cached("build()") ExpectStringNode expectStringNode) {
    return EnsoContext.get(this).getEnvironment().lookupHostSymbol(expectStringNode.execute(name));
  }

  abstract Object execute(Object name);
}
