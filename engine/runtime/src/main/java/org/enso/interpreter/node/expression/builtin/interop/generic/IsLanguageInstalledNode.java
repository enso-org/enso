package org.enso.interpreter.node.expression.builtin.interop.generic;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.runtime.Context;

@BuiltinMethod(
    type = "Polyglot",
    name = "is_language_installed",
    description = "Checks if a polyglot language is installed in the runtime environment.")
public abstract class IsLanguageInstalledNode extends Node {

  static IsLanguageInstalledNode build() {
    return IsLanguageInstalledNodeGen.create();
  }

  @Specialization
  boolean doExecute(Object _this, Object language_name, @Cached ExpectStringNode expectStringNode) {
    String name = expectStringNode.execute(language_name);
    return Context.get(this).getEnvironment().getPublicLanguages().get(name) != null;
  }

  abstract boolean execute(Object _this, Object language_name);
}
