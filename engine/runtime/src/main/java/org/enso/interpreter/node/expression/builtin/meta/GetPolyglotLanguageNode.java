package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(
    type = "Meta",
    name = "get_polyglot_language",
    description = "Returns a text representation of a language of origin of a given value.",
    autoRegister = false)
public abstract class GetPolyglotLanguageNode extends Node {
  static GetPolyglotLanguageNode build() {
    return GetPolyglotLanguageNodeGen.create();
  }

  private final Text java = Text.create("java");
  private final Text unknown = Text.create("unknown");

  abstract Text execute(Object value);

  @Specialization
  Text doExecute(Object value) {
    if (EnsoContext.get(this).isJavaPolyglotObject(value)) {
      return java;
    } else {
      return unknown;
    }
  }
}
