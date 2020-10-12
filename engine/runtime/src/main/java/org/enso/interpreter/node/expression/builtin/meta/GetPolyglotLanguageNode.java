package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Meta", name = "is_atom", description = "Checks if the argument is an Atom")
public abstract class GetPolyglotLanguageNode extends Node {
  static GetPolyglotLanguageNode build() {
    return GetPolyglotLanguageNodeGen.create();
  }

  private final Text java = Text.create("java");

  abstract Text execute(Object _this, Object value);

  @Specialization
  Text doExecute(Object _this, Object value) {
    return java;
  }
}
