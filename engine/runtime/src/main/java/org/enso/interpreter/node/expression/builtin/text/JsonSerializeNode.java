package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.builtin.LanguageEntitySerializer;

@BuiltinMethod(type = "Any", name = "json_serialize", description = "Generic JSON serialization.")
public class JsonSerializeNode extends Node {
  String execute(Object self) {
    return serialize(self);
  }

  @CompilerDirectives.TruffleBoundary
  private String serialize(Object obj) {
    return LanguageEntitySerializer.serialize(obj);
  }
}
