package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.builtin.LanguageEntitySerializer;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "Any", name = "json_serialize", description = "Generic JSON serialization.")
public class JsonSerializeNode extends Node {
  @CompilerDirectives.TruffleBoundary
  Text execute(Object _this) {
    return Text.create(LanguageEntitySerializer.serialize(_this));
  }
}
