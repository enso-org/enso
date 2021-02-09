package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.EnsoFile;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(
    type = "Prim_Io",
    name = "get_file",
    description =
        "Takes the text representation of a path and returns a TruffleFile corresponding to it.")
public abstract class GetFileNode extends Node {
  static GetFileNode build() {
    return GetFileNodeGen.create();
  }

  abstract Object execute(Object _this, Object path);

  @Specialization
  Object doGetFile(
      Object _this,
      Object path,
      @CachedContext(Language.class) Context ctx,
      @Cached("build()") ExpectStringNode expectStringNode) {
    String pathStr = expectStringNode.execute(path);
    TruffleFile file = ctx.getEnvironment().getPublicTruffleFile(pathStr);
    EnsoFile ensoFile = new EnsoFile(file);
    return ctx.getEnvironment().asGuestValue(ensoFile);
  }
}
