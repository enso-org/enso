package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.EnsoFile;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(
    type = "Prim_Io",
    name = "get_cwd",
    description = "A file corresponding to the current working directory.")
public abstract class GetCwdNode extends Node {
  static GetCwdNode build() {
    return GetCwdNodeGen.create();
  }

  abstract Object execute(Object _this);

  @Specialization
  Object doExecute(Object _this, @CachedContext(Language.class) Context ctx) {
    TruffleFile file = ctx.getEnvironment().getCurrentWorkingDirectory();
    EnsoFile ensoFile = new EnsoFile(file);
    return ctx.getEnvironment().asGuestValue(ensoFile);
  }
}
