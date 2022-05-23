package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.EnsoFile;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "File", name = "new", description = "Creates a new File")
public abstract class NewFileNode extends Node {
  static NewFileNode build() {
    return NewFileNodeGen.create();
  }

  abstract Object execute(Object _this, Object path);

  @Specialization
  Object doString(Object _this, String path) {
    var context = Context.get(this);
    TruffleFile file = context.getEnvironment().getPublicTruffleFile(path);
    EnsoFile ensoFile = new EnsoFile(file);
    return ensoFile;
  }

  @Specialization
  Object doText(Object _this, Text path) {
    String pathStr = (String) path.getContents();
    var context = Context.get(this);
    TruffleFile file = context.getEnvironment().getPublicTruffleFile(pathStr);
    EnsoFile ensoFile = new EnsoFile(file);
    return ensoFile;
  }

  @Specialization
  Object doFile(Object _this, EnsoFile f) {
    return f;
  }
}
