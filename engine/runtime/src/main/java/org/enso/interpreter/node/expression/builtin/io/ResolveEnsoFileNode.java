package org.enso.interpreter.node.expression.builtin.io;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.runtime.data.EnsoFile;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.data.text.Text;

@BuiltinMethod(type = "File", name = "resolve", description = "")
public abstract class ResolveEnsoFileNode extends Node {

  abstract EnsoFile execute(EnsoFile _this, Object subpath);

  static ResolveEnsoFileNode build() {
    return ResolveEnsoFileNodeGen.create();
  }

  @Specialization
  public EnsoFile doString(EnsoFile _this, String subPath) {
    return new EnsoFile(_this.resolveUnderlying(subPath));
  }

  @Specialization
  public EnsoFile doText(EnsoFile _this, Text subPath) {
    String subpathStr = (String) subPath.getContents();
    return new EnsoFile(_this.resolveUnderlying(subpathStr));
  }

  @Specialization
  public EnsoFile doFile(EnsoFile _this, EnsoFile subPath) {
    return new EnsoFile(_this.resolveUnderlying(subPath));
  }
}
