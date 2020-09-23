package org.enso.interpreter.node.expression.builtin.interop.java;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.data.text.Text;

import java.io.File;

@BuiltinMethod(
    type = "Java",
    name = "add_to_class_path",
    description = "Adds a path to the host class path.")
public abstract class AddToClassPathNode extends Node {

  static AddToClassPathNode build() {
    return AddToClassPathNodeGen.create();
  }

  @Specialization
  Object doExecute(
      Object _this,
      Text path,
      @CachedContext(Language.class) Context context,
      @Cached("build()") ToJavaStringNode toJavaStringNode) {
    context
        .getEnvironment()
        .addToHostClassPath(context.getTruffleFile(new File(toJavaStringNode.execute(path))));
    return context.getBuiltins().unit();
  }

  abstract Object execute(Object _this, Text path);
}
