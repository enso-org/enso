package org.enso.interpreter.node.expression.builtin.interop.java;

import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.Language;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.Context;

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
  Object doExecute(Object _this, String path, @CachedContext(Language.class) Context context) {
    context.getEnvironment().addToHostClassPath(context.getTruffleFile(new File(path)));
    return context.getBuiltins().unit();
  }

  abstract Object execute(Object _this, String path);
}
