package org.enso.interpreter.node.expression.builtin.interop.java;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.Node;
import java.io.File;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.runtime.EnsoContext;

@BuiltinMethod(
    type = "Java",
    name = "add_to_class_path",
    description = "Adds a path to the host class path.",
    autoRegister = false)
public abstract class AddToClassPathNode extends Node {

  static AddToClassPathNode build() {
    return AddToClassPathNodeGen.create();
  }

  abstract Object execute(Object path);

  @CompilerDirectives.TruffleBoundary
  @Specialization
  Object doExecute(Object path, @Cached ExpectStringNode expectStringNode) {
    EnsoContext context = EnsoContext.get(this);
    context
        .getEnvironment()
        .addToHostClassPath(context.getTruffleFile(new File(expectStringNode.execute(path))));
    return context.getBuiltins().nothing();
  }
}
