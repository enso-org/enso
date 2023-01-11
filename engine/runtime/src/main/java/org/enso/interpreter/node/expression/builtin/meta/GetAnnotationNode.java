package org.enso.interpreter.node.expression.builtin.meta;

import akka.http.scaladsl.model.headers.Expect;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.node.expression.builtin.interop.generic.InvokeNode;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.node.expression.foreign.CoerceNothing;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Meta",
    name = "get_annotation",
    description = "Get annotation associated with an object",
    autoRegister = false)
public abstract class GetAnnotationNode extends Node {

  abstract Object execute(VirtualFrame frame, State state, Object target, Object parameter);

  @Specialization
  Object doExecute(
      VirtualFrame frame,
      State state,
      Object target,
      Object parameter,
      @Cached ThunkExecutorNode thunkExecutorNode,
      @Cached ExpectStringNode expectStringNode) {
    String parameterName = expectStringNode.execute(parameter);
    if (target instanceof Function function) {
      FunctionSchema schema = function.getSchema();
      Function expr = schema.getAnnotation(parameterName);
      if (expr != null) {
        return thunkExecutorNode.executeThunk(expr, state, BaseNode.TailStatus.NOT_TAIL);
      }
    }
    return EnsoContext.get(this).getNothing();
  }

  static GetAnnotationNode build() {
    return GetAnnotationNodeGen.create();
  }
}
