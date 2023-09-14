package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.node.expression.builtin.text.util.ExpectStringNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.Annotation;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Meta",
    name = "get_annotation",
    description = "Get annotation associated with an object",
    autoRegister = false)
public abstract class GetAnnotationNode extends BaseNode {

  abstract Object execute(
      VirtualFrame frame, State state, Object target, Object method, Object parameter);

  @Specialization
  Object doExecute(
      VirtualFrame frame,
      State state,
      Object target,
      Object method,
      Object parameter,
      @Cached ThunkExecutorNode thunkExecutorNode,
      @Cached ExpectStringNode expectStringNode,
      @Cached TypeOfNode typeOfNode) {
    Object targetTypeResult = typeOfNode.execute(target);
    if (targetTypeResult instanceof DataflowError error) {
      return error;
    }

    if (targetTypeResult instanceof Type targetType) {
      Function methodFunction;
      if (method instanceof UnresolvedSymbol symbol) {
        methodFunction = symbol.resolveFor(this, targetType);
      } else {
        CompilerDirectives.transferToInterpreter();
        var ctx = EnsoContext.get(this);
        var err = ctx.getBuiltins().error();
        var payload = err.makeUnsupportedArgumentsError(new Object[] { method }, "Use .name to specify name of function");
        throw new PanicException(payload, this);
       }
      if (methodFunction != null) {
        String parameterName = expectStringNode.execute(parameter);
        Annotation annotation = methodFunction.getSchema().getAnnotation(parameterName);
        if (annotation != null) {
          Function thunk =
              Function.thunk(annotation.getExpression().getCallTarget(), frame.materialize());
          return thunkExecutorNode.executeThunk(frame, thunk, state, getTailStatus());
        }
      }
      if (target instanceof Type type) {
        String methodName = ((UnresolvedSymbol) symbol).getName();
        AtomConstructor constructor = getAtomConstructor(type, methodName);
        if (constructor != null) {
          Function constructorFunction = constructor.getConstructorFunction();
          String parameterName = expectStringNode.execute(parameter);
          Annotation annotation = constructorFunction.getSchema().getAnnotation(parameterName);
          if (annotation != null) {
            Function thunk =
                Function.thunk(annotation.getExpression().getCallTarget(), frame.materialize());
            return thunkExecutorNode.executeThunk(frame, thunk, state, getTailStatus());
          }
        }
      }
    }

    return EnsoContext.get(this).getNothing();
  }

  static GetAnnotationNode build() {
    return GetAnnotationNodeGen.create();
  }

  @CompilerDirectives.TruffleBoundary
  private static AtomConstructor getAtomConstructor(Type type, String name) {
    return type.getConstructors().get(name);
  }
}
