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
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.AtomConstructor;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypeOfNode;

@BuiltinMethod(
    type = "Meta",
    name = "get_annotation",
    description = "Get annotation associated with an object",
    autoRegister = false)
@SuppressWarnings("truffle-splitting")
public abstract class GetAnnotationNode extends BaseNode {

  abstract Object execute(VirtualFrame frame, Object target, Object method, Object parameter);

  @Specialization
  Object doExecute(
      VirtualFrame frame,
      Object target,
      Object method,
      Object parameter,
      @Cached ThunkExecutorNode thunkExecutorNode,
      @Cached ExpectStringNode expectStringNode,
      @Cached TypeOfNode typeOfNode) {
    var targetTypes = typeOfNode.findAllTypesOrNull(target, false);

    if (targetTypes == null) {
      if (typeOfNode.findTypeOrError(target) instanceof DataflowError err) {
        return err;
      }
      targetTypes = new Type[0];
    }
    for (var targetTypeResult : targetTypes) {
      var res =
          findAnnotationOrNull(
              frame,
              targetTypeResult,
              target,
              method,
              parameter,
              thunkExecutorNode,
              expectStringNode);
      if (res != null) {
        return res;
      }
    }
    return EnsoContext.get(this).getNothing();
  }

  private Object findAnnotationOrNull(
      VirtualFrame frame,
      Type targetTypeResult,
      Object target,
      Object method,
      Object parameter,
      ThunkExecutorNode thunkExecutorNode,
      ExpectStringNode expectStringNode) {

    if (targetTypeResult instanceof Type targetType) {
      Function methodFunction;
      if (method instanceof UnresolvedSymbol symbol) {
        var pair = symbol.resolveFor(this, targetType);
        methodFunction = pair == null ? null : pair.function();
      } else {
        CompilerDirectives.transferToInterpreter();
        var ctx = EnsoContext.get(this);
        var err = ctx.getBuiltins().error();
        var payload =
            err.makeUnsupportedArgumentsError(
                new Object[] {method}, "Use .name to specify name of function");
        throw new PanicException(payload, this);
      }
      if (methodFunction != null) {
        String parameterName = expectStringNode.execute(parameter);
        Annotation annotation = methodFunction.getSchema().getAnnotation(parameterName);
        if (annotation != null) {
          return executeAnnotation(annotation, frame, thunkExecutorNode, getTailStatus());
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
            return executeAnnotation(annotation, frame, thunkExecutorNode, getTailStatus());
          }
        }
      }
    }
    return null;
  }

  private Object executeAnnotation(
      Annotation annotation,
      VirtualFrame frame,
      ThunkExecutorNode thunkExecutorNode,
      TailStatus tail) {
    var target = annotation.getExpression().getCallTarget();
    var thunk = Function.thunk(target, frame.materialize());
    var ctx = EnsoContext.get(this);
    var state = ctx.currentState();
    var result = thunkExecutorNode.executeThunk(frame, thunk, state, tail);
    return result;
  }

  static GetAnnotationNode build() {
    return GetAnnotationNodeGen.create();
  }

  @CompilerDirectives.TruffleBoundary
  private static AtomConstructor getAtomConstructor(Type type, String name) {
    return type.getConstructors().get(name);
  }
}
