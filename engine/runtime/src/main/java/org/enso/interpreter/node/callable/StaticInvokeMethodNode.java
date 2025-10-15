package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import java.util.UUID;
import org.enso.compiler.core.ConstantsNames;
import org.enso.interpreter.node.callable.InvokeCallableNode.ArgumentsExecutionMode;
import org.enso.interpreter.node.callable.InvokeCallableNode.DefaultsExecutionMode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.callable.resolver.MethodResolverNode;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.state.State;

/**
 * Node responsible for <i>static method invocation</i>. Static method invocation is a method call
 * with specified {@code self} argument at first position. Such invocation will not preapply the
 * {@code self} argument, but will pass it directly to the method. This is different to {@link
 * InstanceInvokeMethodNode instance method invocation}.
 *
 * @see <a
 *     href="https://github.com/enso-org/enso/blob/169c8a51e782fb72ee7c7a209b587c9e52dd0425/docs/types/dynamic-dispatch.md#method-invocation">Method
 *     invocation specification</a>
 */
abstract class StaticInvokeMethodNode extends InvokeMethodNode {

  private @Child InvokeFunctionNode invokeFunctionNode;

  StaticInvokeMethodNode(
      CallArgumentInfo[] schema,
      DefaultsExecutionMode defaultsExecutionMode,
      ArgumentsExecutionMode argumentsExecutionMode,
      int thisArgumentPosition,
      boolean onBoundary) {
    super(schema, defaultsExecutionMode, argumentsExecutionMode, thisArgumentPosition, onBoundary);
    verifyCallSchema(schema);
    var newSchema = removeFirstArg(schema);
    this.invokeFunctionNode =
        InvokeFunctionNode.build(newSchema, defaultsExecutionMode, argumentsExecutionMode);
  }

  @Override
  public void setId(UUID id) {
    throw new UnsupportedOperationException("unimplemented");
  }

  @Specialization
  Object doStaticInvoke(
      VirtualFrame frame,
      State state,
      UnresolvedSymbol symbol,
      Type self,
      Object[] arguments,
      @Cached MethodResolverNode methodResolverNode) {
    var method = resolveMethod(symbol, self, methodResolverNode);
    if (method == null) {
      throw methodNotFound(this, onBoundary, symbol, self);
    }
    if (!isValidStaticCallTarget(method)) {
      var cause =
          new IllegalArgumentException(
              "Method "
                  + method
                  + " must have `self` as first named parameter to be statically invoked");
      throw methodNotInvocable(this, symbol, self, cause);
    }
    var argsWithoutFirst = removeFirstArg(arguments);
    assert invokeFunctionNode.getSchema().length == argsWithoutFirst.length
        : "After removing implicit self argument, the number of arguments must match the function"
              + " schema.";
    return invokeFunctionNode.execute(method, frame, state, argsWithoutFirst);
  }

  @Fallback
  Object fallback(
      VirtualFrame frame, State state, UnresolvedSymbol symbol, Object self, Object[] arguments) {
    var cause = new IllegalArgumentException("Self argument must be of Type, got: " + self);
    throw methodNotInvocable(this, symbol, self, cause);
  }

  private Function resolveMethod(UnresolvedSymbol symbol, Type self, MethodResolverNode methodResolverNode) {
    var method = methodResolverNode.executeResolution(self, symbol);
    if (method != null) {
      return method;
    }
    if (self != self.getEigentype()) {
      return methodResolverNode.executeResolution(self.getEigentype(), symbol);
    }
    return null;
  }

  /** Verifies that the given call argument schema corresponds to static method invocation. */
  private static void verifyCallSchema(CallArgumentInfo[] schema) {
    assert schema.length >= 2
        : "Static method invocation must have at least one named self argument";
    assert schema[0].isPositional()
        : "First argument is an implicit receiver, it should not have a name";
    assert schema[1].isNamed() && schema[1].getName().equals(ConstantsNames.SELF_ARGUMENT)
        : "Static method invocation must have first named argument named `self`";
  }

  /**
   * Call target of static method invocation must have first parameter named {@code self}. For
   * example, module methods cannot be call targets of static method invocation, because it cannot
   * have named {@code self} parameter.
   *
   * @return true if the first parameter of the method is named {@code self}, false otherwise.
   */
  private static boolean isValidStaticCallTarget(Function method) {
    var argInfos = method.getSchema().getArgumentInfos();
    if (argInfos.length > 0) {
      var name = argInfos[0].getName();
      return name != null && name.equals(ConstantsNames.SELF_ARGUMENT);
    }
    return false;
  }

  private static Object[] removeFirstArg(Object[] args) {
    var withoutFirst = new Object[args.length - 1];
    System.arraycopy(args, 1, withoutFirst, 0, withoutFirst.length);
    return withoutFirst;
  }

  private static CallArgumentInfo[] removeFirstArg(CallArgumentInfo[] args) {
    var withoutFirst = new CallArgumentInfo[args.length - 1];
    System.arraycopy(args, 1, withoutFirst, 0, withoutFirst.length);
    return withoutFirst;
  }
}
