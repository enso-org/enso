package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.Node;
import java.util.UUID;
import org.enso.compiler.core.ConstantsNames;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.resolver.MethodResolverNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.state.State;
import org.enso.interpreter.runtime.warning.WarningsLibrary;

/** */
abstract class InvokeMethodNode extends BaseNode {
  protected static final int CACHE_SIZE = 10;
  protected final int argumentCount;
  protected final boolean onBoundary;
  protected final InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode;
  protected final InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode;

  /**
   * Creates a new node for method invocation.
   *
   * @param schema a description of the arguments being applied to the callable
   * @param defaultsExecutionMode the defaulted arguments handling mode for this call
   * @param argumentsExecutionMode the arguments execution mode for this call
   * @param onBoundary shall we emit plain {@code PanicException} or also attach {@code
   *     UnknownIdentifierException} cause
   * @return a new invoke method node
   */
  public static InvokeMethodNode build(
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      int thisArgumentPosition,
      boolean onBoundary) {
    if (isStaticMethodInvocation(schema)) {
      return StaticInvokeMethodNodeGen.create(
          schema,
          defaultsExecutionMode,
          argumentsExecutionMode,
          namedSelfArgPosition(schema),
          onBoundary);
    } else {
      return InstanceInvokeMethodNodeGen.create(
          schema, defaultsExecutionMode, argumentsExecutionMode, thisArgumentPosition, onBoundary);
    }
  }

  private static boolean isStaticMethodInvocation(CallArgumentInfo[] schema) {
    return namedSelfArgPosition(schema) >= 0;
  }

  private static int namedSelfArgPosition(CallArgumentInfo[] schema) {
    for (int i = 0; i < schema.length; i++) {
      var arg = schema[i];
      if (arg.isNamed() && arg.getName().equals(ConstantsNames.SELF_ARGUMENT)) {
        return i;
      }
    }
    return -1;
  }

  InvokeMethodNode(
      CallArgumentInfo[] schema,
      InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode,
      InvokeCallableNode.ArgumentsExecutionMode argumentsExecutionMode,
      boolean onBoundary) {
    this.argumentCount = schema.length;
    this.onBoundary = onBoundary;
    this.defaultsExecutionMode = defaultsExecutionMode;
    this.argumentsExecutionMode = argumentsExecutionMode;
  }

  public abstract Object execute(
      VirtualFrame frame, State state, UnresolvedSymbol symbol, Object self, Object[] arguments);

  /**
   * Sets the expression ID of this node.
   *
   * @param id the expression ID to assign this node.
   */
  public abstract void setId(UUID id);

  public static Function resolveFunction(
      UnresolvedSymbol symbol, Object self, Type selfTpe, MethodResolverNode methodResolverNode) {
    Function function = methodResolverNode.executeResolution(selfTpe, symbol);
    return function;
  }

  static PanicException methodNotFound(
      Node where, boolean onBoundary, UnresolvedSymbol symbol, Object self) throws PanicException {
    var cause = onBoundary ? UnknownIdentifierException.create(symbol.getName()) : null;
    var ctx = EnsoContext.get(where);
    var payload = ctx.getBuiltins().error().makeNoSuchMethod(self, symbol);
    throw new PanicException(ctx, payload, cause, where);
  }

  static PanicException methodNotInvocable(
      Node where, UnresolvedSymbol symbol, Object self, Throwable cause) throws PanicException {
    var ctx = EnsoContext.get(where);
    var payload = ctx.getBuiltins().error().makeNotInvokable(symbol);
    throw new PanicException(ctx, payload, cause, where);
  }

  /**
   * Resolves symbol to a Warning method, if possible.
   *
   * <p>A regular method dispatch logic will extract/append warnings of `self` before invoking the
   * actual method dispatch logic. This allows for ignoring complexity related to the presence of
   * warnings but prevents us from manipulating warnings directly in the Enso code (they have just
   * been removed). `resolveWarningFunction` will attempt to resolve the symbol in the Warning type
   * scope, if possible. Additionally, we check if under the non-warning `self`, the symbol would
   * resolve to `Any`. If not, it means that we should employ a regular method dispatch logic, due
   * to a method name clash. E.g. if some collection type Foo defines a `has_warnings` method, we
   * should dispatch the call to `Foo`'s `has_warning` rather than to a `Warning` or `Any`'s `one.
   *
   * @param self `self` argument that has some warnings
   * @param symbol symbol to be resolved
   * @param types TypesLibrary instance
   * @param warnings WarningsLibrary instance
   * @return resolved Warning method to be called
   */
  Function resolveWarningFunction(
      Object self, UnresolvedSymbol symbol, TypesLibrary types, WarningsLibrary warnings) {
    Object selfWithoutWarnings;
    try {
      selfWithoutWarnings = warnings.removeWarnings(self);
    } catch (UnsupportedMessageException e) {
      CompilerDirectives.transferToInterpreter();
      var ctx = EnsoContext.get(this);
      throw ctx.raiseAssertionPanic(
          this,
          "`self` object should have some warnings when calling `" + symbol.getName() + "` method",
          e);
    }

    var selfType = types.getType(selfWithoutWarnings);
    var fnAndType = symbol.resolveFor(this, selfType);
    var builtins = EnsoContext.get(this).getBuiltins();
    if (fnAndType != null && fnAndType.type() == builtins.any()) {
      return symbol
          .getScope()
          .lookupMethodDefinition(builtins.warning().getEigentype(), symbol.getName());
    }
    return null;
  }
}
