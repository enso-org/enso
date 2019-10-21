package org.enso.interpreter.node.callable;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.argument.ThunkExecutorNode;
import org.enso.interpreter.node.callable.argument.ThunkExecutorNodeGen;
import org.enso.interpreter.node.callable.argument.sorter.ArgumentSorterNode;
import org.enso.interpreter.node.callable.argument.sorter.ArgumentSorterNodeGen;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.callable.atom.Atom;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.MethodDoesNotExistException;
import org.enso.interpreter.runtime.error.NotInvokableException;
import org.enso.interpreter.runtime.type.TypesGen;

/**
 * This class is responsible for performing the actual invocation of a given callable with its
 * arguments.
 *
 * <p>This invocation is segmented via the type of the callable, allowing handling of the various
 * different kinds within a seamless framework.
 */
public abstract class InvokeCallableNode extends BaseNode {

  @Child private ArgumentSorterNode argumentSorter;
  @Child private MethodResolverNode methodResolverNode;
  @Child private ThunkExecutorNode thisExecutor;

  private final boolean canApplyThis;
  private final int thisArgumentPosition;

  private final ConditionProfile methodCalledOnNonAtom = ConditionProfile.createCountingProfile();

  /**
   * Creates a new instance of the node.
   *
   * @param schema a description of the arguments being applied to the callable
   * @param hasDefaultsSuspended whether or not the invocation has the callable's default arguments
   *     (if any) suspended
   */
  public InvokeCallableNode(CallArgumentInfo[] schema, boolean hasDefaultsSuspended) {
    boolean appliesThis = false;
    int idx = 0;
    for (; idx < schema.length; idx++) {
      CallArgumentInfo arg = schema[idx];

      boolean isNamedThis = arg.isNamed() && arg.getName().equals(Constants.THIS_ARGUMENT_NAME);
      if (arg.isPositional() || isNamedThis) {
        appliesThis = true;
        break;
      }
    }

    this.canApplyThis = appliesThis;
    this.thisArgumentPosition = idx;

    boolean argumentsArePreExecuted = false;
    this.argumentSorter =
        ArgumentSorterNodeGen.create(schema, hasDefaultsSuspended, argumentsArePreExecuted);
    this.methodResolverNode = MethodResolverNodeGen.create();
  }

  /**
   * Invokes a function directly on the arguments contained in this node.
   *
   * @param function the function to be executed
   * @param arguments the arguments to the function
   * @return the result of executing {@code callable} on the known arguments
   */
  @Specialization
  public Object invokeFunction(Function function, Object[] arguments) {
    return this.argumentSorter.execute(function, arguments);
  }

  /**
   * Invokes a constructor directly on the arguments contained in this node.
   *
   * @param constructor the constructor to be executed
   * @param arguments the arguments to the constructor
   * @return the result of executing {@code constructor} on the known arguments
   */
  @Specialization
  public Object invokeConstructor(AtomConstructor constructor, Object[] arguments) {
    return invokeFunction(constructor.getConstructorFunction(), arguments);
  }

  /**
   * Invokes a dynamic symbol after resolving it for the actual symbol for the {@code this}
   * argument.
   *
   * @param symbol the name of the requested symbol
   * @param arguments the arguments to the dynamic symbol
   * @return the result of resolving and executing the symbol for the {@code this} argument
   */
  @Specialization
  public Object invokeDynamicSymbol(UnresolvedSymbol symbol, Object[] arguments) {
    if (canApplyThis) {
      if (thisExecutor == null) {
        CompilerDirectives.transferToInterpreterAndInvalidate();
        thisExecutor = ThunkExecutorNodeGen.create(false);
      }

      Object selfArgument =
          thisExecutor.executeThunk(((Thunk) arguments[thisArgumentPosition]));

      if (methodCalledOnNonAtom.profile(TypesGen.isAtom(selfArgument))) {
        Atom self = (Atom) selfArgument;
        Function function = methodResolverNode.execute(symbol, self);
        return this.argumentSorter.execute(function, arguments);
      } else {
        throw new MethodDoesNotExistException(selfArgument, symbol.getName(), this);
      }
    } else {
      throw new RuntimeException("Currying without `this` argument is not yet supported.");
    }
  }

  /**
   * A fallback that should never be called.
   *
   * <p>If this is called, something has gone horribly wrong. It throws a {@link
   * NotInvokableException} to signal this.
   *
   * @param callable the callable to be executed
   * @param arguments the arguments to the callable
   * @return error
   */
  @Fallback
  public Object invokeGeneric(Object callable, Object[] arguments) {
    throw new NotInvokableException(callable, this);
  }

  /**
   * Executes the provided {@code callable} on the supplied {@code arguments}.
   *
   * @param callable the callable to evaluate
   * @param arguments the arguments to evaluate {@code callable} on
   * @return the result of executing {@code callable} on the supplied {@code arguments}
   */
  public abstract Object execute(Object callable, Object[] arguments);

  /**
   * Sets whether or not the current node is tail-recursive.
   *
   * @param isTail whether or not the node is tail-recursive.
   */
  @Override
  public void setTail(boolean isTail) {
    super.setTail(isTail);
    argumentSorter.setTail(isTail);
  }
}
