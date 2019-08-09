package org.enso.interpreter.node.callable.argument.sorter;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.type.TypesGen;

/**
 * This class handles the case where a mapping for reordering arguments to a given callable has
 * already been computed.
 */
@NodeInfo(shortName = "CachedArgumentSorter")
public class CachedArgumentSorterNode extends BaseNode {
  private @CompilationFinal Object callable;
  private @CompilationFinal(dimensions = 1) int[] mapping;
  private final ConditionProfile otherIsAtomCons = ConditionProfile.createCountingProfile();
  private final ConditionProfile otherIsFunction = ConditionProfile.createCountingProfile();
  private @CompilationFinal boolean callableIsFunction;

  /**
   * Creates a node that generates and then caches the argument mapping.
   *
   * @param callable the callable to sort arguments for
   * @param schema information on the calling arguments
   */
  public CachedArgumentSorterNode(Object callable, CallArgumentInfo[] schema) {
    this.callable = callable;
    this.mapping = CallArgumentInfo.generateArgMapping(callable, schema);

    this.callableIsFunction = TypesGen.isFunction(callable);
    CompilerAsserts.compilationConstant(callableIsFunction);
  }

  /**
   * Creates a node that generates and then caches the argument mapping.
   *
   * @param callable the callable to sort arguments for
   * @param schema information on the calling arguments
   * @return a sorter node for the arguments in {@code schema} being passed to {@code callable}
   */
  public static CachedArgumentSorterNode create(Object callable, CallArgumentInfo[] schema) {
    return new CachedArgumentSorterNode(callable, schema);
  }

  /**
   * Reorders the provided arguments into the necessary order for the cached callable.
   *
   * @param arguments the arguments to reorder
   * @param numArgsDefinedForCallable the number of arguments that the cached callable was defined
   *     for
   * @return the provided {@code arguments} in the order expected by the cached {@link
   *     org.enso.interpreter.runtime.callable.Callable}
   */
  @ExplodeLoop
  public Object[] execute(Object[] arguments, int numArgsDefinedForCallable) {
    return CallArgumentInfo.reorderArguments(this.mapping, arguments, numArgsDefinedForCallable);
  }

  /**
   * Determines if the provided callable is the same as the cached one.
   *
   * @param other the callable to check for equality
   * @return {@code true} if {@code other} matches the cached callable, otherwise {@code false}
   */
  public boolean hasSameCallable(Object other) {
    if (otherIsAtomCons.profile(TypesGen.isAtomConstructor(other))) {
      return this.callable == other;
    } else if (this.callableIsFunction) {
      if (otherIsFunction.profile(TypesGen.isFunction(other))) {
        return ((Function) this.callable).getCallTarget() == ((Function) other).getCallTarget();
      }
    }
    return false;
  }
}
