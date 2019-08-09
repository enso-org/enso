package org.enso.interpreter.node.callable.argument.sorter;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.profiles.ConditionProfile;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.NotInvokableException;
import org.enso.interpreter.runtime.type.TypesGen;

/**
 * This class handles the case where we have no cached mapping for a given callable's arguments, and
 * will compute it on the fly.
 */
@NodeInfo(shortName = "UncachedArgumentSorter")
public class UncachedArgumentSorterNode extends BaseNode {
  private @CompilationFinal(dimensions = 1) CallArgumentInfo[] schema;
  private final ConditionProfile isCallableProfile = ConditionProfile.createCountingProfile();

  /**
   * Creates a node to sort arguments on the fly.
   *
   * @param schema information on the calling arguments
   */
  public UncachedArgumentSorterNode(CallArgumentInfo[] schema) {
    this.schema = schema;
  }

  /**
   * Creates a node to sort arguments on the fly.
   *
   * @param schema information on the calling arguments
   * @return a sorter node for the arguments in {@code schema} for an unknown callable
   */
  public static UncachedArgumentSorterNode create(CallArgumentInfo[] schema) {
    return new UncachedArgumentSorterNode(schema);
  }

  /**
   * Reorders the provided {@code arguments} for the given {@link
   * org.enso.interpreter.runtime.callable.Callable}.
   *
   * @param callable the callable to reorder arguments for
   * @param arguments the arguments passed to {@code callable}
   * @param numArgsDefinedForCallable the number of arguments defined on {@code callable}
   * @return the provided {@code arguments} in the order expected by the cached {@link
   *     org.enso.interpreter.runtime.callable.Callable}
   */
  public Object[] execute(Object callable, Object[] arguments, int numArgsDefinedForCallable) {
    if (isCallableProfile.profile(TypesGen.isCallable(callable))) {
      Function actualCallable = (Function) callable;
      int[] order = CallArgumentInfo.generateArgMapping(actualCallable, this.schema);
      return CallArgumentInfo.reorderArguments(order, arguments, numArgsDefinedForCallable);
    } else {
      throw new NotInvokableException(callable, this);
    }
  }
}
