package org.enso.interpreter.node.callable.argument.sorter;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Fallback;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.ExplodeLoop;
import com.oracle.truffle.api.nodes.NodeInfo;
import java.util.Arrays;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.runtime.callable.Callable;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.error.NotInvokableException;

/**
 * This class represents the protocol for remapping the arguments provided at a call site into the
 * positional order expected by the definition of the {@link Callable}.
 */
@NodeInfo(shortName = "ArgumentSorter")
public abstract class ArgumentSorterNode extends BaseNode {
  private @CompilationFinal(dimensions = 1) CallArgumentInfo[] schema;
  private @CompilationFinal boolean isFullyPositional;

  /**
   * Creates a node that performs the argument organisation for the provided schema.
   *
   * @param schema information about the call arguments in positional order
   */
  public ArgumentSorterNode(CallArgumentInfo[] schema) {
    this.schema = schema;
    this.isFullyPositional = Arrays.stream(schema).allMatch(CallArgumentInfo::isPositional);
  }

  /**
   * Generates the argument mapping in the fully positional case.
   *
   * <p>This specialisation is executed when all of the arguments provided at the call-site are
   * specified in a purely positional fashion (no ignores and no named argument applications). This
   * is intended to be a very quick path.
   *
   * @param callable the callable to sort arguments for
   * @param arguments the arguments being passed to {@code callable}
   * @return the provided {@code} arguments in the order expected by {@code callable}
   */
  @Specialization(guards = "isFullyPositional()")
  public Object[] invokePositional(Object callable, Object[] arguments) {
    CompilerDirectives.ensureVirtualizedHere(arguments);
    return arguments;
  }

  /**
   * Generates the argument mapping where it has already been computed.
   *
   * <p>This specialisation is executed in the cases where the interpreter has already computed the
   * mapping necessary to reorder call-stite arguments into the order expected by the definition
   * site. It is also a fast path.
   *
   * <p>This specialisation can only execute when the {@link Callable} provided to the method
   * matches with the one stored in the cached argument sorter object.
   *
   * @param callable the callable to sort arguments for
   * @param arguments the arguments being passed to {@code callable}
   * @param mappingNode a cached node that tracks information about the mapping to enable a fast
   *     path
   * @return the provided {@code arguments} in the order expected by {@code callable}
   */
  @Specialization(guards = "mappingNode.hasSameCallable(callable)")
  @ExplodeLoop
  public Object[] invokeCached(
      Callable callable,
      Object[] arguments,
      @Cached("create(callable, getSchema())") CachedArgumentSorterNode mappingNode) {
    return mappingNode.execute(arguments, callable.getArgs().length);
  }

  /**
   * Generates the argument mapping freshly.
   *
   * <p>This specialisation is executed only when we cache miss, and will compute the argument
   * mapping freshly each time.
   *
   * @param callable the callable to sort arguments for
   * @param arguments the arguments being passed to {@code callable}
   * @param mappingNode a cached node that computes argument mappings freshly each time
   * @return the provided {@code arguments} in the order expected by {@code callable}
   */
  @Specialization
  public Object[] invokeUncached(
      Callable callable,
      Object[] arguments,
      @Cached("create(getSchema())") UncachedArgumentSorterNode mappingNode) {
    return mappingNode.execute(callable, arguments, callable.getArgs().length);
  }

  /**
   * A fallback that should never be called.
   *
   * <p>The only cases in which this specialisation can be called are system-wide error conditions,
   * and so we stop with a {@link NotInvokableException}.
   *
   * @param callable the callable to sort arguments for
   * @param arguments the arguments being passed to {@code callable}
   * @return error
   */
  @Fallback
  public Object[] invokeGeneric(Object callable, Object[] arguments) {
    throw new NotInvokableException(callable, this);
  }

  /**
   * Executes the {@link ArgumentSorterNode} to reorder the arguments.
   *
   * @param callable the callable to sort arguments for
   * @param arguments the arguments being passed to {@code callable}
   * @return the provided {@code arguments} in the order expected by {@code callable}
   */
  public abstract Object[] execute(Object callable, Object[] arguments);

  /**
   * Gets the schema for use in Truffle DSL guards.
   *
   * @return the argument schema
   */
  CallArgumentInfo[] getSchema() {
    return schema;
  }

  /**
   * Checks if the argument schema is fully positional for use in Truffle DLS guards.
   *
   * @return {@code true} if the arguments are all positional, otherwise {@false}
   */
  boolean isFullyPositional() {
    return isFullyPositional;
  }
}
