package org.enso.interpreter.node.callable.argument.sorter;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.ArgumentSchema;
import org.enso.interpreter.runtime.callable.function.Function;

/**
 * This class handles the case where a mapping for reordering arguments to a given callable has
 * already been computed.
 */
@NodeInfo(shortName = "CachedArgumentSorter")
public class CachedArgumentSorterNode extends BaseNode {
  private final Function originalFunction;
  private final @CompilationFinal(dimensions = 1) int[] mapping;
  private final ArgumentSchema postApplicationSchema;
  private final boolean appliesFully;

  /**
   * Creates a node that generates and then caches the argument mapping.
   *
   * @param function the function to sort arguments for
   * @param schema information on the calling arguments
   */
  public CachedArgumentSorterNode(Function function, CallArgumentInfo[] schema) {
    this.originalFunction = function;
    CallArgumentInfo.ArgumentMapping mapping =
        CallArgumentInfo.ArgumentMapping.generate(function.getSchema(), schema);
    this.mapping = mapping.getAppliedMapping();
    this.postApplicationSchema = mapping.getPostApplicationSchema();

    boolean fullApplication = true;
    for (int i = 0; i < postApplicationSchema.getArgumentsCount(); i++) {
      if (!(postApplicationSchema.hasDefaultAt(i) || postApplicationSchema.hasPreAppliedAt(i))) {
        fullApplication = false;
        break;
      }
    }
    appliesFully = fullApplication;
  }

  /**
   * Creates a node that generates and then caches the argument mapping.
   *
   * @param function the function to sort arguments for
   * @param schema information on the calling arguments
   * @return a sorter node for the arguments in {@code schema} being passed to {@code callable}
   */
  public static CachedArgumentSorterNode create(Function function, CallArgumentInfo[] schema) {
    return new CachedArgumentSorterNode(function, schema);
  }

  /**
   * Reorders the provided arguments into the necessary order for the cached callable.
   *
   * @param function the function this node is reordering arguments for
   * @param arguments the arguments to reorder
   * @return the provided {@code arguments} in the order expected by the cached {@link Function}
   */
  public Object[] execute(Function function, Object[] arguments) {
    Object[] result;
    if (originalFunction.getSchema().hasAnyPreApplied()) {
      result = function.clonePreAppliedArguments();
    } else {
      result = new Object[this.postApplicationSchema.getArgumentsCount()];
    }
    CallArgumentInfo.reorderArguments(this.mapping, arguments, result);
    return result;
  }

  /**
   * Determines if the provided function is the same as the cached one.
   *
   * @param other the function to check for equality
   * @return {@code true} if {@code other} matches the cached function, otherwise {@code false}
   */
  public boolean isCompatible(Function other) {
    return originalFunction.getSchema() == other.getSchema();
  }

  /**
   * Checks whether this node's operation results in a fully saturated function call.
   *
   * @return {@code true} if the call is fully saturated, {@code false} otherwise.
   */
  public boolean appliesFully() {
    return appliesFully;
  }

  /**
   * Returns the {@link ArgumentSchema} to use in case the function call is not fully saturated.
   *
   * @return the call result {@link ArgumentSchema}.
   */
  public ArgumentSchema getPostApplicationSchema() {
    return postApplicationSchema;
  }

  /**
   * Returns the function this node was created for.
   *
   * @return the function this node was created for.
   */
  public Function getOriginalFunction() {
    return originalFunction;
  }
}
