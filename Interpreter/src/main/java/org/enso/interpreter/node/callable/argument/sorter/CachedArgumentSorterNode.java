package org.enso.interpreter.node.callable.argument.sorter;

import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.InvokeCallableNodeGen;
import org.enso.interpreter.node.callable.dispatch.CallOptimiserNode;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo.ArgumentMapping;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo.ArgumentMappingBuilder;
import org.enso.interpreter.runtime.callable.function.ArgumentSchema;
import org.enso.interpreter.runtime.callable.function.Function;

/**
 * This class handles the case where a mapping for reordering arguments to a given callable has
 * already been computed.
 */
@NodeInfo(shortName = "CachedArgumentSorter")
public class CachedArgumentSorterNode extends BaseNode {

  private final Function originalFunction;
  private final ArgumentMapping mapping;
  private final ArgumentSchema postApplicationSchema;
  private final boolean appliesFully;
  @Child private InvokeCallableNode oversaturatedCallableNode = null;

  /**
   * Creates a node that generates and then caches the argument mapping.
   *
   * @param function the function to sort arguments for
   * @param schema information on the calling argument
   * @param hasDefaultsSuspended whether or not the function to which these arguments are applied
   *     has its defaults suspended.
   */
  public CachedArgumentSorterNode(
      Function function, CallArgumentInfo[] schema, boolean hasDefaultsSuspended, boolean isTail) {
    this.setTail(isTail);
    this.originalFunction = function;
    ArgumentMappingBuilder mapping = ArgumentMappingBuilder.generate(function.getSchema(), schema);
    this.mapping = mapping.getAppliedMapping();
    this.postApplicationSchema = mapping.getPostApplicationSchema();

    boolean functionIsFullyApplied = true;
    for (int i = 0; i < postApplicationSchema.getArgumentsCount(); i++) {
      boolean hasValidDefault = postApplicationSchema.hasDefaultAt(i) && !hasDefaultsSuspended;
      boolean hasPreappliedArg = postApplicationSchema.hasPreAppliedAt(i);

      if (!(hasValidDefault || hasPreappliedArg)) {
        functionIsFullyApplied = false;
        break;
      }
    }
    appliesFully = functionIsFullyApplied;

    if (postApplicationSchema.hasOversaturatedArgs()) {
      oversaturatedCallableNode =
          InvokeCallableNodeGen.create(
              postApplicationSchema.getOversaturatedArguments(), hasDefaultsSuspended);
      oversaturatedCallableNode.setTail(isTail);
    }
  }

  /**
   * Creates a node that generates and then caches the argument mapping.
   *
   * @param function the function to sort arguments for
   * @param schema information on the calling arguments
   * @param hasDefaultsSuspended whether or not the default arguments are suspended for this
   *     function invocation
   * @param isTail whether or not this node is a tail call
   * @return a sorter node for the arguments in {@code schema} being passed to {@code callable}
   */
  public static CachedArgumentSorterNode create(
      Function function, CallArgumentInfo[] schema, boolean hasDefaultsSuspended, boolean isTail) {
    return new CachedArgumentSorterNode(function, schema, hasDefaultsSuspended, isTail);
  }

  /**
   * Reorders the provided arguments into the necessary order for the cached callable.
   *
   * @param function the function this node is reordering arguments for
   * @param arguments the arguments to reorder
   * @param optimiser a call optimiser node, capable of performing the actual function call
   * @return the provided {@code arguments} in the order expected by the cached {@link Function}
   */
  public Object execute(Function function, Object[] arguments, CallOptimiserNode optimiser) {
    Object[] mappedAppliedArguments;

    if (originalFunction.getSchema().hasAnyPreApplied()) {
      mappedAppliedArguments = function.clonePreAppliedArguments();
    } else {
      mappedAppliedArguments = new Object[this.postApplicationSchema.getArgumentsCount()];
    }

    mapping.reorderAppliedArguments(arguments, mappedAppliedArguments);

    if (this.appliesFully()) {
      if (!postApplicationSchema.hasOversaturatedArgs()) {
        if (this.isTail()) {
          throw new TailCallException(this.getOriginalFunction(), mappedAppliedArguments);
        } else {
          return optimiser.executeDispatch(this.getOriginalFunction(), mappedAppliedArguments);
        }
      } else {
        Object evaluatedVal =
            optimiser.executeDispatch(this.getOriginalFunction(), mappedAppliedArguments);

        return this.oversaturatedCallableNode.execute(
            evaluatedVal, generateOversaturatedArguments(function, arguments));
      }
    } else {
      return new Function(
          function.getCallTarget(),
          function.getScope(),
          this.getPostApplicationSchema(),
          mappedAppliedArguments,
          generateOversaturatedArguments(function, arguments));
    }
  }

  /**
   * Generates an array containing the oversaturated arguments for the function being executed (if
   * any).
   *
   * <p>It accounts for oversaturated arguments at the function call site, as well as any that have
   * been 'remembered' in the passed {@link Function} object.
   *
   * @param function the function being executed
   * @param arguments the arguments being applied to {@code function}
   * @return any oversaturated arguments on {@code function}
   */
  private Object[] generateOversaturatedArguments(Function function, Object[] arguments) {
    Object[] oversaturatedArguments =
        new Object[this.postApplicationSchema.getOversaturatedArguments().length];

    System.arraycopy(
        function.getOversaturatedArguments(),
        0,
        oversaturatedArguments,
        0,
        originalFunction.getSchema().getOversaturatedArguments().length);

    mapping.obtainOversaturatedArguments(
        arguments,
        oversaturatedArguments,
        originalFunction.getSchema().getOversaturatedArguments().length);

    return oversaturatedArguments;
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
