package org.enso.interpreter.runtime.callable.function;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.node.callable.InteropApplicationNode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.data.Array;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.enso.interpreter.runtime.state.data.EmptyMap;
import org.enso.interpreter.runtime.type.Types;
import org.enso.polyglot.MethodNames;

/** A runtime representation of a function object in Enso. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
public final class Function implements TruffleObject {
  private final RootCallTarget callTarget;
  private final MaterializedFrame scope;
  private final FunctionSchema schema;
  private final @CompilerDirectives.CompilationFinal(dimensions = 1) Object[] preAppliedArguments;
  private final @CompilationFinal(dimensions = 1) Object[] oversaturatedArguments;

  /**
   * Creates a new function.
   *
   * @param callTarget the target containing the function's code
   * @param scope a frame representing the function's scope
   * @param schema the {@link FunctionSchema} with which the function was defined
   * @param preappliedArguments the preapplied arguments for this function. The layout of this array
   *     must be conforming to the {@code schema}. {@code null} is allowed if the function does not
   *     have any partially applied arguments.
   * @param oversaturatedArguments the oversaturated arguments this function may have accumulated.
   *     The layout of this array must be conforming to the {@code schema}. @{code null} is allowed
   *     if the function does not carry any oversaturated arguments.
   */
  public Function(
      RootCallTarget callTarget,
      MaterializedFrame scope,
      FunctionSchema schema,
      Object[] preappliedArguments,
      Object[] oversaturatedArguments) {
    this.callTarget = callTarget;
    this.scope = scope;
    this.schema = schema;
    this.preAppliedArguments = preappliedArguments;
    this.oversaturatedArguments = oversaturatedArguments;
  }

  /**
   * Creates a new function without any partially applied arguments.
   *
   * @param callTarget the target containing the function's code
   * @param scope a frame representing the function's scope
   * @param schema the {@link FunctionSchema} with which the function was defined
   */
  public Function(RootCallTarget callTarget, MaterializedFrame scope, FunctionSchema schema) {
    this(callTarget, scope, schema, null, null);
  }

  public static Function thunk(RootCallTarget callTarget, MaterializedFrame scope) {
    return new Function(callTarget, scope, FunctionSchema.THUNK);
  }

  /**
   * Creates a Function object from a {@link BuiltinRootNode} and argument definitions.
   *
   * @param node the {@link RootNode} for the function logic
   * @param args argument definitons
   * @return a Function object with specified behavior and arguments
   */
  public static Function fromBuiltinRootNode(BuiltinRootNode node, ArgumentDefinition... args) {
    RootCallTarget callTarget = Truffle.getRuntime().createCallTarget(node);
    FunctionSchema schema = new FunctionSchema(args);
    return new Function(callTarget, null, schema);
  }

  /**
   * Creates a Function object from a {@link BuiltinRootNode} and argument definitions.
   *
   * <p>The root node wrapped using this method can safely assume the {@link CallerInfo} argument
   * will be non-null.
   *
   * @param node the {@link RootNode} for the function logic
   * @param args argument definitons
   * @return a Function object with specified behavior and arguments
   */
  public static Function fromBuiltinRootNodeWithCallerFrameAccess(
      BuiltinRootNode node, ArgumentDefinition... args) {
    RootCallTarget callTarget = Truffle.getRuntime().createCallTarget(node);
    FunctionSchema schema = new FunctionSchema(FunctionSchema.CallerFrameAccess.FULL, args);
    return new Function(callTarget, null, schema);
  }

  /**
   * Gets the target containing the function's code.
   *
   * @return the target containing the function's code
   */
  public RootCallTarget getCallTarget() {
    return callTarget;
  }

  /** @return the name of this function. */
  public String getName() {
    return getCallTarget().getRootNode().getName();
  }

  /** @return the source section this function was defined in. */
  public SourceSection getSourceSection() {
    return getCallTarget().getRootNode().getSourceSection();
  }

  /**
   * Gets the function's scope.
   *
   * @return the function's scope
   */
  public MaterializedFrame getScope() {
    return scope;
  }

  /**
   * Gets the function's argument schema.
   *
   * @return the function's argument schema
   */
  public FunctionSchema getSchema() {
    return schema;
  }

  /**
   * Obtains the oversaturated arguments associated with this function.
   *
   * @return an array of this function's oversaturated arguments
   */
  public Object[] getOversaturatedArguments() {
    return oversaturatedArguments != null ? oversaturatedArguments : new Object[0];
  }

  /**
   * Gets a copy of the partially applied arguments for this function, safe to be mutated by
   * clients.
   *
   * @return a copy of the partially applied arguments for this function
   */
  public Object[] clonePreAppliedArguments() {
    return preAppliedArguments.clone();
  }

  /**
   * Checks if this runtime object is executable.
   *
   * @return {@code true}
   */
  @ExportMessage
  boolean isExecutable() {
    return true;
  }

  /**
   * A class representing the executable behaviour of the function.
   *
   * <p>This class gets exposed via the Truffle interop library to allow Enso functions to be called
   * from other guest languages running on GraalVM.
   */
  @ExportMessage
  abstract static class Execute {
    @Specialization
    static Object doCall(
        Function function,
        Object[] arguments,
        @Cached InteropApplicationNode interopApplicationNode) {
      return interopApplicationNode.execute(function, EmptyMap.create(), arguments);
    }
  }

  /**
   * Handles member invocation through the polyglot API.
   *
   * <p>The only supported member is {@code equals} checking for object identity.
   *
   * @param member the member name.
   * @param args arguments to pass to the execution.
   * @return the result of invoking the member.
   * @throws ArityException when an invalid number of arguments is passed to the member.
   * @throws UnknownIdentifierException when an invalid member is requested.
   */
  @ExportMessage
  @CompilerDirectives.TruffleBoundary
  Object invokeMember(String member, Object... args)
      throws ArityException, UnknownIdentifierException, UnsupportedTypeException {
    switch (member) {
      case MethodNames.Function.EQUALS:
        Object that = Types.extractArguments(args, Object.class);
        return this == that;
      case MethodNames.Function.GET_SOURCE_START:
        {
          SourceSection sect = getSourceSection();
          if (sect == null) {
            return null;
          }
          return sect.getCharIndex();
        }
      case MethodNames.Function.GET_SOURCE_LENGTH:
        {
          SourceSection sect = getSourceSection();
          if (sect == null) {
            return null;
          }
          return sect.getCharLength();
        }
    }
    throw UnknownIdentifierException.create(member);
  }

  /**
   * Verifies whether a member can be invoked through the polyglot API.
   *
   * @param member the member name.
   * @return {@code true} if the member can be invoked, {@code false} otherwise.
   */
  @ExportMessage
  boolean isMemberInvocable(String member) {
    return member.equals(MethodNames.Function.EQUALS);
  }

  /**
   * Marks the object as having members available for the polyglot API.
   *
   * @return {@code true}
   */
  @ExportMessage
  boolean hasMembers() {
    return true;
  }

  /**
   * Returns a collection of all members this object exposes through the polyglot API.
   *
   * <p>The only supported member is {@code equals}.
   *
   * @param includeInternal ignored
   * @return a collection of all supported member names.
   */
  @ExportMessage
  Object getMembers(boolean includeInternal) {
    return new Array(MethodNames.Function.EQUALS);
  }

  /**
   * Defines a simple schema for accessing arguments from call targets.
   *
   * <p>As Truffle call targets can only take a simple {@code Object[]}, this class provides a way
   * to get the various necessary pieces of information out of that array.
   */
  public static class ArgumentsHelper {

    /**
     * Generates an array of arguments using the schema to be passed to a call target.
     *
     * <p>The arguments passed to this function must be in positional order. For more information on
     * how to do this, see {@link InvokeFunctionNode}.
     *
     * @param function the function to be called
     * @param state the state to execute the function with
     * @param positionalArguments the arguments to that function, sorted into positional order
     * @return an array containing the necessary information to call an Enso function
     */
    public static Object[] buildArguments(
        Function function, CallerInfo callerInfo, Object state, Object[] positionalArguments) {
      return new Object[] {function.getScope(), callerInfo, state, positionalArguments};
    }

    /**
     * Generates an array of arguments using the schema to be passed to a call target.
     *
     * @param frame the frame becoming the lexical scope
     * @param state the state to execute the thunk with
     * @param positionalArguments the positional arguments to the call target
     * @return an array containing the necessary information to call an Enso function
     */
    public static Object[] buildArguments(
        MaterializedFrame frame, Object state, Object[] positionalArguments) {
      return new Object[] {frame, null, state, positionalArguments};
    }

    /**
     * Generates an array of arguments using the schema to be passed to a call target.
     *
     * @param thunk the thunk to be called
     * @param state the state to execute the thunk with
     * @return an array containing the necessary information to call an Enso thunk
     */
    public static Object[] buildArguments(Function thunk, Object state) {
      return new Object[] {thunk.getScope(), null, state, new Object[0]};
    }

    /**
     * Gets the positional arguments out of the array.
     *
     * @param arguments an array produced by {@link ArgumentsHelper#buildArguments(Function,
     *     CallerInfo, Object, Object[])}
     * @return the positional arguments to the function
     */
    public static Object[] getPositionalArguments(Object[] arguments) {
      return (Object[]) arguments[3];
    }

    /**
     * Gets the state out of the array.
     *
     * @param arguments an array produced by {@link
     *     ArgumentsHelper#buildArguments(Function,CallerInfo, Object, Object[])}
     * @return the state for the function
     */
    public static Object getState(Object[] arguments) {
      return arguments[2];
    }

    /**
     * Gets the caller info out of the array.
     *
     * <p>Any function using this method should declare {@link
     * FunctionSchema.CallerFrameAccess#FULL} in its schema for the result to be guaranteed
     * non-null.
     *
     * @param arguments an array produced by {@link ArgumentsHelper#buildArguments(Function,
     *     CallerInfo, Object, Object[])}
     * @return the caller info for the function
     */
    public static CallerInfo getCallerInfo(Object[] arguments) {
      return (CallerInfo) arguments[1];
    }

    /**
     * Gets the function's local scope out of the array.
     *
     * @param arguments an array produced by {@link ArgumentsHelper#buildArguments(Function,
     *     CallerInfo, Object, Object[])}
     * @return the local scope for the associated function
     */
    public static MaterializedFrame getLocalScope(Object[] arguments) {
      return (MaterializedFrame) arguments[0];
    }
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@CachedLibrary("this") TypesLibrary thisLib) {
    return Context.get(thisLib).getBuiltins().function();
  }

  public boolean isThunk() {
    return schema == FunctionSchema.THUNK;
  }

  public boolean isFullyApplied() {
    return schema.isFullyApplied();
  }
}
