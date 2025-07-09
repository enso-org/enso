package org.enso.interpreter.runtime.callable.function;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Bind;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Idempotent;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.library.CachedLibrary;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.profiles.InlinedBranchProfile;
import com.oracle.truffle.api.source.SourceSection;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import org.enso.interpreter.node.callable.InteropApplicationNode;
import org.enso.interpreter.node.callable.dispatch.InvokeFunctionNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.node.expression.builtin.BuiltinRootNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.function.FunctionSchema.CallerFrameAccess;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.library.dispatch.TypesLibrary;
import org.slf4j.LoggerFactory;

/** A runtime representation of a function object in Enso. */
@ExportLibrary(InteropLibrary.class)
@ExportLibrary(TypesLibrary.class)
public final class Function extends EnsoObject {
  private final RootCallTarget callTarget;
  private final MaterializedFrame scope;
  private final FunctionSchema schema;
  private final @CompilationFinal(dimensions = 1) Object[] preAppliedArguments;
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
   * Helper method to construct a function with pre-applied arguments from any call target.
   *
   * @param callTarget the call target to invoke
   * @param args the arguments to pass to the call target
   * @return fully saturated function ready to be processed by {@link ThunkExecutorNode}
   */
  public static Function fullyApplied(RootCallTarget callTarget, Object... args) {
    var defs = new ArgumentDefinition[args.length];
    var appl = new boolean[args.length];
    for (var i = 0; i < args.length; i++) {
      defs[i] =
          new ArgumentDefinition(i, null, null, null, ArgumentDefinition.ExecutionMode.EXECUTE);
      appl[i] = true;
    }
    var schema = FunctionSchema.newBuilder().argumentDefinitions(defs).hasPreapplied(appl).build();
    return new Function(callTarget, null, schema, args, new Object[0]);
  }

  /**
   * Creates a Function object from a {@link BuiltinRootNode} and argument definitions.
   *
   * @param node the {@link RootNode} for the function logic
   * @param args argument definitons
   * @return a Function object with specified behavior and arguments
   */
  public static Function fromBuiltinRootNode(BuiltinRootNode node, ArgumentDefinition... args) {
    RootCallTarget callTarget = node.getCallTarget();
    FunctionSchema schema = FunctionSchema.newBuilder().argumentDefinitions(args).build();
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
    RootCallTarget callTarget = node.getCallTarget();
    FunctionSchema schema =
        FunctionSchema.newBuilder()
            .argumentDefinitions(args)
            .callerFrameAccess(CallerFrameAccess.FULL)
            .build();
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

  /**
   * @return the name of this function.
   */
  @TruffleBoundary
  public String getName() {
    return getCallTarget().getRootNode().getName();
  }

  @ExportMessage
  boolean hasSourceLocation() {
    return getCallTarget().getRootNode().getSourceSection() != null;
  }

  /**
   * @return the source section this function was defined in.
   */
  @TruffleBoundary
  @ExportMessage(name = "getSourceLocation")
  public SourceSection getSourceSection() throws UnsupportedMessageException {
    var section = getCallTarget().getRootNode().getSourceSection();
    if (section == null) {
      throw UnsupportedMessageException.create();
    } else {
      return section;
    }
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
  @Idempotent
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
   * Gets the partially applied arguments for this function.
   *
   * @return a read only array of the partially applied arguments
   */
  public Object[] getPreAppliedArguments() {
    return preAppliedArguments;
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

  @ExportMessage
  boolean hasExecutableName() {
    return this.getName() != null;
  }

  @ExportMessage
  String getExecutableName() {
    return this.getName();
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
        @Cached InteropApplicationNode interopApplicationNode,
        @CachedLibrary("function") InteropLibrary thisLib,
        @Cached InlinedBranchProfile panicProfile) {
      try {
        return interopApplicationNode.execute(
            function, EnsoContext.get(thisLib).currentState(), arguments);
      } catch (StackOverflowError err) {
        CompilerDirectives.transferToInterpreter();
        var asserts = false;
        assert asserts = true;
        var logger = LoggerFactory.getLogger(Function.class);
        if (asserts) {
          logger.error("StackOverflowError detected", err);
        } else {
          logger.debug("StackOverflowError detected", err);
        }
        throw err;
      } catch (PanicException ex) {
        panicProfile.enter(thisLib);
        // materialize the exception message
        ex.getMessage();
        throw ex;
      }
    }
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
     * @param positionalArguments the arguments to that function, sorted into positional order
     * @return an array containing the necessary information to call an Enso function
     */
    public static Object[] buildArguments(
        Function function, CallerInfo callerInfo, Object[] positionalArguments) {
      return new Object[] {function.getScope(), callerInfo, positionalArguments};
    }

    /**
     * Generates an array of arguments using the schema to be passed to a call target.
     *
     * @param frame the frame becoming the lexical scope
     * @param positionalArguments the positional arguments to the call target
     * @return an array containing the necessary information to call an Enso function
     */
    public static Object[] buildArguments(MaterializedFrame frame, Object[] positionalArguments) {
      return new Object[] {frame, null, positionalArguments};
    }

    /**
     * Generates an array of arguments using the schema to be passed to a call target.
     *
     * @param thunk the thunk to be called
     * @param state the state to execute the thunk with
     * @return an array containing the necessary information to call an Enso thunk
     */
    public static Object[] buildArguments(Function thunk) {
      return new Object[] {thunk.getScope(), null, new Object[0]};
    }

    /**
     * Gets the positional arguments out of the array.
     *
     * @param arguments an array produced by {@link ArgumentsHelper#buildArguments(Function,
     *     CallerInfo, Object, Object[])}
     * @return the positional arguments to the function
     */
    public static Object[] getPositionalArguments(Object[] arguments) {
      return (Object[]) arguments[2];
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
  Type getMetaObject(@CachedLibrary("this") InteropLibrary thisLib) {
    return EnsoContext.get(thisLib).getBuiltins().function();
  }

  @ExportMessage
  boolean hasMetaObject() {
    return true;
  }

  @ExportMessage
  boolean hasType() {
    return true;
  }

  @ExportMessage
  Type getType(@Bind Node node) {
    return EnsoContext.get(node).getBuiltins().function();
  }

  public boolean isThunk() {
    return schema == FunctionSchema.THUNK;
  }

  public boolean isFullyApplied() {
    return schema.isFullyApplied();
  }

  @ExportMessage
  @Override
  public String toDisplayString(boolean sideEffects) {
    return toString();
  }

  @Override
  public String toString() {
    return toString(true);
  }

  @CompilerDirectives.TruffleBoundary
  public final String toString(boolean includeArguments) {
    var iop = InteropLibrary.getUncached();
    var sb = new StringBuilder();
    var n = callTarget.getRootNode();
    sb.append(n.getName());
    var ss = n.getSourceSection();
    if (ss != null) {
      var src = ss.getSource();
      var start = ss.getStartLine();
      var end = ss.getEndLine();
      sb.append("[").append(src.getName()).append(":").append(start);
      if (end == start) {
        sb.append(":").append(ss.getStartColumn()).append("-").append(ss.getEndColumn());
      } else {
        sb.append("-").append(end);
      }
      sb.append("]");
    }
    if (includeArguments) {
      Consumer<String> pending =
          (name) -> {
            sb.append(" ").append(name).append("=_");
          };
      BiConsumer<String, Object> preapplied =
          (name, arg) -> {
            sb.append(" ").append(name).append("=");
            sb.append(iop.toDisplayString(arg, false));
          };
      BiConsumer<String, Object> oversaturated =
          (name, arg) -> {
            sb.append(" +").append(name).append("=");
            sb.append(iop.toDisplayString(arg, false));
          };
      iterateArguments(pending, null, preapplied, oversaturated);
    }
    return sb.toString();
  }

  /**
   * Iterates over function arguments while sorting them into categories and reporting their actual
   * values when available. Any of the arguments can be {@code null} when info about such a category
   * isn't needed.
   *
   * @param pending names of arguments that still need to be applied before the function is invoked
   *     are provided to this constumer
   * @param defaulted names of arguments with default values (without the value itself as it is not
   *     computed yet) are provided to this consumer. These arguments may or may not be provided in
   *     order to invoke the function
   * @param preapplied reports argument name and its associated value that has already been applied
   *     and will be used when the function is invoked
   * @param oversaturated reports over-saturated argument name with a value which will be applied to
   *     the result of the function invocation, when the function is invoked
   */
  @CompilerDirectives.TruffleBoundary
  public final void iterateArguments(
      Consumer<String> pending,
      Consumer<String> defaulted,
      BiConsumer<String, Object> preapplied,
      BiConsumer<String, Object> oversaturated) {
    if (pending == null) {
      pending = this::ignore;
    }
    if (defaulted == null) {
      defaulted = this::ignore;
    }
    if (preapplied == null) {
      preapplied = this::ignore;
    }
    if (oversaturated == null) {
      oversaturated = this::ignore;
    }
    for (var i = 0; i < schema.getArgumentsCount(); i++) {
      var info = schema.getArgumentInfos()[i];
      var name = info.getName();
      if (preAppliedArguments != null && preAppliedArguments[i] != null) {
        preapplied.accept(name, preAppliedArguments[i]);
      } else {
        if (info.hasDefaultValue()) {
          defaulted.accept(name);
        } else {
          pending.accept(name);
        }
      }
    }
    if (schema.getOversaturatedArguments() != null) {
      for (var i = 0; i < schema.getOversaturatedArguments().length; i++) {
        if (oversaturatedArguments != null && oversaturatedArguments[i] != null) {
          oversaturated.accept(
              schema.getOversaturatedArguments()[i].getName(), oversaturatedArguments[i]);
        }
      }
    }
  }

  private void ignore(String ignore1) {}

  private void ignore(String ignore1, Object ignore2) {}
}
