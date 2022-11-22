package org.enso.interpreter.node.expression.debug;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.compiler.context.InlineContext;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.ClosureRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.scope.LocalScope;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.state.State;

/** Node running Enso expressions passed to it as strings. */
@NodeInfo(shortName = "Eval", description = "Evaluates code passed to it as string")
@ReportPolymorphism
public abstract class EvalNode extends BaseNode {
  private final boolean shouldCaptureResultScope;

  EvalNode(boolean shouldCaptureResultScope) {
    this.shouldCaptureResultScope = shouldCaptureResultScope;
  }

  /**
   * Creates an instance of this node.
   *
   * @return an instance of this node
   */
  public static EvalNode build() {
    return EvalNodeGen.create(false);
  }

  /**
   * Creates an instance of this node, with frame capture enabled.
   *
   * @return an instance of this node
   */
  public static EvalNode buildWithResultScopeCapture() {
    return EvalNodeGen.create(true);
  }

  /**
   * Executes a string expression in the given execution context
   *
   * @param callerInfo captured information about the execution context
   * @param state current value of the monadic state
   * @param expression the string containing expression to evaluate
   * @return the result of evaluating {@code expression} in the {@code callerInfo} context
   */
  public abstract Object execute(CallerInfo callerInfo, State state, Text expression);

  @CompilerDirectives.TruffleBoundary
  RootCallTarget parseExpression(LocalScope scope, ModuleScope moduleScope, String expression) {
    Context context = Context.get(this);
    LocalScope localScope = scope.createChild();
    InlineContext inlineContext =
        InlineContext.fromJava(
            localScope, moduleScope, getTailStatus(), context.getCompilerConfig());
    ExpressionNode expr =
        context.getCompiler().runInline(expression, inlineContext).getOrElse(null);
    if (expr == null) {
      throw new RuntimeException("Invalid code passed to `eval`: " + expression);
    }

    if (shouldCaptureResultScope) {
      expr = CaptureResultScopeNode.build(expr);
    }
    ClosureRootNode framedNode =
        ClosureRootNode.build(
            context.getLanguage(), localScope, moduleScope, expr, null, "<eval>", false, false);
    return Truffle.getRuntime().createCallTarget(framedNode);
  }

  @Specialization(
      guards = {
        "expression == cachedExpression",
        "callerInfo.getLocalScope() == cachedCallerInfo.getLocalScope()",
        "callerInfo.getModuleScope() == cachedCallerInfo.getModuleScope()",
      },
      limit = Constants.CacheSizes.EVAL_NODE)
  Object doCached(
      CallerInfo callerInfo,
      State state,
      Text expression,
      @Cached("expression") Text cachedExpression,
      @Cached("build()") ToJavaStringNode toJavaStringNode,
      @Cached("toJavaStringNode.execute(expression)") String expressionStr,
      @Cached("callerInfo") CallerInfo cachedCallerInfo,
      @Cached(
              "parseExpression(callerInfo.getLocalScope(), callerInfo.getModuleScope(), expressionStr)")
          RootCallTarget cachedCallTarget,
      @Cached("build()") ThunkExecutorNode thunkExecutorNode) {
    Function thunk = Function.thunk(cachedCallTarget, callerInfo.getFrame());
    return thunkExecutorNode.executeThunk(thunk, state, getTailStatus());
  }

  @Specialization
  Object doUncached(
      CallerInfo callerInfo,
      State state,
      Text expression,
      @Cached("build()") ThunkExecutorNode thunkExecutorNode,
      @Cached("build()") ToJavaStringNode toJavaStringNode) {
    RootCallTarget callTarget =
        parseExpression(
            callerInfo.getLocalScope(),
            callerInfo.getModuleScope(),
            toJavaStringNode.execute(expression));
    Function thunk = Function.thunk(callTarget, callerInfo.getFrame());
    return thunkExecutorNode.executeThunk(thunk, state, getTailStatus());
  }
}
