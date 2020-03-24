package org.enso.interpreter.node.expression.debug;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.compiler.InlineContext;
import org.enso.interpreter.Constants;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.ClosureRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.argument.Thunk;
import org.enso.interpreter.runtime.scope.LocalScope;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.state.Stateful;
import scala.Some;

/** Node running Enso expressions passed to it as strings. */
@NodeInfo(shortName = "Eval", description = "Evaluates code passed to it as string")
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
  public abstract Stateful execute(CallerInfo callerInfo, Object state, String expression);

  RootCallTarget parseExpression(LocalScope scope, ModuleScope moduleScope, String expression) {
    LocalScope localScope = scope.createChild();
    Language language = lookupLanguageReference(Language.class).get();
    InlineContext inlineContext = InlineContext.fromJava(localScope, moduleScope, isTail());
    ExpressionNode expr =
        lookupContextReference(Language.class)
            .get()
            .compiler()
            .runInline(expression, inlineContext)
            .getOrElse(null);
    if (expr == null) {
      throw new RuntimeException("Invalid code passed to `eval`: " + expression);
    }

    if (shouldCaptureResultScope) {
      expr = CaptureResultScopeNode.build(expr);
    }
    ClosureRootNode framedNode =
        ClosureRootNode.build(
            lookupLanguageReference(Language.class).get(),
            localScope,
            moduleScope,
            expr,
            null,
            "<dynamic_eval>");
    return Truffle.getRuntime().createCallTarget(framedNode);
  }

  @Specialization(
      guards = {
        "expression == cachedExpression",
        "callerInfo.getLocalScope() == cachedCallerInfo.getLocalScope()",
        "callerInfo.getModuleScope() == cachedCallerInfo.getModuleScope()",
      },
      limit = Constants.CacheSizes.EVAL_NODE)
  Stateful doCached(
      CallerInfo callerInfo,
      Object state,
      String expression,
      @Cached("expression") String cachedExpression,
      @Cached("callerInfo") CallerInfo cachedCallerInfo,
      @Cached(
              "parseExpression(callerInfo.getLocalScope(), callerInfo.getModuleScope(), expression)")
          RootCallTarget cachedCallTarget,
      @Cached("build(isTail())") ThunkExecutorNode thunkExecutorNode) {
    Thunk thunk = new Thunk(cachedCallTarget, callerInfo.getFrame());
    return thunkExecutorNode.executeThunk(thunk, state);
  }

  @Specialization
  Stateful doUncached(
      CallerInfo callerInfo,
      Object state,
      String expression,
      @Cached("build(isTail())") ThunkExecutorNode thunkExecutorNode) {
    RootCallTarget callTarget =
        parseExpression(callerInfo.getLocalScope(), callerInfo.getModuleScope(), expression);
    Thunk thunk = new Thunk(callTarget, callerInfo.getFrame());
    return thunkExecutorNode.executeThunk(thunk, state);
  }
}
