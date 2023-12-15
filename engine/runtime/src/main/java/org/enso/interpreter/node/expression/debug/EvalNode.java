package org.enso.interpreter.node.expression.debug;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.dsl.Cached;
import com.oracle.truffle.api.dsl.Cached.Shared;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.context.LocalScope;
import org.enso.interpreter.Constants;
import org.enso.interpreter.node.BaseNode;
import org.enso.interpreter.node.ClosureRootNode;
import org.enso.interpreter.node.callable.thunk.ThunkExecutorNode;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.IrToTruffle;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.text.Text;
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
    EnsoContext context = EnsoContext.get(this);
    LocalScope localScope = scope.createChild();
    var compiler = context.getCompiler();
    InlineContext inlineContext =
        InlineContext.fromJava(
            localScope,
            moduleScope.getModule().asCompilerModule(),
            scala.Option.apply(getTailStatus() != TailStatus.NOT_TAIL),
            context.getCompilerConfig(),
            scala.Option.apply(compiler.packageRepository()));

    var tuppleOption = compiler.runInline(expression, inlineContext);
    if (tuppleOption.isEmpty()) {
      throw new RuntimeException("Invalid code passed to `eval`: " + expression);
    }
    var newInlineContext = tuppleOption.get()._1();
    var ir = tuppleOption.get()._2();
    var src = tuppleOption.get()._3();

    var sco = newInlineContext.localScope().getOrElse(LocalScope::root);
    var mod = newInlineContext.module$access$0().module$access$0();

    var m = org.enso.interpreter.runtime.Module.fromCompilerModule(mod);
    var toTruffle =
        new IrToTruffle(context, src, m.getScope(), compiler.org$enso$compiler$Compiler$$config);
    var expr = toTruffle.runInline(ir, sco, "<inline_source>");

    if (shouldCaptureResultScope) {
      expr = CaptureResultScopeNode.build(expr);
    }
    ClosureRootNode framedNode =
        ClosureRootNode.build(
            context.getLanguage(), localScope, moduleScope, expr, null, "<eval>", false, false);
    return framedNode.getCallTarget();
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
      @Shared("toJavaStringNode") @Cached("build()") ToJavaStringNode toJavaStringNode,
      @Cached("toJavaStringNode.execute(expression)") String expressionStr,
      @Cached("callerInfo") CallerInfo cachedCallerInfo,
      @Cached(
              "parseExpression(callerInfo.getLocalScope(), callerInfo.getModuleScope(),"
                  + " expressionStr)")
          RootCallTarget cachedCallTarget,
      @Shared("thunkExecutorNode") @Cached("build()") ThunkExecutorNode thunkExecutorNode) {
    Function thunk = Function.thunk(cachedCallTarget, callerInfo.getFrame());
    return thunkExecutorNode.executeThunk(callerInfo.getFrame(), thunk, state, getTailStatus());
  }

  @Specialization
  Object doUncached(
      CallerInfo callerInfo,
      State state,
      Text expression,
      @Shared("thunkExecutorNode") @Cached("build()") ThunkExecutorNode thunkExecutorNode,
      @Shared("toJavaStringNode") @Cached("build()") ToJavaStringNode toJavaStringNode) {
    RootCallTarget callTarget =
        parseExpression(
            callerInfo.getLocalScope(),
            callerInfo.getModuleScope(),
            toJavaStringNode.execute(expression));
    Function thunk = Function.thunk(callTarget, callerInfo.getFrame());
    return thunkExecutorNode.executeThunk(callerInfo.getFrame(), thunk, state, getTailStatus());
  }
}
