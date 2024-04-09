package org.enso.interpreter.node.expression.builtin.meta;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.RootNode;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.Annotation;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.polyglot.debugger.IdExecutionService;

final class InstrumentorEvalNode extends RootNode {
  private static final FunctionSchema SUSPENDED_EVAL =
      new FunctionSchema(
          FunctionSchema.CallerFrameAccess.NONE,
          new ArgumentDefinition[] {
            new ArgumentDefinition(0, "expr", null, null, ArgumentDefinition.ExecutionMode.EXECUTE),
            new ArgumentDefinition(1, "info", null, null, ArgumentDefinition.ExecutionMode.EXECUTE)
          },
          new boolean[] {true, true},
          new CallArgumentInfo[0],
          new Annotation[0]);
  private static Reference<InstrumentorEvalNode> last = new WeakReference<>(null);

  private InstrumentorEvalNode(EnsoLanguage language) {
    super(language);
  }

  @TruffleBoundary
  static Function asSuspendedEval(
      EnsoLanguage language, Object expr, IdExecutionService.Info info) {
    var node = last.get();
    if (node == null || node.getLanguage(EnsoLanguage.class) != language) {
      node = new InstrumentorEvalNode(language);
      last = new WeakReference<>(node);
    }
    var call = node.getCallTarget();
    return new Function(call, null, SUSPENDED_EVAL, new Object[] {expr, info}, new Object[0]);
  }

  @Override
  public String getName() {
    return "Instrumentor.eval";
  }

  @Override
  public Object execute(VirtualFrame frame) {
    var args = Function.ArgumentsHelper.getPositionalArguments(frame.getArguments());
    try {
      var expr = InteropLibrary.getUncached().asString(args[0]);
      var info = (IdExecutionService.Info) args[1];
      return info.eval(expr);
    } catch (UnsupportedMessageException e) {
      throw EnsoContext.get(this).raiseAssertionPanic(this, null, e);
    }
  }
}
