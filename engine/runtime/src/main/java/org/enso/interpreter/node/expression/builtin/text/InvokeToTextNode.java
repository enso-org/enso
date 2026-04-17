package org.enso.interpreter.node.expression.builtin.text;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.dsl.NeverDefault;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.enso.compiler.core.ConstantsNames;
import org.enso.interpreter.node.callable.InteropMethodCallNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.data.EnsoMultiValue;

/**
 * Converts provided object into Text. This is a generic node to be used to convert any object into
 * Text. It does so by calling {@code to_text} method or by special means for various builtin
 * objects.
 */
public final class InvokeToTextNode extends Node {
  @CompilerDirectives.CompilationFinal private UnresolvedSymbol toText;
  @Child private InteropMethodCallNode methodNode;
  @Child private InvokeCallableNode invokeCallableNode;
  @Child private AnyToTextNode anyToText;
  private final boolean isUncached;

  private InvokeToTextNode(boolean isUncached) {
    this.isUncached = isUncached;
  }

  @NeverDefault
  public static InvokeToTextNode create() {
    return new InvokeToTextNode(false);
  }

  @NeverDefault
  public static InvokeToTextNode getUncached() {
    return new InvokeToTextNode(true);
  }

  /**
   * Converts an object to its textual representation.
   *
   * @param frame frame to invoke {@code to_text} at - it can be {@code null}
   * @param obj the object to compute textual representation for
   * @return the textual representation of the {@code obj}
   */
  public Object executeToText(VirtualFrame frame, Object obj) {
    if (obj instanceof EnsoMultiValue emv) {
      return executeMultiValue(emv);
    }
    if (frame == null || isUncached()) {
      return executeToTextNoFrame(obj);
    } else {
      return executeWithFrame(frame, obj);
    }
  }

  private Object executeToTextNoFrame(Object obj) {
    if (methodNode == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      methodNode =
          insert(
              isUncached() ? InteropMethodCallNode.getUncached() : InteropMethodCallNode.build());
    }
    return methodNode.executeOrPanic(ensureToTextSymbol(), obj);
  }

  private UnresolvedSymbol ensureToTextSymbol() {
    if (toText == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      var textScope = Builtins.get(this).textExtensions().getDefinitionScope();
      toText = UnresolvedSymbol.build(ConstantsNames.TO_TEXT, textScope);
    }
    return toText;
  }

  private Object executeWithFrame(VirtualFrame frame, Object obj) {
    if (invokeCallableNode == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      invokeCallableNode = insert(buildInvokeCallableNode());
    }
    var ctx = EnsoContext.get(this);
    var state = ctx.currentState();
    return invokeCallableNode.execute(ensureToTextSymbol(), frame, state, new Object[] {obj});
  }

  @NeverDefault
  InvokeCallableNode buildInvokeCallableNode() {
    return InvokeCallableNode.build(
        new CallArgumentInfo[] {new CallArgumentInfo()},
        InvokeCallableNode.DefaultsExecutionMode.EXECUTE,
        InvokeCallableNode.ArgumentsExecutionMode.PRE_EXECUTED);
  }

  private Object executeMultiValue(EnsoMultiValue emv) {
    if (anyToText == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      anyToText = insert(isUncached() ? AnyToTextNode.getUncached() : AnyToTextNode.build());
    }
    return anyToText.execute(emv);
  }

  private boolean isUncached() {
    return isUncached;
  }
}
