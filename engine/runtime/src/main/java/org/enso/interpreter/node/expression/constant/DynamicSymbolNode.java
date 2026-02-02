package org.enso.interpreter.node.expression.constant;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.UnresolvedConstructor;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.data.EnsoObject;
import org.enso.polyglot.RuntimeID;

/** Simple constant node that always results in the same {@link UnresolvedSymbol}. */
@NodeInfo(shortName = "DynamicSym")
public class DynamicSymbolNode extends ExpressionNode {
  private final EnsoObject unresolvedSymbol;
  private @CompilerDirectives.CompilationFinal RuntimeID id = null;

  private DynamicSymbolNode(EnsoObject unresolvedSymbol) {
    this.unresolvedSymbol = unresolvedSymbol;
  }

  private DynamicSymbolNode(String unresolvedConstructor) {
    this.unresolvedSymbol = UnresolvedConstructor.build(this, unresolvedConstructor);
  }

  /**
   * Creates an instance of this node.
   *
   * @param symbol the symbol to be resolved
   * @return a node representing the dynamic lookup of {@code symbol}
   */
  public static DynamicSymbolNode build(UnresolvedSymbol symbol) {
    return new DynamicSymbolNode(symbol);
  }

  public static DynamicSymbolNode buildUnresolvedConstructor(String symbol) {
    return new DynamicSymbolNode(symbol);
  }

  /**
   * Gets the dynamic symbol from the node.
   *
   * @param frame the stack frame for execution
   * @return the constant {@link UnresolvedSymbol}
   */
  @Override
  public Object executeGeneric(VirtualFrame frame) {
    return unresolvedSymbol;
  }

  @Override
  public RuntimeID getId() {
    return this.id;
  }

  @Override
  public void setId(RuntimeID id) {
    CompilerDirectives.transferToInterpreterAndInvalidate();
    this.id = id;
  }

  @Override
  public String toString() {
    return "DynamicSymbolNode(symbol=" + unresolvedSymbol + ")";
  }
}
