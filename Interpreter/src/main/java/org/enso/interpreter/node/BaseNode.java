package org.enso.interpreter.node;

import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;

/** A base type for all Enso language nodes. */
@NodeInfo(shortName = "Base", description = "A base node for the Enso AST")
@ReportPolymorphism
public class BaseNode extends Node {
  private @CompilationFinal boolean isTail = false;

  /**
   * Sets whether the node is tail-recursive.
   *
   * @param isTail whether or not the node is tail-recursive.
   */
  public void setTail(boolean isTail) {
    this.isTail = isTail;
  }

  /**
   * Marks the node as tail-recursive.
   */
  final public void markTail() {
    setTail(true);
  }

  /**
   * Marks the node as not tail-recursive.
   */
  final public void markNotTail() {
    setTail(false);
  }

  /**
   * Checks if the node is tail-recursive.
   *
   * @return {@code true} if the node is tail-recursive, otherwise {@code false}
   */
  public boolean isTail() {
    return isTail;
  }
}
