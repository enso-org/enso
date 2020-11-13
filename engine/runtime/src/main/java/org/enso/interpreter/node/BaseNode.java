package org.enso.interpreter.node;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;

/** A base type for all Enso language nodes. */
@NodeInfo(shortName = "Base", description = "A base node for the Enso AST")
@ReportPolymorphism
public abstract class BaseNode extends Node {
  private @CompilationFinal boolean isTail = false;
  private @CompilerDirectives.CompilationFinal FrameSlot stateFrameSlot;

  /**
   * Obtains the frame slot containing state variable for this node.
   *
   * @return The frame slot for state variable
   */
  protected FrameSlot getStateFrameSlot() {
    if (stateFrameSlot == null) {
      CompilerDirectives.transferToInterpreterAndInvalidate();
      stateFrameSlot = ((EnsoRootNode) getRootNode()).getStateFrameSlot();
    }
    return stateFrameSlot;
  }

  /**
   * Sets whether the node is tail-recursive.
   *
   * @param isTail whether or not the node is tail-recursive.
   */
  public void setTail(boolean isTail) {
    this.isTail = isTail;
  }

  /** Marks the node as tail-recursive. */
  public final void markTail() {
    setTail(true);
  }

  /** Marks the node as not tail-recursive. */
  public final void markNotTail() {
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
