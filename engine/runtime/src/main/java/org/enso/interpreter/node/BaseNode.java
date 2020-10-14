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
  public enum TailStatus {
    TAIL_DIRECT,
    TAIL_LOOP,
    NOT_TAIL
  }

  private @CompilationFinal TailStatus tailStatus = TailStatus.NOT_TAIL;
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

  public void setTailStatus(TailStatus tailStatus) {
    this.tailStatus = tailStatus;
  }

  public TailStatus getTailStatus() {
    return tailStatus;
  }
}
