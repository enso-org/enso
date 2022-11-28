package org.enso.interpreter.node;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.dsl.ReportPolymorphism;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;

/** A base type for all Enso language nodes. */
@NodeInfo(shortName = "Base", description = "A base node for the Enso AST")
@ReportPolymorphism
public abstract class BaseNode extends Node {
  /** Represents the tail-position status of the node. */
  public enum TailStatus {
    /** Node is in a tail position, but not marked as a tail call. */
    TAIL_DIRECT,
    /** Node is in a tail position and marked as a tail call. */
    TAIL_LOOP,
    /** Node is not in a tail position. */
    NOT_TAIL;

    private static final int NUMBER_OF_VALUES = values().length;

    public static int numberOfValues() {
      return NUMBER_OF_VALUES;
    }
  }

  private @CompilationFinal TailStatus tailStatus = TailStatus.NOT_TAIL;

  /**
   * Sets the new tail position status for this node.
   *
   * @param tailStatus the new tail status.
   */
  public void setTailStatus(TailStatus tailStatus) {
    this.tailStatus = tailStatus;
  }

  /** @return the tail position status of this node. */
  public TailStatus getTailStatus() {
    return tailStatus;
  }
}
