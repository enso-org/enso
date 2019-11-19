package org.enso.interpreter.node.expression.debug;

import com.oracle.truffle.api.debug.DebuggerTags;
import com.oracle.truffle.api.dsl.CachedContext;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.GenerateWrapper;
import com.oracle.truffle.api.instrumentation.InstrumentableNode;
import com.oracle.truffle.api.instrumentation.ProbeNode;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.Builtins;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.state.Stateful;

/** A base node serving as an instrumentable marker. */
@NodeInfo(description = "Instrumentation marker node.")
@GenerateWrapper
public abstract class BreakpointNode extends Node implements InstrumentableNode {

  /**
   * Tells Truffle this node is instrumentable.
   *
   * @return {@code true} – this node is always instrumentable.
   */
  @Override
  public boolean isInstrumentable() {
    return true;
  }

  /**
   * Execute this node. Does not do anything interesting, the default implementation returns {@link
   * Builtins#unit()}. The behavior and return value of this node are assumed to be injected by
   * attached instruments.
   *
   * @param frame current execution frame
   * @param state current value of the monadic state
   * @return the result of executing this node
   */
  public abstract Stateful execute(VirtualFrame frame, Object state);

  @Specialization
  Stateful execute(
      VirtualFrame frame, Object state, @CachedContext(Language.class) Context context) {
    return new Stateful(state, context.getUnit().newInstance());
  }

  /**
   * Creates a new instance of this node.
   *
   * @return a new instance of this node
   */
  public static BreakpointNode build() {
    return BreakpointNodeGen.create();
  }

  /**
   * Informs Truffle about the provided tags.
   *
   * <p>This node only provides the {@link DebuggerTags.AlwaysHalt} tag.
   *
   * @param tag the tag to verify
   * @return {@code true} if the tag is {@link DebuggerTags.AlwaysHalt}, {@code false} otherwise
   */
  @Override
  public boolean hasTag(Class<? extends Tag> tag) {
    return tag == DebuggerTags.AlwaysHalt.class;
  }

  /**
   * Creates an instrumentable wrapper node for this node.
   *
   * @param probeNode the probe node to wrap
   * @return the wrapper instance wrapping both this and the probe node
   */
  @Override
  public WrapperNode createWrapper(ProbeNode probeNode) {
    return new BreakpointNodeWrapper(this, probeNode);
  }
}
