package org.enso.interpreter.node.expression.builtin.debug;

import com.oracle.truffle.api.debug.DebuggerTags;
import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.GenerateWrapper;
import com.oracle.truffle.api.instrumentation.InstrumentableNode;
import com.oracle.truffle.api.instrumentation.ProbeNode;
import com.oracle.truffle.api.instrumentation.Tag;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.dsl.BuiltinMethod;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.state.State;

@BuiltinMethod(
    type = "Debug",
    name = "breakpoint",
    description = "Instrumentation marker node.",
    autoRegister = false)
@GenerateWrapper
public abstract class DebugBreakpointNode extends Node implements InstrumentableNode {
  private static final SourceSection DEBUG_SECTION;

  static {
    var src = Source.newBuilder("enso", "Debug.breakpoint", "Debug.enso").build();
    DEBUG_SECTION = src.createUnavailableSection();
  }

  /**
   * Creates a new instance of this node.
   *
   * @return a new instance of this node
   */
  public static DebugBreakpointNode build() {
    return DebugBreakpointNodeGen.create();
  }

  @Override
  public final SourceSection getSourceSection() {
    return DEBUG_SECTION;
  }

  /**
   * Tells Truffle this node is instrumentable.
   *
   * @return {@code true} – this node is always instrumentable.
   */
  @Override
  public final boolean isInstrumentable() {
    return true;
  }

  abstract Object execute(VirtualFrame frame, CallerInfo callerInfo, State state);

  @Specialization
  Object doExecute(VirtualFrame frame, CallerInfo callerInfo, State state) {
    return EnsoContext.get(this).getNothing();
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
  public final boolean hasTag(Class<? extends Tag> tag) {
    return tag == DebuggerTags.AlwaysHalt.class;
  }

  /**
   * Creates an instrumentable wrapper node for this node.
   *
   * @param probeNode the probe node to wrap
   * @return the wrapper instance wrapping both this and the probe node
   */
  @Override
  public final WrapperNode createWrapper(ProbeNode probeNode) {
    return new DebugBreakpointNodeWrapper(this, probeNode);
  }
}
