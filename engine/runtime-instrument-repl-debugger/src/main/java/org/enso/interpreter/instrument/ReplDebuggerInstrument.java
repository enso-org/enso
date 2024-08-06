package org.enso.interpreter.instrument;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.TruffleStackTrace;
import com.oracle.truffle.api.debug.DebuggerTags;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.EventContext;
import com.oracle.truffle.api.instrumentation.ExecutionEventNode;
import com.oracle.truffle.api.instrumentation.ExecutionEventNodeFactory;
import com.oracle.truffle.api.instrumentation.Instrumenter;
import com.oracle.truffle.api.instrumentation.SourceSectionFilter;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.TruffleInstrument;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.nodes.RootNode;
import java.io.IOException;
import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.enso.compiler.context.FramePointer;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.expression.builtin.debug.DebugBreakpointNode;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.node.expression.debug.CaptureResultScopeNode;
import org.enso.interpreter.node.expression.debug.EvalNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.state.State;
import org.enso.polyglot.debugger.DebugServerInfo;
import org.graalvm.options.OptionDescriptor;
import org.graalvm.options.OptionDescriptors;
import org.graalvm.options.OptionKey;
import org.graalvm.polyglot.io.MessageEndpoint;
import org.graalvm.polyglot.io.MessageTransport;
import scala.util.Either;
import scala.util.Left;
import scala.util.Right;

/** The Instrument implementation for the interactive debugger REPL. */
@TruffleInstrument.Registration(id = DebugServerInfo.INSTRUMENT_NAME)
public class ReplDebuggerInstrument extends TruffleInstrument {
  /**
   * Called by Truffle when this instrument is installed.
   *
   * @param env the instrumentation environment
   */
  @Override
  protected void onCreate(Env env) {
    SourceSectionFilter filter =
        SourceSectionFilter.newBuilder().tagIs(DebuggerTags.AlwaysHalt.class).build();

    DebuggerMessageHandler handler = new DebuggerMessageHandler();
    try {
      MessageEndpoint client = env.startServer(URI.create(DebugServerInfo.URI), handler);
      if (client != null) {
        handler.setClient(client);
        Instrumenter instrumenter = env.getInstrumenter();
        instrumenter.attachExecutionEventFactory(
            filter,
            ctx ->
                ctx.getInstrumentedNode() instanceof DebugBreakpointNode
                    ? new ReplExecutionEventNodeImpl(
                        true, ctx, handler, env.getLogger(ReplExecutionEventNodeImpl.class))
                    : null);
        var lastReplChance = new AtTheEndOfFirstMethod(handler, env, instrumenter);
        lastReplChance.activate();
      } else {
        env.getLogger(ReplDebuggerInstrument.class)
            .warning("ReplDebuggerInstrument was initialized, " + "but no client connected");
      }
    } catch (MessageTransport.VetoException e) {
      env.getLogger(ReplDebuggerInstrument.class)
          .warning(
              "ReplDebuggerInstrument was initialized, " + "but client connection has been vetoed");
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  protected OptionDescriptors getOptionDescriptors() {
    return OptionDescriptors.create(
        Collections.singletonList(
            OptionDescriptor.newBuilder(new OptionKey<>(""), DebugServerInfo.ENABLE_OPTION)
                .build()));
  }

  /** The actual node that's installed as a probe on any node the instrument was launched for. */
  private static class ReplExecutionEventNodeImpl extends ExecutionEventNode
      implements ReplExecutionEventNode {
    private final boolean atEnter;
    private @Child EvalNode evalNode = EvalNode.buildWithResultScopeCapture();
    private @Child ToJavaStringNode toJavaStringNode = ToJavaStringNode.build();

    private ReplExecutionEventNodeState nodeState;
    private State monadicState;

    private EventContext eventContext;
    private DebuggerMessageHandler handler;
    private TruffleLogger logger;

    private ReplExecutionEventNodeImpl(
        boolean atEnter,
        EventContext eventContext,
        DebuggerMessageHandler handler,
        TruffleLogger logger) {
      this.atEnter = atEnter;
      this.eventContext = eventContext;
      this.handler = handler;
      this.logger = logger;
    }

    private Object getValue(MaterializedFrame frame, FramePointer ptr) {
      return getProperFrame(frame, ptr).getValue(ptr.frameSlotIdx());
    }

    private MaterializedFrame getProperFrame(MaterializedFrame frame, FramePointer ptr) {
      MaterializedFrame currentFrame = frame;
      for (int i = 0; i < ptr.parentLevel(); i++) {
        currentFrame = Function.ArgumentsHelper.getLocalScope(currentFrame.getArguments());
      }
      return currentFrame;
    }

    @Override
    public Map<String, Object> listBindings() {
      Map<String, FramePointer> flatScope =
          nodeState.getLastScope().getLocalScope().flattenBindings();
      Map<String, Object> result = new HashMap<>();
      for (Map.Entry<String, FramePointer> entry : flatScope.entrySet()) {
        result.put(entry.getKey(), getValue(nodeState.getLastScope().getFrame(), entry.getValue()));
      }
      return result;
    }

    @Override
    public Either<Exception, Object> evaluate(String expression) {
      ReplExecutionEventNodeState savedState = nodeState;
      try {
        CaptureResultScopeNode.WithCallerInfo payload =
            (CaptureResultScopeNode.WithCallerInfo)
                evalNode.execute(nodeState.getLastScope(), monadicState, Text.create(expression));
        CallerInfo lastScope = payload.getCallerInfo();
        Object lastReturn = payload.getResult();
        nodeState = new ReplExecutionEventNodeState(lastReturn, lastScope);
        return new Right<>(formatObject(lastReturn));
      } catch (Exception e) {
        nodeState = savedState;
        TruffleStackTrace.fillIn(e);
        return new Left<>(e);
      }
    }

    @Override
    public Either<Exception, String> showObject(Object object) {
      try {
        InteropLibrary interop = InteropLibrary.getUncached();
        return new Right<>(interop.asString(interop.toDisplayString(object)));
      } catch (Exception e) {
        return new Left<>(e);
      }
    }

    private Object formatObject(Object o) {
      if (o instanceof Text) {
        return toJavaStringNode.execute((Text) o);
      } else {
        return o;
      }
    }

    @Override
    public void exit() {
      throw eventContext.createUnwind(nodeState.getLastReturn());
    }

    /**
     * Called by Truffle whenever this node starts execution.
     *
     * @param frame current execution frame
     */
    @Override
    protected void onEnter(VirtualFrame frame) {
      if (atEnter) {
        startSession(getRootNode(), frame);
      }
    }

    @Override
    public void onReturnValue(VirtualFrame frame, Object result) {
      if (!atEnter) {
        startSession(getRootNode(), frame);
      }
    }

    @Override
    public void onReturnExceptional(VirtualFrame frame, Throwable exception) {
      if (!atEnter) {
        startSession(getRootNode(), frame);
      }
    }

    /* Note [Safe Access to State in the Debugger Instrument]
     * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     * This is safe to do as we ensure that the instrument's `onEnter` is always called as the
     * first instruction of the function that it's observing.
     */

    /**
     * Called by Truffle whenever an unwind {@see {@link EventContext#createUnwind(Object)}} was
     * thrown in the course of REPL execution.
     *
     * <p>We use this mechanism to inject the REPL-returned value back into caller code.
     *
     * @param frame current execution frame
     * @param info The unwind's payload. Currently unused.
     * @return the object that will become the instrumented node's return value
     */
    @Override
    protected Object onUnwind(VirtualFrame frame, Object info) {
      return nodeState.getLastReturn();
    }

    private void startSession(RootNode root, VirtualFrame frame) {
      CallerInfo lastScope = Function.ArgumentsHelper.getCallerInfo(frame.getArguments());
      if (lastScope == null && root instanceof EnsoRootNode enso) {
        lastScope =
            new CallerInfo(frame.materialize(), enso.getLocalScope(), enso.getModuleScope());
      }
      if (lastScope != null) {
        var lastReturn = EnsoContext.get(this).getNothing();
        // Note [Safe Access to State in the Debugger Instrument]
        monadicState = Function.ArgumentsHelper.getState(frame.getArguments());
        nodeState = new ReplExecutionEventNodeState(lastReturn, lastScope);
        startSessionImpl();
      }
    }

    @CompilerDirectives.TruffleBoundary
    private void startSessionImpl() {
      if (handler.hasClient()) {
        handler.startSession(this);
      } else {
        logger.warning(
            "Debugger session starting, "
                + "but no client connected, will terminate the session immediately");
        exit();
      }
    }

    /**
     * State of the execution node.
     *
     * <p>As the execution nodes are reused by Truffle, the nested nodes share state. If execution
     * of a nested node fails, to ensure consistent state of the parent node, its state has to be
     * restored.
     */
    private static class ReplExecutionEventNodeState {
      private final Object lastReturn;
      private final CallerInfo lastScope;

      ReplExecutionEventNodeState(Object lastReturn, CallerInfo lastScope) {
        this.lastReturn = lastReturn;
        this.lastScope = lastScope;
      }

      Object getLastReturn() {
        return lastReturn;
      }

      CallerInfo getLastScope() {
        return lastScope;
      }
    }
  }

  private final class AtTheEndOfFirstMethod implements ExecutionEventNodeFactory {
    private final Instrumenter instr;
    private final DebuggerMessageHandler handler;
    private final TruffleInstrument.Env env;
    private EventBinding<?> firstMethod;

    AtTheEndOfFirstMethod(DebuggerMessageHandler h, TruffleInstrument.Env env, Instrumenter instr) {
      this.instr = instr;
      this.handler = h;
      this.env = env;
    }

    final void activate() {
      var anyMethod =
          SourceSectionFilter.newBuilder().tagIs(StandardTags.RootBodyTag.class).build();
      this.firstMethod = instr.attachExecutionEventFactory(anyMethod, this);
    }

    @Override
    public ExecutionEventNode create(EventContext ctx) {
      firstMethod.dispose();
      return new ReplExecutionEventNodeImpl(
          false, ctx, handler, env.getLogger(ReplExecutionEventNodeImpl.class));
    }
  }
}
