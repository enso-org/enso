package org.enso.interpreter.instrument;

import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.TruffleStackTrace;
import com.oracle.truffle.api.debug.DebuggerTags;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.EventContext;
import com.oracle.truffle.api.instrumentation.ExecutionEventNode;
import com.oracle.truffle.api.instrumentation.ExecutionEventNodeFactory;
import com.oracle.truffle.api.instrumentation.SourceSectionFilter;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.TruffleInstrument;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.InvalidArrayIndexException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.RootNode;
import java.io.IOException;
import java.io.OutputStream;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import org.enso.compiler.context.FramePointer;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.expression.builtin.debug.DebugBreakpointNode;
import org.enso.interpreter.node.expression.builtin.text.util.ToJavaStringNode;
import org.enso.interpreter.node.expression.debug.CaptureResultScopeNode;
import org.enso.interpreter.node.expression.debug.EvalNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.hash.HashMapToVectorNode;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.data.vector.ArrayLikeAtNode;
import org.enso.interpreter.runtime.data.vector.ArrayLikeLengthNode;
import org.enso.interpreter.runtime.state.State;
import org.enso.interpreter.runtime.warning.WarningsLibrary;
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
public final class ReplDebuggerInstrument extends TruffleInstrument {
  /** Option for {@link DebugServerInfo#ENABLE_OPTION} */
  private static final OptionKey<Boolean> ENABLE_OPTION = new OptionKey<>(false);

  /** Option for {@link DebugServerInfo#FN_OPTION} */
  private static final OptionKey<String> FN_OPTION = new OptionKey<>("");

  /**
   * Called by Truffle when this instrument is installed.
   *
   * @param env the instrumentation environment
   */
  @Override
  protected void onCreate(Env env) {
    DebuggerMessageHandler handler = new DebuggerMessageHandler();
    SourceSectionFilter filter = null;
    ExecutionEventNodeFactory factory = null;

    if (env.getOptions().get(ENABLE_OPTION)) {
      factory =
          ctx ->
              ctx.getInstrumentedNode() instanceof DebugBreakpointNode
                  ? new ReplExecutionEventNodeImpl(
                      null, ctx, handler, env.getLogger(ReplExecutionEventNodeImpl.class))
                  : null;

      filter = SourceSectionFilter.newBuilder().tagIs(DebuggerTags.AlwaysHalt.class).build();
      env.getInstrumenter().attachExecutionEventFactory(filter, factory);
    }
    if (env.getOptions().get(FN_OPTION) instanceof String replMethodName
        && !replMethodName.isEmpty()) {
      factory = new AtTheEndOfMethod(handler, env);

      filter =
          SourceSectionFilter.newBuilder()
              .tagIs(StandardTags.RootBodyTag.class)
              .rootNameIs(replMethodName::equals)
              .build();
      env.getInstrumenter().attachExecutionEventFactory(filter, factory);
    }

    if (factory != null || filter != null) {
      try {
        MessageEndpoint client = env.startServer(URI.create(DebugServerInfo.URI), handler);
        if (client != null) {
          handler.setClient(client);
        }
      } catch (MessageTransport.VetoException e) {
        env.getLogger(ReplDebuggerInstrument.class)
            .warning(
                "ReplDebuggerInstrument was initialized, "
                    + "but client connection has been vetoed");
      } catch (IOException e) {
        throw new RuntimeException(e);
      }
    }
  }

  @Override
  protected OptionDescriptors getOptionDescriptors() {
    var options = new ArrayList<OptionDescriptor>();
    options.add(OptionDescriptor.newBuilder(ENABLE_OPTION, DebugServerInfo.ENABLE_OPTION).build());
    options.add(OptionDescriptor.newBuilder(FN_OPTION, DebugServerInfo.FN_OPTION).build());
    return OptionDescriptors.create(options);
  }

  /** The actual node that's installed as a probe on any node the instrument was launched for. */
  private static class ReplExecutionEventNodeImpl extends ExecutionEventNode
      implements ReplExecutionEventNode {
    private @Child EvalNode evalNode = EvalNode.buildWithResultScopeCapture();
    private @Child ToJavaStringNode toJavaStringNode = ToJavaStringNode.build();

    private ReplExecutionEventNodeState nodeState;
    private State monadicState;

    private EventContext eventContext;
    private DebuggerMessageHandler handler;
    private TruffleLogger logger;
    private final OutputStream atExit;

    private ReplExecutionEventNodeImpl(
        OutputStream atExit,
        EventContext eventContext,
        DebuggerMessageHandler handler,
        TruffleLogger logger) {
      this.eventContext = eventContext;
      this.handler = handler;
      this.logger = logger;
      this.atExit = atExit;
    }

    private Object readValue(
        MaterializedFrame frame, FramePointer ptr, boolean onlyWarningsOrErrors) {
      var raw = getProperFrame(frame, ptr).getValue(ptr.frameSlotIdx());
      if (WarningsLibrary.getUncached().hasWarnings(raw)) {
        try {
          var sb = new StringBuilder();
          sb.append(WarningsLibrary.getUncached().removeWarnings(raw));
          var mappedWarnings = WarningsLibrary.getUncached().getWarnings(raw, true);
          var pairs = HashMapToVectorNode.getUncached().execute(mappedWarnings);
          var size = ArrayLikeLengthNode.getUncached().executeLength(pairs);
          for (var i = 0L; i < size; i++) {
            try {
              var pair = ArrayLikeAtNode.getUncached().executeAt(pairs, i);
              var value = ArrayLikeAtNode.getUncached().executeAt(pair, 1);
              sb.append("\n  ! ").append(value);
            } catch (InvalidArrayIndexException ex) {
              // go on
            }
          }
          return sb.toString();
        } catch (UnsupportedMessageException e) {
          // go on
        }
      }
      if (onlyWarningsOrErrors) {
        if (!InteropLibrary.getUncached().isException(raw)) {
          return null;
        }
      }
      return raw;
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
      return listBindings(false);
    }

    public Map<String, Object> listBindings(boolean onlyWarningsOrErrors) {
      Map<String, FramePointer> flatScope =
          nodeState.getLastScope().getLocalScope().flattenBindings();
      Map<String, Object> result = new HashMap<>();
      for (Map.Entry<String, FramePointer> entry : flatScope.entrySet()) {
        var valueOrNull =
            readValue(nodeState.getLastScope().getFrame(), entry.getValue(), onlyWarningsOrErrors);
        if (valueOrNull != null) {
          result.put(entry.getKey(), valueOrNull);
        }
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
      if (atExit == null) {
        CallerInfo lastScope = Function.ArgumentsHelper.getCallerInfo(frame.getArguments());
        Object lastReturn = EnsoContext.get(this).getNothing();
        // Note [Safe Access to State in the Debugger Instrument]
        monadicState = Function.ArgumentsHelper.getState(frame.getArguments());
        nodeState = new ReplExecutionEventNodeState(lastReturn, lastScope);
        startSessionImpl();
      }
    }

    @Override
    public void onReturnValue(VirtualFrame frame, Object result) {
      if (atExit != null) {
        startSession(getRootNode(), frame, result);
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

    private void startSession(RootNode root, VirtualFrame frame, Object toReturn) {
      CallerInfo lastScope = Function.ArgumentsHelper.getCallerInfo(frame.getArguments());
      if (lastScope == null && root instanceof EnsoRootNode enso) {
        lastScope =
            new CallerInfo(frame.materialize(), enso.getLocalScope(), enso.getModuleScope());
      }
      if (lastScope != null) {
        // Note [Safe Access to State in the Debugger Instrument]
        monadicState = Function.ArgumentsHelper.getState(frame.getArguments());
        nodeState = new ReplExecutionEventNodeState(toReturn, lastScope);
        startSessionImpl();
      }
    }

    @CompilerDirectives.TruffleBoundary
    private void startSessionImpl() {
      if (handler.hasClient()) {
        handler.startSession(this);
      } else {
        if (atExit == null) {
          logger.warning(
              "Debugger session starting, "
                  + "but no client connected, will terminate the session immediately");
        } else {
          for (var b : listBindings(true).entrySet()) {
            var line = b.getKey() + " = " + b.getValue() + "\n";
            try {
              atExit.write(line.getBytes(StandardCharsets.UTF_8));
            } catch (IOException ex) {
              logger.log(Level.SEVERE, line, ex);
            }
          }
        }
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

  private final class AtTheEndOfMethod implements ExecutionEventNodeFactory {
    private final DebuggerMessageHandler handler;
    private final TruffleInstrument.Env env;

    AtTheEndOfMethod(DebuggerMessageHandler h, TruffleInstrument.Env env) {
      this.handler = h;
      this.env = env;
    }

    @Override
    public ExecutionEventNode create(EventContext ctx) {
      var log = env.getLogger(ReplExecutionEventNodeImpl.class);
      return new ReplExecutionEventNodeImpl(env.err(), ctx, handler, log);
    }
  }
}
