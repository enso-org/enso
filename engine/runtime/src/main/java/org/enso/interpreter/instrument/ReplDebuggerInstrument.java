package org.enso.interpreter.instrument;

import com.oracle.truffle.api.debug.DebuggerTags;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.EventContext;
import com.oracle.truffle.api.instrumentation.ExecutionEventNode;
import com.oracle.truffle.api.instrumentation.Instrumenter;
import com.oracle.truffle.api.instrumentation.SourceSectionFilter;
import com.oracle.truffle.api.instrumentation.TruffleInstrument;

import java.io.IOException;
import java.net.URI;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.debug.CaptureResultScopeNode;
import org.enso.interpreter.node.expression.debug.EvalNode;
import org.enso.interpreter.runtime.Builtins;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.scope.FramePointer;
import org.enso.interpreter.runtime.state.Stateful;
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
@TruffleInstrument.Registration(
    id = DebugServerInfo.INSTRUMENT_NAME,
    services = ReplDebuggerInstrument.class)
public class ReplDebuggerInstrument extends TruffleInstrument {

  /**
   * Internal reference type to store session manager and get the current version on each execution
   * of this instrument.
   */
  private static class SessionManagerReference {
    private SessionManager sessionManager;

    /**
     * Create a new instance of this class
     *
     * @param sessionManager the session manager to initially store
     */
    private SessionManagerReference(SessionManager sessionManager) {
      this.sessionManager = sessionManager;
    }

    /**
     * Get the current session manager
     *
     * @return the current session manager
     */
    private SessionManager get() {
      return sessionManager;
    }

    /**
     * Set a new session manager for subsequent {@link #get()} calls.
     *
     * @param sessionManager the new session manager
     */
    private void set(SessionManager sessionManager) {
      this.sessionManager = sessionManager;
    }
  }

  /** An object controlling the execution of REPL.
   * Deprecated, will be removed in the next version.
   * Please use org.enso.polyglot.debugger.SessionManager.
   */
  public interface SessionManager {
    /**
     * Starts a new session with the provided execution node.
     *
     * @param executionNode the execution node that should be used for the duration of this session.
     */
    void startSession(ReplExecutionEventNode executionNode);
  }

  private SessionManagerReference sessionManagerReference =
      new SessionManagerReference(ReplExecutionEventNode::exit);

  /**
   * Called by Truffle when this instrument is installed.
   *
   * @param env the instrumentation environment
   */
  @Override
  protected void onCreate(Env env) {
    SourceSectionFilter filter =
        SourceSectionFilter.newBuilder().tagIs(DebuggerTags.AlwaysHalt.class)
            .build();
    env.registerService(this); // TODO [RW] this seems unnecessary after #791

    DebuggerMessageHandler handler = new DebuggerMessageHandler();
    try {
      MessageEndpoint client =
          env.startServer(URI.create(DebugServerInfo.URI), handler);
      if (client != null) {
        handler.setClient(client);
      } else {
        env.getLogger(ReplDebuggerInstrument.class)
            .warning("ReplDebuggerInstrument was initialized, " +
                "but no client connected");
      }
    } catch (MessageTransport.VetoException e) {
      env.getLogger(ReplDebuggerInstrument.class)
          .warning("ReplDebuggerInstrument was initialized, " +
              "but client connection has been vetoed");
    } catch (IOException e) {
      throw new RuntimeException(e);
    }

    // TODO [RW] in #791 move this inside try to not initialize the factory if
    //  there are no clients
    Instrumenter instrumenter = env.getInstrumenter();
    instrumenter.attachExecutionEventFactory(filter, ctx ->
        new ReplExecutionEventNode(ctx, sessionManagerReference, handler));

  }

  @Override
  protected OptionDescriptors getOptionDescriptors() {
    return OptionDescriptors.create(
        Collections.singletonList(
            OptionDescriptor.newBuilder(new OptionKey<>(""), DebugServerInfo.ENABLE_OPTION)
                .build()));
  }

  /**
   * Registers the session manager to use whenever this instrument is activated.
   *
   * @param sessionManager the session manager to use
   */
  public void setSessionManager(SessionManager sessionManager) {
    this.sessionManagerReference.set(sessionManager);
  }

  /** The actual node that's installed as a probe on any node the instrument was launched for. */
  public static class ReplExecutionEventNode extends ExecutionEventNode {
    private @Child EvalNode evalNode = EvalNode.buildWithResultScopeCapture();

    private Object lastReturn;
    private Object lastState;
    private CallerInfo lastScope;

    private EventContext eventContext;
    private SessionManagerReference sessionManagerReference;
    private DebuggerMessageHandler handler;

    private ReplExecutionEventNode(
        EventContext eventContext, SessionManagerReference sessionManagerReference, DebuggerMessageHandler handler) {
      this.eventContext = eventContext;
      this.sessionManagerReference = sessionManagerReference;
      this.handler = handler;
    }

    private Object getValue(MaterializedFrame frame, FramePointer ptr) {
      return getProperFrame(frame, ptr).getValue(ptr.getFrameSlot());
    }

    private MaterializedFrame getProperFrame(MaterializedFrame frame, FramePointer ptr) {
      MaterializedFrame currentFrame = frame;
      for (int i = 0; i < ptr.getParentLevel(); i++) {
        currentFrame = Function.ArgumentsHelper.getLocalScope(currentFrame.getArguments());
      }
      return currentFrame;
    }

    /**
     * Lists all the bindings available in the current execution scope.
     *
     * @return a map, where keys are variable names and values are current values of variables.
     */
    public Map<String, Object> listBindings() {
      Map<String, FramePointer> flatScope = lastScope.getLocalScope().flattenBindings();
      Map<String, Object> result = new HashMap<>();
      for (Map.Entry<String, FramePointer> entry : flatScope.entrySet()) {
        result.put(entry.getKey(), getValue(lastScope.getFrame(), entry.getValue()));
      }
      return result;
    }

    /**
     * Evaluates an arbitrary expression in the current execution context.
     *
     * @param expression the expression to evaluate
     * @return the result of evaluating the expression or an exception that
     *          caused failure
     */
    public Either<Exception, Object> evaluate(String expression) {
      try {
        Stateful result = evalNode.execute(lastScope, lastState, expression);
        lastState = result.getState();
        CaptureResultScopeNode.WithCallerInfo payload =
            (CaptureResultScopeNode.WithCallerInfo) result.getValue();
        lastScope = payload.getCallerInfo();
        lastReturn = payload.getResult();
        return new Right<>(lastReturn);
      } catch (Exception e) {
        return new Left<>(e);
      }
    }

    /**
     * Terminates this REPL session.
     *
     * <p>The last result of {@link #evaluate(String)} (or {@link Builtins#unit()} if {@link
     * #evaluate(String)} was not called before) will be returned from the instrumented node.
     *
     * <p>This function must always be called at the end of REPL session, as otherwise the program
     * will never resume. It's forbidden to use this object after exit has been called.
     */
    public void exit() {
      throw eventContext.createUnwind(lastReturn);
    }

    /**
     * Called by Truffle whenever this node starts execution.
     *
     * @param frame current execution frame
     */
    @Override
    protected void onEnter(VirtualFrame frame) {
      lastScope = Function.ArgumentsHelper.getCallerInfo(frame.getArguments());
      lastReturn = lookupContextReference(Language.class).get().getUnit().newInstance();
      // Note [Safe Access to State in the Debugger Instrument]
      lastState = Function.ArgumentsHelper.getState(frame.getArguments());
      startSession();
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
      return new Stateful(lastState, lastReturn);
    }

    private void startSession() {
      if (handler.hasClient()) {
        handler.startSession(this);
      } else if (sessionManagerReference.get() != null) {
        sessionManagerReference.get().startSession(this);
      }
    }
  }
}
