package org.enso.interpreter.instrument;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.frame.FrameInstance;
import com.oracle.truffle.api.frame.FrameInstanceVisitor;
import com.oracle.truffle.api.frame.MaterializedFrame;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.EventContext;
import com.oracle.truffle.api.instrumentation.ExecutionEventNode;
import com.oracle.truffle.api.instrumentation.ExecutionEventNodeFactory;
import com.oracle.truffle.api.instrumentation.SourceSectionFilter;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.TruffleInstrument;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.source.SourceSection;
import java.util.UUID;
import org.enso.interpreter.node.ClosureRootNode;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.node.expression.debug.EvalNode;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.CallerInfo;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.control.TailCallException;
import org.enso.interpreter.runtime.data.text.Text;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.error.PanicSentinel;
import org.enso.interpreter.runtime.state.State;
import org.enso.interpreter.runtime.tag.AvoidIdInstrumentationTag;
import org.enso.interpreter.runtime.tag.IdentifiedTag;
import org.enso.polyglot.debugger.IdExecutionService;

/** An instrument for getting values from AST-identified expressions. */
@TruffleInstrument.Registration(
    id = IdExecutionService.INSTRUMENT_ID,
    services = IdExecutionService.class)
public class IdExecutionInstrument extends TruffleInstrument implements IdExecutionService {

  private Env env;

  /**
   * Initializes the instrument. Substitute for a constructor, called by the Truffle framework.
   *
   * @param env the instrumentation environment
   */
  @Override
  protected void onCreate(Env env) {
    env.registerService(this);
    this.env = env;
  }

  /** Factory for creating new id event nodes. */
  private static class IdEventNodeFactory implements ExecutionEventNodeFactory {

    private final CallTarget entryCallTarget;
    private final Callbacks callbacks;
    private final Timer timer;

    private final EvalNode evalNode = EvalNode.build();

    /**
     * Creates a new event node factory.
     *
     * @param entryCallTarget the call target being observed.
     * @param callbacks communication with users
     * @param timer the timer for timing execution
     */
    IdEventNodeFactory(CallTarget entryCallTarget, Callbacks callbacks, Timer timer) {
      this.entryCallTarget = entryCallTarget;
      this.callbacks = callbacks;
      this.timer = timer;
    }

    @Override
    public ExecutionEventNode create(EventContext context) {
      return new IdExecutionEventNode(context);
    }

    /** Implementation of {@link Info} for the instrumented {@link Node}. */
    private final class NodeInfo extends Info {

      private final UUID nodeId;
      private final Object result;
      private final long elapsedTime;
      private final MaterializedFrame materializedFrame;
      private final EnsoRootNode ensoRootNode;

      /**
       * Create a {@link NodeInfo} for the entered node.
       *
       * @param materializedFrame the execution frame
       * @param node the entered node
       */
      public NodeInfo(
          MaterializedFrame materializedFrame,
          Node node) {
        super();

        this.nodeId = getNodeId(node);
        this.result = null;
        this.elapsedTime = -1;
        this.materializedFrame = materializedFrame;
        this.ensoRootNode = (EnsoRootNode) node.getRootNode();
      }

      /**
       * Create a {@link NodeInfo} for the executed node.
       *
       * @param nodeId the id of the executed node
       * @param result the result of the node execution
       * @param elapsedTime the execution time
       * @param materializedFrame the execution frame
       * @param node the executed node
       */
      public NodeInfo(
          UUID nodeId,
          Object result,
          long elapsedTime,
          MaterializedFrame materializedFrame,
          Node node) {
        super();

        this.nodeId = nodeId;
        this.result = result;
        this.elapsedTime = elapsedTime;
        this.materializedFrame = materializedFrame;
        this.ensoRootNode = (EnsoRootNode) node.getRootNode();
      }

      @Override
      public UUID getId() {
        return nodeId;
      }

      @Override
      public Object getResult() {
        return result;
      }

      @Override
      public boolean isPanic() {
        return result instanceof AbstractTruffleException && !(result instanceof DataflowError);
      }

      @Override
      public long getElapsedTime() {
        return elapsedTime;
      }

      @Override
      public Object eval(String code) {
        CallerInfo callerInfo =
            new CallerInfo(
                materializedFrame, ensoRootNode.getLocalScope(), ensoRootNode.getModuleScope());

        return evalNode.execute(callerInfo, State.create(EnsoContext.get(null)), Text.create(code));
      }

      private static UUID getNodeId(Node node) {
        return switch (node) {
          case ExpressionNode n -> n.getId();
          case FunctionCallInstrumentationNode n -> n.getId();
          case null -> null;
          default -> null;
        };
      }
    }

    /** The execution event node class used by this instrument. */
    private class IdExecutionEventNode extends ExecutionEventNode {

      private final EventContext context;
      private long nanoTimeElapsed = 0;

      /**
       * Creates a new event node.
       *
       * @param context location where the node is being inserted
       */
      IdExecutionEventNode(EventContext context) {
        this.context = context;
      }

      @Override
      public Object onUnwind(VirtualFrame frame, Object info) {
        return info;
      }

      @Override
      public void onEnter(VirtualFrame frame) {
        if (!isTopFrame(entryCallTarget)) {
          return;
        }

        Info info = new NodeInfo(frame.materialize(), context.getInstrumentedNode());
        Object result = callbacks.findCachedResult(info);

        if (result != null) {
          throw context.createUnwind(result);
        }
        nanoTimeElapsed = timer.getTime();
      }

      /**
       * Triggered when a node (either a function call sentry or an identified expression) finishes
       * execution.
       *
       * @param frame the current execution frame.
       * @param result the result of executing the node this method was triggered for.
       */
      @Override
      public void onReturnValue(VirtualFrame frame, Object result) {
        nanoTimeElapsed = timer.getTime() - nanoTimeElapsed;
        if (!isTopFrame(entryCallTarget)) {
          return;
        }
        Node node = context.getInstrumentedNode();

        if (node instanceof FunctionCallInstrumentationNode functionCallInstrumentationNode
            && result instanceof FunctionCallInstrumentationNode.FunctionCall) {
          Info info =
              new NodeInfo(
                  functionCallInstrumentationNode.getId(),
                  result,
                  nanoTimeElapsed,
                  frame.materialize(),
                  node);
          Object cachedResult = callbacks.onFunctionReturn(info);
          if (cachedResult != null) {
            throw context.createUnwind(cachedResult);
          }
        } else if (node instanceof ExpressionNode expressionNode) {
          Info info =
              new NodeInfo(
                  expressionNode.getId(), result, nanoTimeElapsed, frame.materialize(), node);
          callbacks.updateCachedResult(info);

          if (info.isPanic()) {
            throw context.createUnwind(result);
          }
        }
      }

      @Override
      public void onReturnExceptional(VirtualFrame frame, Throwable exception) {
        if (exception instanceof TailCallException) {
          onTailCallReturn(exception, Function.ArgumentsHelper.getState(frame.getArguments()));
        } else if (exception instanceof PanicException panicException) {
          onReturnValue(frame, new PanicSentinel(panicException, context.getInstrumentedNode()));
        } else if (exception instanceof AbstractTruffleException) {
          onReturnValue(frame, exception);
        }
      }

      @CompilerDirectives.TruffleBoundary
      private void onTailCallReturn(Throwable exception, State state) {
        try {
          TailCallException tailCallException = (TailCallException) exception;
          FunctionCallInstrumentationNode.FunctionCall functionCall =
              new FunctionCallInstrumentationNode.FunctionCall(
                  tailCallException.getFunction(), state, tailCallException.getArguments());
          Object result = InteropLibrary.getFactory().getUncached().execute(functionCall);
          onReturnValue(null, result);
        } catch (InteropException e) {
          throw new PanicException(Text.create(e.getMessage()), this);
        }
      }

      /**
       * Checks if we're not inside a recursive call, i.e. the {@link #entryCallTarget} only appears
       * in the stack trace once.
       *
       * @return {@code true} if it's not a recursive call, {@code false} otherwise.
       */
      private boolean isTopFrame(CallTarget entryCallTarget) {
        Object result =
            Truffle.getRuntime()
                .iterateFrames(
                    new FrameInstanceVisitor<Object>() {
                      boolean seenFirst = false;

                      @Override
                      public Object visitFrame(FrameInstance frameInstance) {
                        CallTarget ct = frameInstance.getCallTarget();
                        if (ct != entryCallTarget) {
                          return null;
                        }
                        if (seenFirst) {
                          return new Object();
                        } else {
                          seenFirst = true;
                          return null;
                        }
                      }
                    });
        return result == null;
      }
    }
  }

  /**
   * Attach a new event node factory to observe identified nodes within given function.
   *
   * @param mod module that contains the code
   * @param entryCallTarget the call target being observed.
   * @param callbacks the precomputed expression values.
   * @param timer the execution timer.
   * @return a reference to the attached event node factory.
   */
  @Override
  public EventBinding<ExecutionEventNodeFactory> bind(
      TruffleObject mod, CallTarget entryCallTarget, Callbacks callbacks, Object timer) {
    var module = (Module) mod;
    var builder =
        SourceSectionFilter.newBuilder()
            .tagIs(StandardTags.ExpressionTag.class, StandardTags.CallTag.class)
            .tagIs(IdentifiedTag.class)
            .tagIsNot(AvoidIdInstrumentationTag.class)
            .sourceIs(module::isModuleSource);

    if (entryCallTarget instanceof RootCallTarget r
        && r.getRootNode() instanceof ClosureRootNode c
        && c.getSourceSection() instanceof SourceSection section
        && section != null) {
      final int firstFunctionLine = section.getStartLine();
      final int afterFunctionLine = section.getEndLine() + 1;
      builder.lineIn(SourceSectionFilter.IndexRange.between(firstFunctionLine, afterFunctionLine));
    }
    var filter = builder.build();
    var factory = new IdEventNodeFactory(entryCallTarget, callbacks, (Timer) timer);
    return env.getInstrumenter().attachExecutionEventFactory(filter, factory);
  }
}
