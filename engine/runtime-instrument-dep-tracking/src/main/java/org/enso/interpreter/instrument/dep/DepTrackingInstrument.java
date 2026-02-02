package org.enso.interpreter.instrument.dep;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.frame.FrameInstance;
import com.oracle.truffle.api.frame.FrameInstanceVisitor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.EventContext;
import com.oracle.truffle.api.instrumentation.ExecutionEventNode;
import com.oracle.truffle.api.instrumentation.ExecutionEventNodeFactory;
import com.oracle.truffle.api.instrumentation.SourceSectionFilter;
import com.oracle.truffle.api.instrumentation.StandardTags;
import com.oracle.truffle.api.instrumentation.TruffleInstrument;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.nodes.Node;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.scope.AssignLocalVariableNode;
import org.enso.interpreter.node.scope.ReadLocalVariableNode;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.instrument.Timer;
import org.enso.interpreter.runtime.tag.AvoidIdInstrumentationTag;
import org.enso.interpreter.runtime.tag.IdentifiedTag;
import org.enso.polyglot.DepTrackingService;
import org.enso.polyglot.RuntimeID;

/** An instrument for getting values from AST-identified expressions. */
@TruffleInstrument.Registration(
    id = DepTrackingService.INSTRUMENT_ID,
    services = DepTrackingService.class)
public class DepTrackingInstrument extends TruffleInstrument implements DepTrackingService {

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
  private static class DepTrackingEventNodeFactory implements ExecutionEventNodeFactory {

    private final CallTarget entryCallTarget;
    private final Callbacks callbacks;
    private final Timer timer;
    private final TruffleLogger logger;

    /**
     * Creates a new event node factory.
     *
     * @param entryCallTarget the call target being observed.
     * @param callbacks communication with users
     * @param timer the timer for timing execution
     */
    DepTrackingEventNodeFactory(
        CallTarget entryCallTarget, Callbacks callbacks, Timer timer, TruffleLogger logger) {

      this.entryCallTarget = entryCallTarget;
      this.callbacks = callbacks;
      this.timer = timer;
      this.logger = logger;
    }

    /**
     * Creates a new even node. If the event node replaces an invalidated one, it inherits its state
     * (execution environment).
     *
     * @param context the current context where this event node should get created.
     * @return a new event node wrapping a regular node
     */
    @Override
    public ExecutionEventNode create(EventContext context) {
      return new DepTrackingEventNode(context, timer, logger);
    }

    /** The execution event node class used by this instrument. */
    private class DepTrackingEventNode extends ExecutionEventNode {

      private final EventContext context;
      private long nanoTimeElapsed = 0;
      private static int COUNTER = 0;
      private final int eventNodeId;
      private final Timer timer;

      /**
       * Creates a new event node for instrumentation.
       *
       * @param context location where the node is being inserted
       */
      DepTrackingEventNode(EventContext context, Timer timer, TruffleLogger logger) {
        this.context = context;
        this.eventNodeId = COUNTER++;
        this.timer = timer;
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
        Node node = context.getInstrumentedNode();
        nanoTimeElapsed = timer.getTime();
        var runtimeID = getNodeId(node);
        if (node instanceof AssignLocalVariableNode localAssignment) {
          callbacks.startVariableAssignment(runtimeID);
        }
      }

      private static RuntimeID getNodeId(Node node) {
        return switch (node) {
          case ExpressionNode n -> n.getId();
          case null -> null;
          default -> null;
        };
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
        var runtimeID = getNodeId(node);
        if (node instanceof AssignLocalVariableNode localAssignment) {
          callbacks.endVariableAssignment(runtimeID);
          var wrappedRef = callbacks.wrapAsReference(runtimeID, result);
          throw context.createUnwind(wrappedRef);
        } else if (node instanceof ReadLocalVariableNode readLocal) {
          var unwrapped = callbacks.registerReturnValue(runtimeID, result);
          throw context.createUnwind(unwrapped);
        }
      }

      @Override
      public void onReturnExceptional(VirtualFrame frame, Throwable exception) {
        var node = context.getInstrumentedNode();
        var runtimeID = getNodeId(node);
        if (node instanceof AssignLocalVariableNode localAssignment) {
          callbacks.endVariableAssignment(runtimeID);
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
      TruffleObject mod, Callbacks callbacks, CallTarget entryCallTarget, Object timer) {
    var module = (Module) mod;
    var builder =
        SourceSectionFilter.newBuilder()
            .tagIs(StandardTags.WriteVariableTag.class, StandardTags.ReadVariableTag.class)
            .tagIs(IdentifiedTag.class)
            .tagIsNot(AvoidIdInstrumentationTag.class)
            .sourceIs(module::isModuleSource);

    /*if (entryCallTarget instanceof RootCallTarget r
        && r.getRootNode() instanceof ClosureRootNode c
        && c.getSourceSection() instanceof SourceSection section
        && section != null) {
      final int firstFunctionLine = section.getStartLine();
      final int afterFunctionLine = section.getEndLine() + 1;
      builder.lineIn(SourceSectionFilter.IndexRange.between(firstFunctionLine, afterFunctionLine));
    }*/
    var filter = builder.build();
    var factory =
        new DepTrackingEventNodeFactory(
            entryCallTarget,
            callbacks,
            (Timer) timer,
            env.getLogger(DepTrackingEventNodeFactory.DepTrackingEventNode.class));
    return env.getInstrumenter().attachExecutionEventFactory(filter, factory);
  }
}
