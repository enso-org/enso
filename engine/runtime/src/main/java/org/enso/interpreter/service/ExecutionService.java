package org.enso.interpreter.service;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.ExecutionEventNodeFactory;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.compiler.context.SimpleUpdate;
import org.enso.interpreter.instrument.Endpoint;
import org.enso.interpreter.instrument.IdExecutionService;
import org.enso.interpreter.instrument.MethodCallsCache;
import org.enso.interpreter.instrument.NotificationHandler;
import org.enso.interpreter.instrument.RuntimeCache;
import org.enso.interpreter.instrument.UpdatesSynchronizationState;
import org.enso.interpreter.instrument.execution.Timer;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.node.expression.builtin.text.util.TypeToDisplayTextNodeGen;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.state.State;
import org.enso.interpreter.service.error.TypeNotFoundException;
import org.enso.interpreter.service.error.FailedToApplyEditsException;
import org.enso.interpreter.service.error.MethodNotFoundException;
import org.enso.interpreter.service.error.ModuleNotFoundException;
import org.enso.interpreter.service.error.SourceNotFoundException;
import org.enso.lockmanager.client.ConnectedLockManager;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;
import org.enso.text.editing.JavaEditorAdapter;
import org.enso.text.editing.model;

import java.io.File;
import java.io.IOException;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;

/**
 * A service allowing externally-triggered code execution, registered by an instance of the
 * language.
 */
public class ExecutionService {

  private static final String MAIN_METHOD = "main";
  private final EnsoContext context;
  private final Optional<IdExecutionService> idExecutionInstrument;
  private final NotificationHandler.Forwarder notificationForwarder;
  private final TruffleLogger logger = TruffleLogger.getLogger(LanguageInfo.ID);
  private final ConnectedLockManager connectedLockManager;
  private final ExecuteRootNode execute = new ExecuteRootNode();
  private final CallRootNode call = new CallRootNode();
  private final InvokeMemberRootNode invoke = new InvokeMemberRootNode();

  private final Timer timer;

  /**
   * Creates a new instance of this service.
   *
   * @param context the language context to use.
   * @param idExecutionInstrument optional instance of the {@link IdExecutionService} to use in the
   *     course of executions.
   * @param notificationForwarder a forwarder of notifications, used to communicate with the user.
   * @param connectedLockManager a connected lock manager (if it is in use) that should be connected
   *     to the language server, or null.
   * @param timer an execution timer.
   */
  public ExecutionService(
      EnsoContext context,
      Optional<IdExecutionService> idExecutionInstrument,
      NotificationHandler.Forwarder notificationForwarder,
      ConnectedLockManager connectedLockManager,
      Timer timer) {
    this.idExecutionInstrument = idExecutionInstrument;
    this.context = context;
    this.notificationForwarder = notificationForwarder;
    this.connectedLockManager = connectedLockManager;
    this.timer = timer;
  }

  /** @return the language context. */
  public EnsoContext getContext() {
    return context;
  }

  /** @return the execution service logger. */
  public TruffleLogger getLogger() {
    return logger;
  }

  public FunctionCallInstrumentationNode.FunctionCall prepareFunctionCall(
      Module module, String typeName, String methodName)
      throws TypeNotFoundException, MethodNotFoundException {
    ModuleScope scope = module.compileScope(context);
    Type type =
        scope
            .getType(typeName)
            .orElseThrow(() -> new TypeNotFoundException(module.getName().toString(), typeName));
    Function function = scope.lookupMethodDefinition(type, methodName);
    if (function == null) {
      throw new MethodNotFoundException(module.getName().toString(), type, methodName);
    }
    Object[] arguments = MAIN_METHOD.equals(methodName) ? new Object[] {} : new Object[] {type};
    return new FunctionCallInstrumentationNode.FunctionCall(
        function, State.create(EnsoContext.get(null)), arguments);
  }

  public void initializeLanguageServerConnection(Endpoint endpoint) {
    var notificationHandler = new NotificationHandler.InteractiveMode(endpoint);
    notificationForwarder.addListener(notificationHandler);

    if (connectedLockManager != null) {
      connectedLockManager.connect(endpoint);
    } else {
      logger.warning(
          "ConnectedLockManager was not initialized, even though a Language Server connection has been established. "
              + "This may result in synchronization errors.");
    }
  }

  /**
   * Executes a function with given arguments, represented as runtime language-level objects.
   *
   * @param module the module where the call is defined
   * @param call the call metadata.
   * @param cache the precomputed expression values.
   * @param methodCallsCache the storage tracking the executed method calls.
   * @param syncState the synchronization state of runtime updates.
   * @param nextExecutionItem the next item scheduled for execution.
   * @param funCallCallback the consumer for function call events.
   * @param onComputedCallback the consumer of the computed value events.
   * @param onCachedCallback the consumer of the cached value events.
   * @param onExceptionalCallback the consumer of the exceptional events.
   */
  public void execute(
      Module module,
      FunctionCallInstrumentationNode.FunctionCall call,
      RuntimeCache cache,
      MethodCallsCache methodCallsCache,
      UpdatesSynchronizationState syncState,
      UUID nextExecutionItem,
      Consumer<IdExecutionService.ExpressionCall> funCallCallback,
      Consumer<IdExecutionService.ExpressionValue> onComputedCallback,
      Consumer<IdExecutionService.ExpressionValue> onCachedCallback,
      Consumer<Exception> onExceptionalCallback)
      throws ArityException, SourceNotFoundException, UnsupportedMessageException,
          UnsupportedTypeException {
    SourceSection src = call.getFunction().getSourceSection();
    if (src == null) {
      throw new SourceNotFoundException(call.getFunction().getName());
    }
    Optional<EventBinding<ExecutionEventNodeFactory>> eventNodeFactory =
        idExecutionInstrument.map(
            service ->
                service.bind(
                    module,
                    call.getFunction().getCallTarget(),
                    cache,
                    methodCallsCache,
                    syncState,
                    this.timer,
                    nextExecutionItem,
                    funCallCallback,
                    onComputedCallback,
                    onCachedCallback,
                    onExceptionalCallback));
    Object p = context.getThreadManager().enter();
    try {
      execute.getCallTarget().call(call);
    } finally {
      context.getThreadManager().leave(p);
      eventNodeFactory.ifPresent(EventBinding::dispose);
    }
  }

  /**
   * Executes a method described by its name, constructor it's defined on and the module it's
   * defined in.
   *
   * @param moduleName the module where the method is defined.
   * @param typeName the name of the type the method is defined on.
   * @param methodName the method name.
   * @param cache the precomputed expression values.
   * @param methodCallsCache the storage tracking the executed method calls.
   * @param syncState the synchronization state of runtime updates.
   * @param nextExecutionItem the next item scheduled for execution.
   * @param funCallCallback the consumer for function call events.
   * @param onComputedCallback the consumer of the computed value events.
   * @param onCachedCallback the consumer of the cached value events.
   * @param onExceptionalCallback the consumer of the exceptional events.
   */
  public void execute(
      String moduleName,
      String typeName,
      String methodName,
      RuntimeCache cache,
      MethodCallsCache methodCallsCache,
      UpdatesSynchronizationState syncState,
      UUID nextExecutionItem,
      Consumer<IdExecutionService.ExpressionCall> funCallCallback,
      Consumer<IdExecutionService.ExpressionValue> onComputedCallback,
      Consumer<IdExecutionService.ExpressionValue> onCachedCallback,
      Consumer<Exception> onExceptionalCallback)
      throws ArityException, TypeNotFoundException, MethodNotFoundException,
          ModuleNotFoundException, UnsupportedMessageException, UnsupportedTypeException {
    Module module =
        context.findModule(moduleName).orElseThrow(() -> new ModuleNotFoundException(moduleName));
    FunctionCallInstrumentationNode.FunctionCall call =
        prepareFunctionCall(module, typeName, methodName);
    execute(
        module,
        call,
        cache,
        methodCallsCache,
        syncState,
        nextExecutionItem,
        funCallCallback,
        onComputedCallback,
        onCachedCallback,
        onExceptionalCallback);
  }

  /**
   * Evaluates an expression in the scope of the provided module.
   *
   * @param module the module providing a scope for the expression
   * @param expression the expression to evaluated
   * @return a result of evaluation
   */
  public Object evaluateExpression(Module module, String expression)
      throws UnsupportedMessageException, ArityException, UnknownIdentifierException,
          UnsupportedTypeException {
    Object p = context.getThreadManager().enter();
    try {
      return invoke.getCallTarget().call(module, expression);
    } finally {
      context.getThreadManager().leave(p);
    }
  }

  /**
   * Converts the provided object to a readable representation.
   *
   * @param receiver the object to convert.
   * @return the textual representation of the object.
   */
  public String toDisplayString(Object receiver) {
    try {
      var iop = InteropLibrary.getUncached();
      return iop.asString(iop.toDisplayString(receiver));
    } catch (UnsupportedMessageException ignored) {
      CompilerDirectives.shouldNotReachHere("Message support already checked.");
    }
    return null;
  }

  /**
   * Calls a function with the given argument.
   *
   * @param fn the function object
   * @param argument the argument applied to the function
   * @return the result of calling the function
   */
  public Object callFunction(Object fn, Object argument)
      throws UnsupportedTypeException, ArityException, UnsupportedMessageException {
    Object p = context.getThreadManager().enter();
    try {
      return call.getCallTarget().call(fn, new Object[] { argument });
    } finally {
      context.getThreadManager().leave(p);
    }
  }

  /**
   * Calls a function with the given argument and attaching an execution instrument.
   *
   * @param cache the runtime cache
   * @param module the module providing scope for the function
   * @param function the function object
   * @param arguments the sequence of arguments applied to the function
   * @return the result of calling the function
   */
  public Object callFunctionWithInstrument(
      RuntimeCache cache, Module module, Object function, Object... arguments)
      throws UnsupportedTypeException, ArityException, UnsupportedMessageException {
    UUID nextExecutionItem = null;
    CallTarget entryCallTarget =
        (function instanceof Function) ? ((Function) function).getCallTarget() : null;
    MethodCallsCache methodCallsCache = new MethodCallsCache();
    UpdatesSynchronizationState syncState = new UpdatesSynchronizationState();
    Consumer<IdExecutionService.ExpressionCall> funCallCallback = (value) -> {};
    Consumer<IdExecutionService.ExpressionValue> onComputedCallback =
        (value) -> context.getLogger().finest("_ON_COMPUTED " + value.getExpressionId());
    Consumer<IdExecutionService.ExpressionValue> onCachedCallback =
        (value) -> context.getLogger().finest("_ON_CACHED_VALUE " + value.getExpressionId());
    Consumer<Exception> onExceptionalCallback =
        (value) -> context.getLogger().finest("_ON_ERROR " + value);

    Optional<EventBinding<ExecutionEventNodeFactory>> eventNodeFactory =
        idExecutionInstrument.map(
            service ->
                service.bind(
                    module,
                    entryCallTarget,
                    cache,
                    methodCallsCache,
                    syncState,
                    this.timer,
                    nextExecutionItem,
                    funCallCallback,
                    onComputedCallback,
                    onCachedCallback,
                    onExceptionalCallback));
    Object p = context.getThreadManager().enter();
    try {
      return call.getCallTarget().call(function, arguments);
    } finally {
      context.getThreadManager().leave(p);
      eventNodeFactory.ifPresent(EventBinding::dispose);
    }
  }

  /**
   * Sets a module at a given path to use a literal source.
   *
   * <p>If a module does not exist it will be created.
   *
   * @param path the module path.
   * @param contents the sources to use for it.
   */
  public void setModuleSources(File path, String contents) {
    Optional<Module> module = context.getModuleForFile(path);
    if (module.isEmpty()) {
      module = context.createModuleForFile(path);
    }
    module.ifPresent(
        mod -> {
          mod.setLiteralSource(contents);
        });
  }

  /**
   * Resets a module to use on-disk sources.
   *
   * @param path the module path.
   */
  public void resetModuleSources(File path) {
    Optional<Module> module = context.getModuleForFile(path);
    module.ifPresent(Module::unsetLiteralSource);
  }

  /**
   * Applies modifications to literal module sources.
   *
   * @param module the module to edit.
   * @param edits the edits to apply.
   */
  public void modifyModuleSources(
      Module module,
      scala.collection.immutable.Seq<model.TextEdit> edits,
      SimpleUpdate simpleUpdate) {
    try {
      module.getSource();
    } catch (IOException e) {
      throw new SourceNotFoundException(module.getName(), e);
    }

    JavaEditorAdapter.applyEdits(module.getLiteralSource(), edits)
        .fold(
            failure -> {
              throw new FailedToApplyEditsException(
                  module.getName(), edits, failure, module.getLiteralSource());
            },
            rope -> {
              module.setLiteralSource(rope, simpleUpdate);
              return new Object();
            });
  }

  /**
   * Returns the language for the provided object, if it exists.
   *
   * @param o the object to get the language for
   * @return the associated language, or {@code null} if it doesn't exist
   */
  public String getLanguage(Object o) {
    var iop = InteropLibrary.getUncached(o);
    if (iop.hasSourceLocation(o)) {
      try {
        var sourceSection = iop.getSourceLocation(o);
        var source = sourceSection.getSource();
        if (source != null) {
          return source.getLanguage();
        }
      } catch (UnsupportedMessageException ignored) {
        CompilerDirectives.shouldNotReachHere("Message support already checked.");
      }
    }
    return null;
  }

  /**
   * Returns the source section for the provided object, if it exists.
   *
   * @param o the object to get the source section for
   * @return the associated source section, or {@code null} if it doesn't exist
   */
  public SourceSection getSourceLocation(Object o) {
    var iop = InteropLibrary.getUncached(o);
    if (iop.hasSourceLocation(o)) {
      try {
        return iop.getSourceLocation(o);
      } catch (UnsupportedMessageException ignored) {
        CompilerDirectives.shouldNotReachHere("Message support already checked.");
      }
    }
    return null;
  }

  /**
   * Returns a human-readable message for a panic exception.
   *
   * @param panic the panic to display.
   * @return a human-readable version of its contents.
   */
  public String getExceptionMessage(PanicException panic) {
    var iop = InteropLibrary.getUncached();
    var p = context.getThreadManager().enter();
    try {
      return iop.asString(
          iop.invokeMember(panic.getPayload(), "to_display_text"));
    } catch (UnsupportedMessageException
        | ArityException
        | UnknownIdentifierException
        | UnsupportedTypeException e) {
      return TypeToDisplayTextNodeGen.getUncached().execute(panic.getPayload());
    } catch (Throwable e) {
      if (iop.isException(e)) {
        return TypeToDisplayTextNodeGen.getUncached().execute(panic.getPayload());
      } else {
        throw e;
      }
    } finally {
      context.getThreadManager().leave(p);
    }
  }

  @SuppressWarnings("unchecked")
  private static <E extends Exception> E raise(Class<E> type, Exception ex) throws E {
    throw (E) ex;
  }

  private static final class ExecuteRootNode extends RootNode {
    @Node.Child
    private InteropLibrary iop = InteropLibrary.getFactory().createDispatched(5);

    ExecuteRootNode() {
      super(null);
    }

    @Override
    public Object execute(VirtualFrame frame) {
      try {
        if (frame.getArguments()[0] instanceof FunctionCallInstrumentationNode.FunctionCall call) {
          return iop.execute(call);
        }
        throw ArityException.create(1, 1, frame.getArguments().length);
      } catch (UnsupportedTypeException | ArityException | UnsupportedMessageException ex) {
        throw raise(RuntimeException.class, ex);
      }
    }
  }

  private static final class CallRootNode extends RootNode {
    @Node.Child
    private InteropLibrary iop = InteropLibrary.getFactory().createDispatched(5);

    CallRootNode() {
      super(null);
    }

    @Override
    public Object execute(VirtualFrame frame) {
      try {
        var self = frame.getArguments()[0];
        var args = (Object[]) frame.getArguments()[1];
        return iop.execute(self, args);
      } catch (UnsupportedTypeException | ArityException | UnsupportedMessageException ex) {
        throw raise(RuntimeException.class, ex);
      }
    }
  }

  private static final class InvokeMemberRootNode extends RootNode {
    @Node.Child
    private InteropLibrary iop = InteropLibrary.getFactory().createDispatched(5);

    InvokeMemberRootNode() {
      super(null);
    }

    @Override
    public Object execute(VirtualFrame frame) {
      var module = frame.getArguments()[0];
      var expression = frame.getArguments()[1];
      try {
        return iop.invokeMember(module, MethodNames.Module.EVAL_EXPRESSION, expression);
      } catch (UnknownIdentifierException | UnsupportedTypeException | ArityException | UnsupportedMessageException ex) {
        throw raise(RuntimeException.class, ex);
      }
    }
  }
}
