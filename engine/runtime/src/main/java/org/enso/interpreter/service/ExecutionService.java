package org.enso.interpreter.service;

import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.ExecutionEventListener;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.compiler.context.Changeset;
import org.enso.interpreter.instrument.IdExecutionInstrument;
import org.enso.interpreter.instrument.MethodCallsCache;
import org.enso.interpreter.instrument.RuntimeCache;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.error.ConstructorDoesNotExistException;
import org.enso.interpreter.runtime.error.MethodDoesNotExistException;
import org.enso.interpreter.runtime.error.ModuleDoesNotExistException;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.state.data.EmptyMap;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;
import org.enso.text.buffer.Rope;
import org.enso.text.editing.IndexedSource;
import org.enso.text.editing.JavaEditorAdapter;
import org.enso.text.editing.TextEditor;
import org.enso.text.editing.model;

import java.io.File;
import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;

/**
 * A service allowing externally-triggered code execution, registered by an instance of the
 * language.
 */
public class ExecutionService {
  private final Context context;
  private final IdExecutionInstrument idExecutionInstrument;
  private final InteropLibrary interopLibrary = InteropLibrary.getFactory().getUncached();
  private final TruffleLogger logger = TruffleLogger.getLogger(LanguageInfo.ID);

  /**
   * Creates a new instance of this service.
   *
   * @param context the language context to use.
   * @param idExecutionInstrument an instance of the {@link IdExecutionInstrument} to use in the
   *     course of executions.
   */
  public ExecutionService(Context context, IdExecutionInstrument idExecutionInstrument) {
    this.idExecutionInstrument = idExecutionInstrument;
    this.context = context;
  }

  /** Thrown when the source code of the execution item does not exist. */
  public static class SourceDoesNotExistException extends RuntimeException {

    /**
     * Create new instance of this error.
     *
     * @param item the item which source code is missing.
     */
    public SourceDoesNotExistException(String item) {
      super("The " + item + " source does not exist.");
    }
  }

  /** @return the language context. */
  public Context getContext() {
    return context;
  }

  /** @return the execution service logger. */
  public TruffleLogger getLogger() {
    return logger;
  }

  private FunctionCallInstrumentationNode.FunctionCall prepareFunctionCall(
      Module module, String consName, String methodName)
      throws ConstructorDoesNotExistException, MethodDoesNotExistException {
    ModuleScope scope = module.compileScope(context);
    AtomConstructor atomConstructor =
        scope
            .getConstructor(consName)
            .orElseThrow(
                () -> new ConstructorDoesNotExistException(module.getName().toString(), consName));
    Function function = scope.lookupMethodDefinition(atomConstructor, methodName);
    if (function == null) {
      throw new MethodDoesNotExistException(atomConstructor, methodName, null);
    }
    return new FunctionCallInstrumentationNode.FunctionCall(
        function, EmptyMap.create(), new Object[] {atomConstructor.newInstance()});
  }

  /**
   * Executes a function with given arguments, represented as runtime language-level objects.
   *
   * @param call the call metadata.
   * @param cache the precomputed expression values.
   * @param methodCallsCache the storage tracking the executed method calls.
   * @param nextExecutionItem the next item scheduled for execution.
   * @param funCallCallback the consumer for function call events.
   * @param onComputedCallback the consumer of the computed value events.
   * @param onCachedCallback the consumer of the cached value events.
   * @param onExceptionalCallback the consumer of the exceptional events.
   */
  public void execute(
      FunctionCallInstrumentationNode.FunctionCall call,
      RuntimeCache cache,
      MethodCallsCache methodCallsCache,
      UUID nextExecutionItem,
      Consumer<IdExecutionInstrument.ExpressionCall> funCallCallback,
      Consumer<IdExecutionInstrument.ExpressionValue> onComputedCallback,
      Consumer<IdExecutionInstrument.ExpressionValue> onCachedCallback,
      Consumer<Throwable> onExceptionalCallback)
      throws ArityException, ModuleDoesNotExistException, MethodDoesNotExistException,
          SourceDoesNotExistException, UnsupportedMessageException, UnsupportedTypeException {
    SourceSection src = call.getFunction().getSourceSection();
    if (src == null) {
      throw new SourceDoesNotExistException(call.getFunction().getName());
    }
    EventBinding<ExecutionEventListener> listener =
        idExecutionInstrument.bind(
            call.getFunction().getCallTarget(),
            src.getCharIndex(),
            src.getCharLength(),
            cache,
            methodCallsCache,
            nextExecutionItem,
            funCallCallback,
            onComputedCallback,
            onCachedCallback,
            onExceptionalCallback);
    interopLibrary.execute(call);
    listener.dispose();
  }

  /**
   * Executes a method described by its name, constructor it's defined on and the module it's
   * defined in.
   *
   * @param moduleName the module where the method is defined.
   * @param consName the name of the constructor the method is defined on.
   * @param methodName the method name.
   * @param cache the precomputed expression values.
   * @param methodCallsCache the storage tracking the executed method calls.
   * @param nextExecutionItem the next item scheduled for execution.
   * @param funCallCallback the consumer for function call events.
   * @param onComputedCallback the consumer of the computed value events.
   * @param onCachedCallback the consumer of the cached value events.
   * @param onExceptionalCallback the consumer of the exceptional events.
   */
  public void execute(
      String moduleName,
      String consName,
      String methodName,
      RuntimeCache cache,
      MethodCallsCache methodCallsCache,
      UUID nextExecutionItem,
      Consumer<IdExecutionInstrument.ExpressionCall> funCallCallback,
      Consumer<IdExecutionInstrument.ExpressionValue> onComputedCallback,
      Consumer<IdExecutionInstrument.ExpressionValue> onCachedCallback,
      Consumer<Throwable> onExceptionalCallback)
      throws ArityException, ModuleDoesNotExistException, MethodDoesNotExistException,
          UnsupportedMessageException, UnsupportedTypeException {
    Module module = context.getModule(moduleName);
    FunctionCallInstrumentationNode.FunctionCall call =
        prepareFunctionCall(module, consName, methodName);
    execute(
        call,
        cache,
        methodCallsCache,
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
   * @param expression the expression to evluated
   * @return a result of evaluation
   */
  public Object evaluateExpression(Module module, String expression)
      throws UnsupportedMessageException, ArityException, UnknownIdentifierException,
          UnsupportedTypeException {
    return interopLibrary.invokeMember(module, MethodNames.Module.EVAL_EXPRESSION, expression);
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
    return interopLibrary.execute(fn, argument);
  }

  /**
   * Sets a module at a given path to use a literal source.
   *
   * <p>If a module does not exist it will be created.
   *
   * @param path the module path.
   * @param contents the sources to use for it.
   */
  public void setModuleSources(File path, String contents, boolean isIndexed) {
    Optional<Module> module = context.getModuleForFile(path);
    if (!module.isPresent()) {
      module = context.createModuleForFile(path);
    }
    module.ifPresent(
        mod -> {
          mod.setLiteralSource(contents);
          mod.setIndexed(isIndexed);
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
   * Finds a module by qualified name.
   *
   * @param moduleName the qualified name of the module
   * @return the relevant module, if exists
   */
  public Optional<Module> findModule(String moduleName) {
    return context.findModule(moduleName);
  }

  /**
   * Applies modifications to literal module sources.
   *
   * @param path the module to edit.
   * @param edits the edits to apply.
   * @return an object for computing the changed IR nodes.
   */
  public Optional<Changeset<Rope>> modifyModuleSources(File path, List<model.TextEdit> edits) {
    Optional<Module> moduleMay = context.getModuleForFile(path);
    if (!moduleMay.isPresent()) {
      return Optional.empty();
    }
    Module module = moduleMay.get();
    if (module.getLiteralSource() == null) {
      return Optional.empty();
    }
    Changeset<Rope> changeset =
        new Changeset<>(
            module.getLiteralSource(),
            module.getIr(),
            TextEditor.ropeTextEditor(),
            IndexedSource.RopeIndexedSource());
    Optional<Rope> editedSource = JavaEditorAdapter.applyEdits(module.getLiteralSource(), edits);
    editedSource.ifPresent(module::setLiteralSource);
    return Optional.of(changeset);
  }
}
