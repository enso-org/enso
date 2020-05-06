package org.enso.interpreter.service;

import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.ExecutionEventListener;
import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.instrument.Cache;
import org.enso.interpreter.instrument.IdExecutionInstrument;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.scope.ModuleScope;

import java.io.File;

import org.enso.polyglot.MethodNames;
import org.enso.text.buffer.Rope;
import org.enso.text.editing.JavaEditorAdapter;
import org.enso.text.editing.model;

import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;

/**
 * A service allowing externally-triggered code execution, registered by an instance of the
 * language.
 */
public class ExecutionService {
  private final Context context;
  private final IdExecutionInstrument idExecutionInstrument;
  private final InteropLibrary interopLibrary = InteropLibrary.getFactory().getUncached();

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

  /** @return the language context. */
  public Context getContext() {
    return context;
  }

  private Optional<FunctionCallInstrumentationNode.FunctionCall> prepareFunctionCall(
      Module module, String consName, String methodName) {
    ModuleScope scope = module.getScope(context);
    Optional<AtomConstructor> atomConstructorMay = scope.getConstructor(consName);
    if (!atomConstructorMay.isPresent()) {
      return Optional.empty();
    }
    AtomConstructor atomConstructor = atomConstructorMay.get();
    Function function = scope.lookupMethodDefinition(atomConstructor, methodName);
    if (function == null) {
      return Optional.empty();
    }
    FunctionCallInstrumentationNode.FunctionCall call =
        new FunctionCallInstrumentationNode.FunctionCall(
            function, context.getBuiltins().unit(), new Object[] {atomConstructor.newInstance()});
    return Optional.of(call);
  }

  /**
   * Executes a function with given arguments, represented as runtime language-level objects.
   *
   * @param call the call metadata.
   * @param cache the precomputed expression values.
   * @param valueCallback the consumer for expression value events.
   * @param funCallCallback the consumer for function call events.
   */
  public void execute(
      FunctionCallInstrumentationNode.FunctionCall call,
      Cache cache,
      Consumer<IdExecutionInstrument.ExpressionValue> valueCallback,
      Consumer<IdExecutionInstrument.ExpressionCall> funCallCallback)
      throws UnsupportedMessageException, ArityException, UnsupportedTypeException {

    SourceSection src = call.getFunction().getSourceSection();
    if (src == null) {
      return;
    }
    EventBinding<ExecutionEventListener> listener =
        idExecutionInstrument.bind(
            call.getFunction().getCallTarget(),
            src.getCharIndex(),
            src.getCharLength(),
            cache,
            valueCallback,
            funCallCallback);
    interopLibrary.execute(call);
    listener.dispose();
  }

  /**
   * Executes a method described by its name, constructor it's defined on and the module it's
   * defined in.
   *
   * @param modulePath the path to the module where the method is defined.
   * @param consName the name of the constructor the method is defined on.
   * @param methodName the method name.
   * @param cache the precomputed expression values.
   * @param valueCallback the consumer for expression value events.
   * @param funCallCallback the consumer for function call events.
   */
  public void execute(
      File modulePath,
      String consName,
      String methodName,
      Cache cache,
      Consumer<IdExecutionInstrument.ExpressionValue> valueCallback,
      Consumer<IdExecutionInstrument.ExpressionCall> funCallCallback)
      throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
    Optional<FunctionCallInstrumentationNode.FunctionCall> callMay =
        context
            .getModuleForFile(modulePath)
            .flatMap(module -> prepareFunctionCall(module, consName, methodName));
    if (!callMay.isPresent()) {
      return;
    }
    execute(callMay.get(), cache, valueCallback, funCallCallback);
  }

    /**
     * Evaluates an expression in the scope of the provided module.
     *
     * @param module the module providing a scope for the expression
     * @param expression the expression to evluated
     * @return a result of evaluation
     */
  public Object evaluateExpression(Module module, String expression)
        throws UnsupportedMessageException, ArityException,
        UnknownIdentifierException, UnsupportedTypeException {
    return interopLibrary.invokeMember(
            module,
            MethodNames.Module.EVAL_EXPRESSION,
            expression
    );
  }

    /**
     * Calls a function with the given argument.
     *
     * @param fn the function object
     * @param argument the argument applied to the function
     * @return the result of calling the function
     */
  public Object callFunction(Object fn, Object argument)
        throws UnsupportedTypeException, ArityException,
        UnsupportedMessageException {
    return interopLibrary.execute(fn, argument);
  }

  /**
   * Sets a module at a given path to use a literal source.
   *
   * If a module does not exist it will be created.
   *
   * @param path the module path.
   * @param contents the sources to use for it.
   */
  public void setModuleSources(File path, String contents) {
    Optional<Module> module = context.getModuleForFile(path);
    if (!module.isPresent()) {
      module = context.createModuleForFile(path);
    }
    module.ifPresent(mod -> mod.setLiteralSource(contents));
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
   */
  public void modifyModuleSources(File path, List<model.TextEdit> edits) {
    Optional<Module> moduleMay = context.getModuleForFile(path);
    if (!moduleMay.isPresent()) {
      return;
    }
    Module module = moduleMay.get();
    if (module.getLiteralSource() == null) {
      return;
    }
    Optional<Rope> editedSource = JavaEditorAdapter.applyEdits(module.getLiteralSource(), edits);
    editedSource.ifPresent(module::setLiteralSource);
  }
}
