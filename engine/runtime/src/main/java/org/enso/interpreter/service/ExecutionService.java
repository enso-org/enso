package org.enso.interpreter.service;

import com.oracle.truffle.api.instrumentation.EventBinding;
import com.oracle.truffle.api.instrumentation.ExecutionEventListener;
import com.oracle.truffle.api.interop.ArityException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.interop.UnsupportedTypeException;
import com.oracle.truffle.api.source.SourceSection;
import org.enso.interpreter.instrument.IdExecutionInstrument;
import org.enso.interpreter.node.callable.FunctionCallInstrumentationNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.scope.ModuleScope;

import java.util.Optional;
import java.util.function.Consumer;

/**
 * A service allowing externally-triggered code execution, registered by an instance of the
 * language.
 */
public class ExecutionService {
  private final Context context;
  private final IdExecutionInstrument idExecutionInstrument;
  private InteropLibrary interopLibrary = InteropLibrary.getFactory().getUncached();

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

  private Optional<FunctionCallInstrumentationNode.FunctionCall> prepareFunctionCall(
      String moduleName, String consName, String methodName) {
    Optional<Module> moduleMay = context.compiler().topScope().getModule(moduleName);
    if (!moduleMay.isPresent()) {
      return Optional.empty();
    }
    ModuleScope scope = moduleMay.get().getScope(context);
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
   * @param valueCallback the consumer for expression value events.
   * @param funCallCallback the consumer for function call events.
   */
  public void execute(
      FunctionCallInstrumentationNode.FunctionCall call,
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
            valueCallback,
            funCallCallback);
    interopLibrary.execute(call);
    listener.dispose();
  }

  /**
   * Executes a method described by its name, constructor it's defined on and the module it's
   * defined in.
   *
   * @param moduleName the qualified name of the module the method is defined in.
   * @param consName the name of the constructor the method is defined on.
   * @param methodName the method name.
   * @param valueCallback the consumer for expression value events.
   * @param funCallCallback the consumer for function call events.
   */
  public void execute(
      String moduleName,
      String consName,
      String methodName,
      Consumer<IdExecutionInstrument.ExpressionValue> valueCallback,
      Consumer<IdExecutionInstrument.ExpressionCall> funCallCallback)
      throws UnsupportedMessageException, ArityException, UnsupportedTypeException {
    Optional<FunctionCallInstrumentationNode.FunctionCall> callMay =
        prepareFunctionCall(moduleName, consName, methodName);
    if (!callMay.isPresent()) {
      return;
    }
    execute(callMay.get(), valueCallback, funCallCallback);
  }
}
