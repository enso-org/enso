package org.enso.interpreter.runtime.builtin;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.expression.builtin.interop.generic.*;
import org.enso.interpreter.node.expression.builtin.interop.syntax.*;
import org.enso.interpreter.node.expression.builtin.interop.syntax.GetArrayElementMethodGen;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.scope.ModuleScope;

/** A container class for all Boolean-related stdlib builtins. */
public class Polyglot {

  private final RootCallTarget interopDispatchRoot;
  private final FunctionSchema interopDispatchSchema;
  private final Function newInstanceFunction;
  private final Function polyglotArrayLengthFunction;
  private final Function polyglotArrayAtFunction;
  private final Function polyglotToTextFunction;

  public Polyglot(Language language, ModuleScope scope) {

    interopDispatchRoot = Truffle.getRuntime().createCallTarget(MethodDispatchNode.build(language));
    interopDispatchSchema =
        new FunctionSchema(
            FunctionSchema.CallStrategy.ALWAYS_DIRECT,
            FunctionSchema.CallerFrameAccess.NONE,
            new ArgumentDefinition[] {
              new ArgumentDefinition(1, "this", ArgumentDefinition.ExecutionMode.EXECUTE),
              new ArgumentDefinition(2, "method_name", ArgumentDefinition.ExecutionMode.EXECUTE),
              new ArgumentDefinition(3, "arguments", ArgumentDefinition.ExecutionMode.EXECUTE)
            },
            new boolean[] {false, true, false},
            new CallArgumentInfo[0]);
    newInstanceFunction = ConstructorDispatchNode.makeFunction(language);
    polyglotArrayAtFunction = GetArrayElementMethodGen.makeFunction(language);
    polyglotArrayLengthFunction = ArrayLengthMethodGen.makeFunction(language);
    polyglotToTextFunction = PolyglotToTextMethodGen.makeFunction(language);

    createPolyglot(language, scope);
  }

  private void createPolyglot(Language language, ModuleScope scope) {
    AtomConstructor polyglot = new AtomConstructor("Polyglot", scope).initializeFields();
    scope.registerConstructor(polyglot);
    scope.registerMethod(polyglot, "execute", ExecuteMethodGen.makeFunction(language));
    scope.registerMethod(polyglot, "invoke", InvokeMethodGen.makeFunction(language));
    scope.registerMethod(polyglot, "new", InstantiateMethodGen.makeFunction(language));
    scope.registerMethod(polyglot, "get_member", GetMemberMethodGen.makeFunction(language));
    scope.registerMethod(polyglot, "get_members", GetMembersMethodGen.makeFunction(language));
  }

  /*
   * Builds a function dispatching to a polyglot method call.
   *
   * @param method the name and scope of the method this function will dispatch to.
   * @return a function calling {@code method} with given arguments.
   */
  public Function buildPolyglotMethodDispatch(UnresolvedSymbol method) {
    Object[] preAppliedArr = new Object[] {null, method, null};
    return new Function(interopDispatchRoot, null, interopDispatchSchema, preAppliedArr, null);
  }

  public Function getPolyglotArrayLengthFunction() {
    return polyglotArrayLengthFunction;
  }

  public Function getPolyglotArrayAtFunction() {
    return polyglotArrayAtFunction;
  }

  public Function getPolyglotToTextFunction() {
    return polyglotToTextFunction;
  }

  /** @return a function executing a constructor with given arguments. */
  public Function getConstructorDispatch() {
    return newInstanceFunction;
  }
}
