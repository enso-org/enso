package org.enso.interpreter.builder;

import org.enso.interpreter.AstCallArgVisitor;
import org.enso.interpreter.AstExpression;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.callable.argument.CallArgument;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.scope.LocalScope;

/**
 * A {@code CallArgFactory} is responsible for converting arguments passed to a function call into
 * runtime nodes used by the interpreter to guide function evaluation.
 */
public class CallArgFactory implements AstCallArgVisitor<CallArgument> {
  private final LocalScope scope;
  private final Language language;
  private final String scopeName;
  private final ModuleScope moduleScope;

  /**
   * Explicitly specifies all constructor parameters.
   *
   * @param scope the language scope in which the arguments are called
   * @param language the name of the language for which the arguments are defined
   * @param scopeName the name of the scope in which the arguments are called
   * @param moduleScope the current language global scope
   */
  public CallArgFactory(
      LocalScope scope, Language language, String scopeName, ModuleScope moduleScope) {
    this.scope = scope;
    this.language = language;
    this.scopeName = scopeName;
    this.moduleScope = moduleScope;
  }

  /**
   * Processes a named argument application.
   *
   * <p>Arguments can be applied by name, and can occur at any point in the parameter list.
   *
   * @param name the name of the argument being applied
   * @param value the value of the argument being applied
   * @param position the position of this argument in the calling arguments list
   * @return a runtime representation of the argument
   */
  @Override
  public CallArgument visitNamedCallArg(String name, AstExpression value, int position) {
    ExpressionFactory factory = new ExpressionFactory(language, scope, scopeName, moduleScope);
    return new CallArgument(name, value.visit(factory));
  }

  /**
   * Processes a positional argument application.
   *
   * <p>Though all arguments have positions at the call site, an argument without a name is applied
   * purely based on its position.
   *
   * @param value the value of the argument being applied
   * @param position the position of this argument in the calling arguments list
   * @return a runtime representation of the argument
   */
  @Override
  public CallArgument visitUnnamedCallArg(AstExpression value, int position) {
    ExpressionFactory factory = new ExpressionFactory(language, scope, scopeName, moduleScope);
    return new CallArgument(value.visit(factory));
  }
}
