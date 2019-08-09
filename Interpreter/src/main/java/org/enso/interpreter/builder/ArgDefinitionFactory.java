package org.enso.interpreter.builder;

import org.enso.interpreter.AstArgDefinitionVisitor;
import org.enso.interpreter.AstExpression;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.scope.GlobalScope;
import org.enso.interpreter.runtime.scope.LocalScope;

/**
 * An {@code ArgDefinitionFactory} is responsible for converting argument definitions in the Enso
 * AST into runtime nodes for evaluation in the interpreter.
 */
public class ArgDefinitionFactory implements AstArgDefinitionVisitor<ArgumentDefinition> {
  private final LocalScope scope;
  private final Language language;
  private final String scopeName;
  private final GlobalScope globalScope;

  /**
   * Explicitly specifies all constructor arguments.
   *
   * @param scope the language scope into which the arguments are defined
   * @param language the name of the language for which the arguments are defined
   * @param scopeName the name of the scope in which the arguments are defined
   * @param globalScope the current language global scope
   */
  public ArgDefinitionFactory(
      LocalScope scope, Language language, String scopeName, GlobalScope globalScope) {
    this.scope = scope;
    this.language = language;
    this.scopeName = scopeName;
    this.globalScope = globalScope;
  }

  /**
   * Defaults the local scope to a newly created one.
   *
   * @param language the name of the language for which the arguments are defined
   * @param scopeName the name of the scope in which the arguments are defined
   * @param globalScope the current language global scope
   */
  public ArgDefinitionFactory(Language language, String scopeName, GlobalScope globalScope) {
    this(new LocalScope(), language, scopeName, globalScope);
  }

  /**
   * Default constructs the {@code LocalScope} and defaults the scope name to {@code <root>}.
   *
   * @param language the name of the language for which the arguments are defined
   * @param globalScope the current language global scope
   */
  public ArgDefinitionFactory(Language language, GlobalScope globalScope) {
    this(language, "<root>", globalScope);
  }

  /**
   * Processes a function argument that is provided without a default.
   *
   * @param name the name of the argument
   * @param position the position of the argument in the definition list
   * @return a runtime type representing the argument input
   */
  @Override
  public ArgumentDefinition visitBareArg(String name, int position) {
    return new ArgumentDefinition(position, name);
  }

  /**
   * Processes a function argument that is provided with a default value.
   *
   * @param name the name of the argument
   * @param value the default value of the argument
   * @param position the position of the argument in the definition list
   * @return a runtime type representing the argument input
   */
  @Override
  public ArgumentDefinition visitDefaultedArg(String name, AstExpression value, int position) {
    ExpressionFactory exprFactory = new ExpressionFactory(language, scope, scopeName, globalScope);
    return new ArgumentDefinition(position, name, value.visit(exprFactory));
  }
}
