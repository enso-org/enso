package org.enso.interpreter.builder;

import org.enso.interpreter.AstArgDefinitionVisitor;
import org.enso.interpreter.AstExpression;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.scope.LocalScope;

import java.util.Optional;

/**
 * An {@code ArgDefinitionFactory} is responsible for converting argument definitions in the Enso
 * AST into runtime nodes for evaluation in the interpreter.
 */
public class ArgDefinitionFactory implements AstArgDefinitionVisitor<ArgumentDefinition> {
  private final LocalScope scope;
  private final Language language;
  private final String scopeName;
  private final ModuleScope moduleScope;

  /**
   * Explicitly specifies all constructor arguments.
   *
   * @param scope the language scope into which the arguments are defined
   * @param language the name of the language for which the arguments are defined
   * @param scopeName the name of the scope in which the arguments are defined
   * @param moduleScope the current language global scope
   */
  public ArgDefinitionFactory(
      LocalScope scope, Language language, String scopeName, ModuleScope moduleScope) {
    this.scope = scope;
    this.language = language;
    this.scopeName = scopeName;
    this.moduleScope = moduleScope;
  }

  /**
   * Defaults the local scope to a newly created one.
   *
   * @param language the name of the language for which the arguments are defined
   * @param scopeName the name of the scope in which the arguments are defined
   * @param moduleScope the current language global scope
   */
  public ArgDefinitionFactory(Language language, String scopeName, ModuleScope moduleScope) {
    this(new LocalScope(), language, scopeName, moduleScope);
  }

  /**
   * Default constructs the {@code LocalScope} and defaults the scope name to {@code <root>}.
   *
   * @param language the name of the language for which the arguments are defined
   * @param moduleScope the current language global scope
   */
  public ArgDefinitionFactory(Language language, ModuleScope moduleScope) {
    this(language, "<root>", moduleScope);
  }

  /**
   * Processes a function argument.
   *
   * @param name the name of the argument
   * @param defaultValue the default value expression for this argument
   * @param suspended whether this argument's execution is lazy or not
   * @param position the position of the argument in the definition list
   * @return a runtime type representing the argument input
   */
  @Override
  public ArgumentDefinition visitArg(
      String name, Optional<AstExpression> defaultValue, boolean suspended, int position) {
    ExpressionNode defNode =
        defaultValue
            .map(
                def -> {
                  ExpressionFactory exprFactory =
                      new ExpressionFactory(language, scope, scopeName, moduleScope);
                  return defaultValue.get().visit(exprFactory);
                })
            .orElse(null);

    ArgumentDefinition.ExecutionMode executionMode =
        suspended
            ? ArgumentDefinition.ExecutionMode.PASS_THUNK
            : ArgumentDefinition.ExecutionMode.EXECUTE;
    return new ArgumentDefinition(position, name, defNode, executionMode);
  }
}
