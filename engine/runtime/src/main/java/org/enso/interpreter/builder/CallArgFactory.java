package org.enso.interpreter.builder;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import org.enso.compiler.core.AstCallArgVisitor;
import org.enso.compiler.core.IR.Expression;
import com.oracle.truffle.api.source.Source;

import com.oracle.truffle.api.source.SourceSection;
import org.enso.compiler.core.IR.ForcedTerm;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.ClosureRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.thunk.CreateThunkNode;
import org.enso.interpreter.runtime.callable.argument.CallArgument;
import org.enso.interpreter.runtime.scope.LocalScope;
import org.enso.interpreter.runtime.scope.ModuleScope;

import java.util.Optional;

/**
 * A {@code CallArgFactory} is responsible for converting arguments passed to a function call into
 * runtime nodes used by the interpreter to guide function evaluation.
 */
public class CallArgFactory implements AstCallArgVisitor<CallArgument> {

  private final LocalScope scope;
  private final Language language;
  private final Source source;
  private final String scopeName;
  private final ModuleScope moduleScope;

  /**
   * Explicitly specifies all constructor parameters.
   *
   * @param scope the language scope in which the arguments are called
   * @param language the name of the language for which the arguments are defined
   * @param source the source this factory is used to parse
   * @param scopeName the name of the scope in which the arguments are called
   * @param moduleScope the current language global scope
   */
  public CallArgFactory(
      LocalScope scope,
      Language language,
      Source source,
      String scopeName,
      ModuleScope moduleScope) {
    this.scope = scope;
    this.language = language;
    this.source = source;
    this.scopeName = scopeName;
    this.moduleScope = moduleScope;
  }

  /**
   * Processes an argument application.
   *
   * <p>Arguments can be applied by name, and named arguments can occur at any point in the
   * parameter list.
   *
   * @param name the name of the argument being applied
   * @param value the value of the argument being applied
   * @param position the position of this argument in the calling arguments list
   * @return a runtime representation of the argument
   */
  @Override
  public CallArgument visitCallArg(Optional<String> name, Expression value, int position) {
    ExpressionNode result;

    if (value instanceof ForcedTerm) {
      ExpressionFactory factory = new ExpressionFactory(language, source, scope, scopeName, moduleScope);
      result = ((ForcedTerm) value).target().visit(factory);
    } else {
      LocalScope childScope = new LocalScope(scope);
      ExpressionFactory factory =
          new ExpressionFactory(language, source, childScope, scopeName, moduleScope);
      ExpressionNode expr = value.visit(factory);
      expr.markTail();
      String displayName = "call_argument<" + name.orElse(String.valueOf(position)) + ">";
      SourceSection section =
          value
              .getLocation()
              .map(loc -> source.createSection(loc.start(), loc.length()))
              .orElse(null);

      RootCallTarget callTarget =
          Truffle.getRuntime()
              .createCallTarget(
                  new ClosureRootNode(language, childScope, moduleScope, expr, section, displayName));
      result = CreateThunkNode.build(callTarget);
    }

    return new CallArgument(name.orElse(null), result);
  }
}
