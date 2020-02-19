package org.enso.interpreter.builder;

import com.oracle.truffle.api.source.Source;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.enso.compiler.core.IR.DefinitionSiteArgument;
import org.enso.compiler.core.IR.Expression;
import org.enso.compiler.core.IR.AstImport;
import org.enso.compiler.core.IR.Module;
import org.enso.compiler.core.AstModuleScopeVisitor;
import org.enso.compiler.core.IR.MethodDef;
import org.enso.compiler.core.IR.AtomDef;
import org.enso.interpreter.Constants;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.callable.function.CreateFunctionNode;
import org.enso.interpreter.runtime.Context;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.callable.function.FunctionSchema;
import org.enso.interpreter.runtime.error.VariableDoesNotExistException;
import org.enso.interpreter.runtime.scope.ModuleScope;

/**
 * A {@code GlobalScopeExpressionFactory} is responsible for converting the top-level definitions of
 * an Enso program into AST nodes for the interpreter to evaluate.
 */
public class ModuleScopeExpressionFactory implements AstModuleScopeVisitor<Function> {
  private final Language language;
  private final ModuleScope moduleScope;
  private final Source source;

  /**
   * Creates a factory for the given language.
   *
   * @param language the name of the language for which this factory is creating nodes
   * @param source the source this factory is used to parse
   * @param moduleScope the scope in which bindings created by this factory should be registered
   */
  public ModuleScopeExpressionFactory(Language language, Source source, ModuleScope moduleScope) {
    this.language = language;
    this.moduleScope = moduleScope;
    this.source = source;
  }

  /**
   * Executes the factory on a global expression.
   *
   * @param expr the expression to execute on
   */
  public void run(Module expr) {
    expr.visit(this);
  }

  /**
   * Processes definitions in the language global scope.
   *
   * @param imports any imports requested by this module
   * @param atomDefs any type definitions defined in the global scope
   * @param bindings any bindings made in the global scope
   */
  @Override
  public void visitModuleScope(
      List<AstImport> imports, List<AtomDef> atomDefs, List<MethodDef> bindings) {
    Context context = language.getCurrentContext();

    for (AstImport imp : imports) {
      this.moduleScope.addImport(context.compiler().requestProcess(imp.name()));
    }

    List<AtomConstructor> constructors =
        atomDefs.stream()
            .map(type -> new AtomConstructor(type.name(), moduleScope))
            .collect(Collectors.toList());

    constructors.forEach(moduleScope::registerConstructor);

    IntStream.range(0, constructors.size())
        .forEach(
            idx -> {
              ArgDefinitionFactory argFactory =
                  new ArgDefinitionFactory(language, source, moduleScope);
              AtomDef type = atomDefs.get(idx);
              ArgumentDefinition[] argDefs = new ArgumentDefinition[type.getArguments().size()];

              for (int i = 0; i < type.getArguments().size(); ++i) {
                argDefs[i] = type.getArguments().get(i).visit(argFactory, i);
              }

              constructors.get(idx).initializeFields(argDefs);
            });

    for (MethodDef method : bindings) {
      scala.Option<Expression> scalaNone = scala.Option.apply(null);
      DefinitionSiteArgument thisArgument =
          new DefinitionSiteArgument(Constants.Names.THIS_ARGUMENT, scalaNone, false);

      String typeName = method.typeName();
      if (typeName.equals(Constants.Names.CURRENT_MODULE)) {
        typeName = moduleScope.getAssociatedType().getName();
      }

      ExpressionFactory expressionFactory =
          new ExpressionFactory(
              language,
              source,
              typeName + Constants.SCOPE_SEPARATOR + method.methodName(),
              moduleScope);

      List<DefinitionSiteArgument> realArgs = new ArrayList<>(method.function().getArguments());
      realArgs.add(0, thisArgument);

      CreateFunctionNode funNode =
          expressionFactory.processFunctionBody(
              method.function().getLocation(), realArgs, method.function().body());
      funNode.markTail();
      Function function =
          new Function(
              funNode.getCallTarget(),
              null,
              new FunctionSchema(FunctionSchema.CallStrategy.CALL_LOOP, funNode.getArgs()));

      AtomConstructor constructor =
          moduleScope
              .getConstructor(typeName)
              .orElseThrow(() -> new VariableDoesNotExistException(method.typeName()));
      moduleScope.registerMethod(constructor, method.methodName(), function);
    }
  }
}
