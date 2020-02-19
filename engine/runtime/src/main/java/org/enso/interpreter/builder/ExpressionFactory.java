package org.enso.interpreter.builder;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.nodes.RootNode;
import com.oracle.truffle.api.source.Source;
import com.oracle.truffle.api.source.SourceSection;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Stream;
import org.enso.compiler.core.AstExpressionVisitor;
import org.enso.compiler.core.IR.BinaryOperator;
import org.enso.compiler.core.IR.Binding;
import org.enso.compiler.core.IR.CallArgumentDefinition;
import org.enso.compiler.core.IR.CaseFunction;
import org.enso.compiler.core.IR.DefinitionSiteArgument;
import org.enso.compiler.core.IR.Expression;
import org.enso.compiler.core.IR.ForcedTerm;
import org.enso.compiler.core.IR.Lambda;
import org.enso.compiler.core.IR.CaseExpr;
import org.enso.compiler.core.IR.LiteralName;
import org.enso.compiler.core.IR.NumberLiteral;
import org.enso.compiler.core.IR.Prefix;
import org.enso.compiler.core.IR.TextLiteral;
import org.enso.interpreter.Constants;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.ClosureRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.ApplicationNode;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.argument.ReadArgumentNode;
import org.enso.interpreter.node.callable.function.BlockNode;
import org.enso.interpreter.node.callable.function.CreateFunctionNode;
import org.enso.interpreter.node.callable.thunk.CreateThunkNode;
import org.enso.interpreter.node.callable.thunk.ForceNode;
import org.enso.interpreter.node.callable.thunk.ForceNodeGen;
import org.enso.interpreter.node.controlflow.CaseNode;
import org.enso.interpreter.node.controlflow.ConstructorCaseNode;
import org.enso.interpreter.node.controlflow.DefaultFallbackNode;
import org.enso.interpreter.node.controlflow.FallbackNode;
import org.enso.interpreter.node.controlflow.MatchNodeGen;
import org.enso.interpreter.node.expression.constant.ConstructorNode;
import org.enso.interpreter.node.expression.constant.DynamicSymbolNode;
import org.enso.interpreter.node.expression.literal.IntegerLiteralNode;
import org.enso.interpreter.node.expression.literal.TextLiteralNode;
import org.enso.interpreter.node.expression.operator.AddOperatorNodeGen;
import org.enso.interpreter.node.expression.operator.DivideOperatorNodeGen;
import org.enso.interpreter.node.expression.operator.ModOperatorNodeGen;
import org.enso.interpreter.node.expression.operator.MultiplyOperatorNodeGen;
import org.enso.interpreter.node.expression.operator.SubtractOperatorNodeGen;
import org.enso.interpreter.node.scope.AssignmentNode;
import org.enso.interpreter.node.scope.AssignmentNodeGen;
import org.enso.interpreter.node.scope.ReadLocalTargetNodeGen;
import org.enso.interpreter.runtime.callable.UnresolvedSymbol;
import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.CallArgument;
import org.enso.interpreter.runtime.error.DuplicateArgumentNameException;
import org.enso.interpreter.runtime.scope.LocalScope;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.syntax.text.Location;

/**
 * An {@code ExpressionFactory} is responsible for converting the majority of Enso's parsed AST into
 * nodes evaluated by the interpreter at runtime.
 */
public class ExpressionFactory implements AstExpressionVisitor<ExpressionNode> {

  private final LocalScope scope;
  private final Language language;
  private final Source source;
  private final String scopeName;
  private final ModuleScope moduleScope;
  private String currentVarName = "annonymous";

  /**
   * Explicitly specifies all contructor parameters.
   *
   * @param language the name of the language for which the nodes are defined
   * @param source the source this factory is used to parse
   * @param scope the language scope in which definitions are processed
   * @param scopeName the name of the scope in which definitions are processed
   * @param moduleScope the current language global scope
   */
  public ExpressionFactory(
      Language language,
      Source source,
      LocalScope scope,
      String scopeName,
      ModuleScope moduleScope) {
    this.language = language;
    this.source = source;
    this.scope = scope;
    this.scopeName = scopeName;
    this.moduleScope = moduleScope;
  }

  /**
   * Defaults the local scope to a freshly constructed scope.
   *
   * @param language the name of the language for which the nodes are defined
   * @param source the source this factory is used to parse
   * @param scopeName the name of the scope in which definitions are processed
   * @param moduleScope the current language global scope
   */
  public ExpressionFactory(
      Language language, Source source, String scopeName, ModuleScope moduleScope) {
    this(language, source, new LocalScope(), scopeName, moduleScope);
  }

  /**
   * Defaults the local scope, and defaults the name of said scope to {@code "<root>"}
   *
   * @param language the name of the language for which the nodes are defined
   * @param source the source this factory is used to parse
   * @param moduleScope the current language global scope
   */
  public ExpressionFactory(Language language, Source source, ModuleScope moduleScope) {
    this(language, source, "<root>", moduleScope);
  }

  /**
   * Creates a child of this {@code ExpressionFactory}.
   *
   * <p>This child will be initialized with a {@code LocalScope} that is a child of the local scope
   * contained within {@code this}.
   *
   * @param name the name of the new scope
   * @return a child of this current expression factory
   */
  public ExpressionFactory createChild(String name) {
    return new ExpressionFactory(language, source, scope.createChild(), name, this.moduleScope);
  }

  /**
   * Creates an executable expression from an AST expression.
   *
   * @param expr the expression to make executable
   * @return a node representing the provided computation
   */
  public ExpressionNode run(Expression expr) {
    ExpressionNode result = expr.visit(this);
    result.markNotTail();
    return result;
  }

  private SourceSection makeSection(Optional<Location> location) {
    return location
        .map(loc -> source.createSection(loc.start(), loc.length()))
        .orElseGet(source::createUnavailableSection);
  }

  private <T extends ExpressionNode> T setLocation(T expr, Optional<Location> location) {
    if (location.isPresent()) {
      Location loc = location.get();
      expr.setSourceLocation(loc.start(), loc.length());
    }
    return expr;
  }

  /**
   * Creates a runtime {@code long} value from an AST node.
   *
   * @param l the AST to represent
   * @return a runtime node representing that value
   */
  @Override
  public ExpressionNode visitLong(NumberLiteral l) {
    return setLocation(new IntegerLiteralNode(Long.parseLong(l.value())), l.getLocation());
  }

  /**
   * Creates a runtime String literal value from an AST node.
   *
   * @param string the AST to represent
   * @return a runtime node representing this literal
   */
  @Override
  public ExpressionNode visitStringLiteral(TextLiteral string) {
    ExpressionNode node = new TextLiteralNode(string.text());
    return setLocation(node, string.getLocation());
  }

  /**
   * Creates runtime nodes representing arithmetic expressions.
   *
   * @param ast the AST to represent
   * @return a runtime node representing the arithmetic expression
   */
  @Override
  public ExpressionNode visitArithOp(BinaryOperator ast) {
    ExpressionNode left = ast.left().visit(this);
    ExpressionNode right = ast.right().visit(this);
    String operator = ast.operator();
    ExpressionNode resultOp = null;
    if (operator.equals("+")) {
      resultOp = AddOperatorNodeGen.create(left, right);
    }
    if (operator.equals("-")) {
      resultOp = SubtractOperatorNodeGen.create(left, right);
    }
    if (operator.equals("*")) {
      resultOp = MultiplyOperatorNodeGen.create(left, right);
    }
    if (operator.equals("/")) {
      resultOp = DivideOperatorNodeGen.create(left, right);
    }
    if (operator.equals("%")) {
      resultOp = ModOperatorNodeGen.create(left, right);
    }
    return setLocation(resultOp, ast.getLocation());
  }

  /**
   * Creates runtime nodes representing foreign code blocks.
   *
   * @param lang the name of the foreign language
   * @param code the code in the foreign language
   * @return a runtime node representing the foreign code
   */
  @Override
  public ExpressionNode visitForeign(String lang, String code) {
    throw new RuntimeException("Foreign expressions not implemented yet.");
  }

  /**
   * Creates a runtime node representing a variable lookup.
   *
   * <p>This method is solely responsible for looking up a variable in the parent scope with the
   * provided name and does not handle associating that variable with a value.
   *
   * @param astName the AST to represent
   * @return a runtime node representing the variable
   */
  @Override
  public ExpressionNode visitName(LiteralName astName) {
    String name = astName.name();
    Optional<ExpressionNode> currentModuleVariable =
        name.equals(Constants.Names.CURRENT_MODULE)
            ? Optional.of(new ConstructorNode(moduleScope.getAssociatedType()))
            : Optional.empty();
    Supplier<Optional<ExpressionNode>> localVariableNode =
        () -> scope.getSlot(name).map(ReadLocalTargetNodeGen::create);
    Supplier<Optional<ExpressionNode>> constructorNode =
        () -> moduleScope.getConstructor(name).map(ConstructorNode::new);

    ExpressionNode variableRead =
        Stream.of(() -> currentModuleVariable, localVariableNode, constructorNode)
            .map(Supplier::get)
            .filter(Optional::isPresent)
            .map(Optional::get)
            .findFirst()
            .orElseGet(() -> new DynamicSymbolNode(new UnresolvedSymbol(name, moduleScope)));
    return setLocation(variableRead, astName.getLocation());
  }

  /**
   * Creates a runtime node representing the body of a function.
   *
   * <p>In addition to the creation of the node, this method is also responsible for rewriting
   * function arguments into a state where they can actually be read.
   *
   * @param location the source location of this function
   * @param arguments the arguments the function is defined for
   * @param body the body of the function
   * @return a runtime node representing the function body
   */
  public CreateFunctionNode processFunctionBody(
      Optional<Location> location, List<DefinitionSiteArgument> arguments, Expression body) {
    ArgDefinitionFactory argFactory =
        new ArgDefinitionFactory(scope, language, source, scopeName, moduleScope);
    ArgumentDefinition[] argDefinitions = new ArgumentDefinition[arguments.size()];
    List<ExpressionNode> argExpressions = new ArrayList<>();
    Set<String> seenArgNames = new HashSet<>();

    // Note [Rewriting Arguments]
    for (int i = 0; i < arguments.size(); i++) {
      ArgumentDefinition arg = arguments.get(i).visit(argFactory, i);
      argDefinitions[i] = arg;
      FrameSlot slot = scope.createVarSlot(arg.getName());
      ReadArgumentNode readArg = new ReadArgumentNode(i, arg.getDefaultValue().orElse(null));
      AssignmentNode assignArg = AssignmentNodeGen.create(readArg, slot);
      argExpressions.add(assignArg);

      String argName = arg.getName();

      if (seenArgNames.contains(argName)) {
        throw new DuplicateArgumentNameException(argName);
      } else {
        seenArgNames.add(argName);
      }
    }

    ExpressionNode bodyExpr = body.visit(this);

    BlockNode fnBodyNode = new BlockNode(argExpressions.toArray(new ExpressionNode[0]), bodyExpr);
    RootNode fnRootNode =
        new ClosureRootNode(
            language, scope, moduleScope, fnBodyNode, makeSection(location), scopeName);
    RootCallTarget callTarget = Truffle.getRuntime().createCallTarget(fnRootNode);

    CreateFunctionNode expr = new CreateFunctionNode(callTarget, argDefinitions);
    return setLocation(expr, location);
  }

  /* Note [Rewriting Arguments]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~
   * While it would be tempting to handle function arguments as a special case of a lookup, it is
   * instead far simpler to rewrite them such that they just become bindings in the function local
   * scope. This occurs for both explicitly passed argument values, and those that have been
   * defaulted.
   *
   * For each argument, the following algorithm is executed:
   *
   * 1. Argument Conversion: Arguments are converted into their definitions so as to provide a
   *    compact representation of all known information about that argument.
   * 2. Frame Conversion: A variable slot is created in the function's local frame to contain the
   *    value of the function argument.
   * 3. Read Provision: A `ReadArgumentNode` is generated to allow that function argument to be
   *    treated purely as a local variable access. See Note [Handling Argument Defaults] for more
   *    information on how this works.
   * 4. Value Assignment: A `AssignmentNode` is created to connect the argument value to the frame
   *    slot created in Step 2.
   * 5. Body Rewriting: The expression representing the argument is written into the function body,
   *    thus allowing it to be read simply.
   */

  /**
   * Creates a runtime node representing a function.
   *
   * <p>Given that most of the work takes place in {@link #processFunctionBody(Optional, List,
   * Expression)}, this node is solely responsible for handling the creation of a new scope for the
   * function, and marking it as tail recursive.
   *
   * @param lambda the AST to represent
   * @return a runtime node representing the function
   */
  @Override
  public ExpressionNode visitLambda(Lambda lambda) {
    ExpressionFactory child = createChild(currentVarName);
    ExpressionNode fun =
        child.processFunctionBody(lambda.getLocation(), lambda.getArguments(), lambda.body());
    fun.markTail();
    return fun;
  }

  /**
   * Creates a runtime node representing a case function.
   *
   * <p>Given that most of the work takes place in {@link #processFunctionBody(Optional, List,
   * Expression) processFunctionBody}, this node is solely responsible for handling the creation of
   * a new scope for the function.
   *
   * @param function the AST to represent
   * @return a runtime node representing the function
   */
  @Override
  public ExpressionNode visitCaseFunction(CaseFunction function) {
    ExpressionFactory child = createChild("case_expression");
    return child.processFunctionBody(
        function.getLocation(), function.getArguments(), function.body());

    // Explicitly not marked as tail
  }

  /**
   * Creates a runtime node representing function application.
   *
   * @param application the AST to represent
   * @return a runtime node representing the function call
   */
  @Override
  public ExpressionNode visitFunctionApplication(Prefix application) {
    CallArgFactory argFactory = new CallArgFactory(scope, language, source, scopeName, moduleScope);
    List<CallArgumentDefinition> arguments = application.getArgs();
    List<CallArgument> callArgs = new ArrayList<>();
    for (int position = 0; position < arguments.size(); ++position) {
      CallArgument arg = arguments.get(position).visit(argFactory, position);
      callArgs.add(arg);
    }

    InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode =
        application.hasDefaultsSuspended()
            ? InvokeCallableNode.DefaultsExecutionMode.IGNORE
            : InvokeCallableNode.DefaultsExecutionMode.EXECUTE;

    ApplicationNode appNode =
        new ApplicationNode(
            application.function().visit(this),
            callArgs.toArray(new CallArgument[0]),
            defaultsExecutionMode);
    setLocation(appNode, application.getLocation());
    return appNode;
  }

  /**
   * Creates a runtime node representing an assignment expression.
   *
   * @param ast the AST to represent
   * @return a runtime node representing the assignment
   */
  @Override
  public ExpressionNode visitAssignment(Binding ast) {
    currentVarName = ast.name();
    FrameSlot slot = scope.createVarSlot(ast.name());
    return setLocation(AssignmentNodeGen.create(ast.expression().visit(this), slot), ast.getLocation());
  }

  /**
   * Creates a runtime node representing a pattern match.
   *
   * @param caseExpr the AST to represent
   * @return a runtime node representing a pattern match expression
   */
  @Override
  public ExpressionNode visitMatch(CaseExpr caseExpr) {
    //      AstExpression target, List<AstCase> branches, Optional<AstCaseFunction> fallback) {

    ExpressionNode targetNode = caseExpr.scrutinee().visit(this);
    CaseNode[] cases =
        caseExpr.getBranches().stream()
            .map(
                branch ->
                    new ConstructorCaseNode(
                        branch.pattern().visit(this), branch.expression().visit(this)))
            .toArray(CaseNode[]::new);

    // Note [Pattern Match Fallbacks]
    CaseNode fallbackNode =
        caseExpr
            .getFallback()
            .map(fb -> (CaseNode) new FallbackNode(fb.visit(this)))
            .orElseGet(DefaultFallbackNode::new);

    ExpressionNode matchExpr = MatchNodeGen.create(cases, fallbackNode, targetNode);
    return setLocation(matchExpr, caseExpr.getLocation());
  }

  /* Note [Pattern Match Fallbacks]
   * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   * Enso in its current state has no coverage checking for constructors on pattern matches as it
   * has no sense of what constructors contribute to make a 'type'. This means that, in absence of a
   * user-provided fallback or catch-all case in a pattern match, the interpreter has to ensure that
   * it has one to catch that error.
   */

  /**
   * Creates a runtime representation of lazy function argument forcing.
   *
   * @param forcedTerm the AST to represent
   * @return the AST fragment representing forcing of the requested value
   */
  @Override
  public ExpressionNode visitForce(ForcedTerm forcedTerm) {
    ForceNode node = ForceNodeGen.create(forcedTerm.target().visit(this));
    return setLocation(node, forcedTerm.getLocation());
  }

  /**
   * Creates a runtime representation of a block.
   *
   * @param statements the statements making up the body of this block
   * @param retValue the return value expression
   * @return AST fragment representing the block
   */
  @Override
  public ExpressionNode visitBlock(
      List<Expression> statements, Expression retValue, boolean suspended) {
    if (suspended) {
      ExpressionFactory childFactory = this.createChild("suspended-block");
      LocalScope childScope = childFactory.scope;

      ExpressionNode block = childFactory.visitBlock(statements, retValue, false);

      RootNode defaultRootNode =
          new ClosureRootNode(
              language, childScope, moduleScope, block, null, "default::" + scopeName);
      RootCallTarget callTarget = Truffle.getRuntime().createCallTarget(defaultRootNode);

      return CreateThunkNode.build(callTarget);
    } else {
      ExpressionNode[] statementExprs =
          statements.stream().map(expr -> expr.visit(this)).toArray(ExpressionNode[]::new);
      ExpressionNode retExpr = retValue.visit(this);

      return new BlockNode(statementExprs, retExpr);
    }
  }
}
