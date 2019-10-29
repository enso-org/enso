package org.enso.interpreter.builder;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.nodes.RootNode;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.enso.interpreter.AstArgDefinition;
import org.enso.interpreter.AstCallArg;
import org.enso.interpreter.AstCase;
import org.enso.interpreter.AstCaseFunction;
import org.enso.interpreter.AstExpression;
import org.enso.interpreter.AstExpressionVisitor;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.callable.ApplicationNode;
import org.enso.interpreter.node.callable.ForceNodeGen;
import org.enso.interpreter.node.callable.InvokeCallableNode;
import org.enso.interpreter.node.callable.argument.ReadArgumentNode;
import org.enso.interpreter.node.callable.function.CreateFunctionNode;
import org.enso.interpreter.node.callable.function.FunctionBodyNode;
import org.enso.interpreter.node.controlflow.*;
import org.enso.interpreter.node.expression.constant.ConstructorNode;
import org.enso.interpreter.node.expression.constant.DynamicSymbolNode;
import org.enso.interpreter.node.expression.literal.IntegerLiteralNode;
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

/**
 * An {@code ExpressionFactory} is responsible for converting the majority of Enso's parsed AST into
 * nodes evaluated by the interpreter at runtime.
 */
public class ExpressionFactory implements AstExpressionVisitor<ExpressionNode> {

  private final LocalScope scope;
  private final Language language;
  private final String scopeName;
  private final ModuleScope moduleScope;
  private String currentVarName = "annonymous";

  /**
   * Explicitly specifies all contructor parameters.
   *
   * @param language the name of the language for which the nodes are defined
   * @param scope the language scope in which definitions are processed
   * @param scopeName the name of the scope in which definitions are processed
   * @param moduleScope the current language global scope
   */
  public ExpressionFactory(
      Language language, LocalScope scope, String scopeName, ModuleScope moduleScope) {
    this.language = language;
    this.scope = scope;
    this.scopeName = scopeName;
    this.moduleScope = moduleScope;
  }

  /**
   * Defaults the local scope to a freshly constructed scope.
   *
   * @param language the name of the language for which the nodes are defined
   * @param scopeName the name of the scope in which definitions are processed
   * @param moduleScope the current language global scope
   */
  public ExpressionFactory(Language language, String scopeName, ModuleScope moduleScope) {
    this(language, new LocalScope(), scopeName, moduleScope);
  }

  /**
   * Defaults the local scope, and defaults the name of said scope to {@code "<root>"}
   *
   * @param language the name of the language for which the nodes are defined
   * @param moduleScope the current language global scope
   */
  public ExpressionFactory(Language language, ModuleScope moduleScope) {
    this(language, "<root>", moduleScope);
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
    return new ExpressionFactory(language, scope.createChild(), name, this.moduleScope);
  }

  /**
   * Creates an executable expression from an AST expression.
   *
   * @param expr the expression to make executable
   * @return a node representing the provided computation
   */
  public ExpressionNode run(AstExpression expr) {
    ExpressionNode result = expr.visit(this);
    result.markNotTail();
    return result;
  }

  /**
   * Creates a runtime {@code long} value from an AST node.
   *
   * @param l the value to represent
   * @return a runtime node representing that value
   */
  public ExpressionNode visitLong(long l) {
    return new IntegerLiteralNode(l);
  }

  /**
   * Creates runtime nodes representing arithmetic expressions.
   *
   * @param operator the operator to represent
   * @param leftAst the expressions to the left of the operator
   * @param rightAst the expressions to the right of the operator
   * @return a runtime node representing the arithmetic expression
   */
  @Override
  public ExpressionNode visitArithOp(
      String operator, AstExpression leftAst, AstExpression rightAst) {
    ExpressionNode left = leftAst.visit(this);
    ExpressionNode right = rightAst.visit(this);
    if (operator.equals("+")) {
      return AddOperatorNodeGen.create(left, right);
    }
    if (operator.equals("-")) {
      return SubtractOperatorNodeGen.create(left, right);
    }
    if (operator.equals("*")) {
      return MultiplyOperatorNodeGen.create(left, right);
    }
    if (operator.equals("/")) {
      return DivideOperatorNodeGen.create(left, right);
    }
    if (operator.equals("%")) {
      return ModOperatorNodeGen.create(left, right);
    }
    return null;
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
   * @param name the name of the variable
   * @return a runtime node representing the variable
   */
  @Override
  public ExpressionNode visitVariable(String name) {
    Supplier<Optional<ExpressionNode>> localVariableNode =
        () -> scope.getSlot(name).map(ReadLocalTargetNodeGen::create);
    Supplier<Optional<ExpressionNode>> constructorNode =
        () -> moduleScope.getConstructor(name).map(ConstructorNode::new);

    return Stream.of(localVariableNode, constructorNode)
        .map(Supplier::get)
        .filter(Optional::isPresent)
        .map(Optional::get)
        .findFirst()
        .orElseGet(() -> new DynamicSymbolNode(new UnresolvedSymbol(name, moduleScope)));
  }

  /**
   * Creates a runtime node representing the body of a function.
   *
   * <p>In addition to the creation of the node, this method is also responsible for rewriting
   * function arguments into a state where they can actually be read.
   *
   * @param arguments the arguments the function is defined for
   * @param expressions the body of the function
   * @param retValue the return value of the function
   * @return a runtime node representing the function body
   */
  public CreateFunctionNode processFunctionBody(
      List<AstArgDefinition> arguments, List<AstExpression> expressions, AstExpression retValue) {

    ArgDefinitionFactory argFactory =
        new ArgDefinitionFactory(scope, language, scopeName, moduleScope);
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

    List<ExpressionNode> fnBodyExpressionNodes =
        expressions.stream().map(stmt -> stmt.visit(this)).collect(Collectors.toList());

    List<ExpressionNode> allFnExpressions = new ArrayList<>();
    allFnExpressions.addAll(argExpressions);
    allFnExpressions.addAll(fnBodyExpressionNodes);

    ExpressionNode returnExpr = retValue.visit(this);

    FunctionBodyNode fnBodyNode =
        new FunctionBodyNode(allFnExpressions.toArray(new ExpressionNode[0]), returnExpr);
    RootNode fnRootNode =
        new EnsoRootNode(
            language, scope.getFrameDescriptor(), fnBodyNode, null, "lambda::" + scopeName);
    RootCallTarget callTarget = Truffle.getRuntime().createCallTarget(fnRootNode);

    return new CreateFunctionNode(callTarget, argDefinitions);
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
   * <p>Given that most of the work takes place in {@link #processFunctionBody(List, List,
   * AstExpression) processFunctionBody}, this node is solely responsible for handling the creation
   * of a new scope for the function, and marking it as tail recursive.
   *
   * @param arguments the arguments to the function
   * @param expressions the expressions that make up the function body
   * @param retValue the return value of the function
   * @return a runtime node representing the function
   */
  @Override
  public ExpressionNode visitFunction(
      List<AstArgDefinition> arguments, List<AstExpression> expressions, AstExpression retValue) {
    ExpressionFactory child = createChild(currentVarName);
    ExpressionNode fun = child.processFunctionBody(arguments, expressions, retValue);
    fun.markTail();
    return fun;
  }

  /**
   * Creates a runtime node representing a case function.
   *
   * <p>Given that most of the work takes place in {@link #processFunctionBody(List, List,
   * AstExpression) processFunctionBody}, this node is solely responsible for handling the creation
   * of a new scope for the function.
   *
   * @param arguments the arguments to the function
   * @param expressions the expressions that make up the function body
   * @param retValue the return value of the function
   * @return a runtime node representing the function
   */
  @Override
  public ExpressionNode visitCaseFunction(
      List<AstArgDefinition> arguments, List<AstExpression> expressions, AstExpression retValue) {
    ExpressionFactory child = createChild(currentVarName);
    return child.processFunctionBody(arguments, expressions, retValue);
  }

  /**
   * Creates a runtime node representing function application.
   *
   * @param function the function being called
   * @param arguments the arguments being applied to the function
   * @return a runtime node representing the function call
   */
  @Override
  public ExpressionNode visitFunctionApplication(
      AstExpression function, List<AstCallArg> arguments, boolean hasDefaultsSuspended) {
    CallArgFactory argFactory = new CallArgFactory(scope, language, scopeName, moduleScope);

    List<CallArgument> callArgs = new ArrayList<>();
    for (int position = 0; position < arguments.size(); ++position) {
      CallArgument arg = arguments.get(position).visit(argFactory, position);
      callArgs.add(arg);
    }

    InvokeCallableNode.DefaultsExecutionMode defaultsExecutionMode =
        hasDefaultsSuspended
            ? InvokeCallableNode.DefaultsExecutionMode.IGNORE
            : InvokeCallableNode.DefaultsExecutionMode.EXECUTE;

    return new ApplicationNode(
        function.visit(this), callArgs.toArray(new CallArgument[0]), defaultsExecutionMode);
  }

  /**
   * Creates a runtime node representing a conditional expression.
   *
   * @param cond the condition
   * @param ifTrue the code to execute if {@code cond} is true
   * @param ifFalse the code to execute if {@code cond} is false
   * @return a runtime node representing the conditional
   */
  @Override
  public ExpressionNode visitIf(AstExpression cond, AstExpression ifTrue, AstExpression ifFalse) {
    return new IfZeroNode(cond.visit(this), ifTrue.visit(this), ifFalse.visit(this));
  }

  /**
   * Creates a runtime node representing an assignment expression.
   *
   * @param varName the name of the variable
   * @param expr the expression whose result is assigned to {@code varName}
   * @return a runtime node representing the assignment
   */
  @Override
  public ExpressionNode visitAssignment(String varName, AstExpression expr) {
    currentVarName = varName;
    FrameSlot slot = scope.createVarSlot(varName);
    return AssignmentNodeGen.create(expr.visit(this), slot);
  }

  /**
   * Creates a runtime node representing a pattern match.
   *
   * @param target the value to destructure in the pattern match
   * @param branches the cases of the pattern match
   * @param fallback any fallback case for the pattern match
   * @return a runtime node representing a pattern match expression
   */
  @Override
  public ExpressionNode visitMatch(
      AstExpression target, List<AstCase> branches, Optional<AstCaseFunction> fallback) {
    ExpressionNode targetNode = target.visit(this);
    CaseNode[] cases =
        branches.stream()
            .map(
                branch ->
                    new ConstructorCaseNode(
                        branch.cons().visit(this), branch.function().visit(this)))
            .toArray(CaseNode[]::new);

    // Note [Pattern Match Fallbacks]
    CaseNode fallbackNode =
        fallback
            .map(fb -> (CaseNode) new FallbackNode(fb.visit(this)))
            .orElseGet(DefaultFallbackNode::new);

    return MatchNodeGen.create(cases, fallbackNode, targetNode);
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
   * @param target the parser AST fragment representing a value to force
   * @return the AST fragment representing forcing of the requested value
   */
  @Override
  public ExpressionNode visitDesuspend(AstExpression target) {
    return ForceNodeGen.create(target.visit(this));
  }
}
