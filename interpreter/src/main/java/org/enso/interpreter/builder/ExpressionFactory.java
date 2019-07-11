package org.enso.interpreter.builder;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.nodes.RootNode;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.enso.interpreter.AstExpression;
import org.enso.interpreter.AstExpressionVisitor;
import org.enso.interpreter.Language;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.controlflow.AssignmentNode;
import org.enso.interpreter.node.controlflow.AssignmentNodeGen;
import org.enso.interpreter.node.controlflow.IfZeroNode;
import org.enso.interpreter.node.expression.builtin.PrintNode;
import org.enso.interpreter.node.expression.literal.IntegerLiteralNode;
import org.enso.interpreter.node.expression.operator.AddOperatorNodeGen;
import org.enso.interpreter.node.expression.operator.DivideOperatorNodeGen;
import org.enso.interpreter.node.expression.operator.ModOperatorNodeGen;
import org.enso.interpreter.node.expression.operator.MultiplyOperatorNodeGen;
import org.enso.interpreter.node.expression.operator.SubtractOperatorNodeGen;
import org.enso.interpreter.node.function.CreateFunctionNode;
import org.enso.interpreter.node.function.FunctionBodyNode;
import org.enso.interpreter.node.function.InvokeNode;
import org.enso.interpreter.node.function.ReadArgumentNode;
import org.enso.interpreter.node.scope.ReadGlobalTargetNode;
import org.enso.interpreter.node.scope.ReadLocalTargetNodeGen;
import org.enso.interpreter.runtime.GlobalCallTarget;

public class ExpressionFactory implements AstExpressionVisitor<ExpressionNode> {

  private final LocalScope scope;
  private final Language language;
  private final String scopeName;
  private final GlobalScope globalScope;

  private String currentVarName = "annonymous";

  public ExpressionFactory(
      Language language, LocalScope scope, String name, GlobalScope globalScope) {
    this.language = language;
    this.scope = scope;
    this.scopeName = name;
    this.globalScope = globalScope;
  }

  public ExpressionFactory(Language lang, String scopeName, GlobalScope globalScope) {
    this(lang, new LocalScope(), scopeName, globalScope);
  }

  public ExpressionFactory(Language language, GlobalScope globalScope) {
    this(language, "<root>", globalScope);
  }

  public ExpressionFactory createChild(String name) {
    return new ExpressionFactory(language, scope.createChild(), name, this.globalScope);
  }

  public ExpressionNode run(AstExpression expr) {
    ExpressionNode result = expr.visit(this);
    result.markNotTail();
    return result;
  }

  public ExpressionNode visitLong(long l) {
    return new IntegerLiteralNode(l);
  }

  @Override
  public ExpressionNode visitArithOp(
      String operator, AstExpression leftAst, AstExpression rightAst) {
    ExpressionNode left = leftAst.visit(this);
    ExpressionNode right = rightAst.visit(this);
    if (operator.equals("+")) return AddOperatorNodeGen.create(left, right);
    if (operator.equals("-")) return SubtractOperatorNodeGen.create(left, right);
    if (operator.equals("*")) return MultiplyOperatorNodeGen.create(left, right);
    if (operator.equals("/")) return DivideOperatorNodeGen.create(left, right);
    if (operator.equals("%")) return ModOperatorNodeGen.create(left, right);
    return null;
  }

  @Override
  public ExpressionNode visitForeign(String lang, String code) {
    return null;
  }

  @Override
  public ExpressionNode visitVariable(String name) {
    Optional<FramePointer> slot = scope.getSlot(name);

    if (slot.isPresent()) {
      return ReadLocalTargetNodeGen.create(slot.get().getFrameSlot(), slot.get().getParentLevel());
    } else {
      Optional<GlobalCallTarget> tgt = this.globalScope.getGlobalCallTarget(name);

      if (tgt.isPresent()) {
        return new ReadGlobalTargetNode(tgt.get());
      } else {
        throw new VariableDoesNotExistException(name);
      }
    }
  }

  public ExpressionNode processFunctionBody(
      List<String> arguments, List<AstExpression> statements, AstExpression retValue) {
    List<ExpressionNode> argRewrites = new ArrayList<>();
    for (int i = 0; i < arguments.size(); i++) {
      FrameSlot slot = scope.createVarSlot(arguments.get(i));
      ReadArgumentNode readArg = new ReadArgumentNode(i);
      AssignmentNode assignArg = AssignmentNodeGen.create(readArg, slot);
      argRewrites.add(assignArg);
    }
    List<ExpressionNode> statementNodes =
        statements.stream().map(stmt -> stmt.visit(this)).collect(Collectors.toList());
    List<ExpressionNode> allStatements = new ArrayList<>();
    allStatements.addAll(argRewrites);
    allStatements.addAll(statementNodes);
    ExpressionNode expr = retValue.visit(this);
    FunctionBodyNode functionBodyNode =
        new FunctionBodyNode(allStatements.toArray(new ExpressionNode[0]), expr);
    RootNode rootNode =
        new EnsoRootNode(
            language, scope.getFrameDescriptor(), functionBodyNode, null, "lambda::" + scopeName);
    RootCallTarget callTarget = Truffle.getRuntime().createCallTarget(rootNode);
    return new CreateFunctionNode(callTarget);
  }

  @Override
  public ExpressionNode visitFunction(
      List<String> arguments, List<AstExpression> statements, AstExpression retValue) {
    ExpressionFactory child = createChild(currentVarName);
    return child.processFunctionBody(arguments, statements, retValue);
  }

  @Override
  public ExpressionNode visitApplication(AstExpression function, List<AstExpression> arguments) {
    return new InvokeNode(
        function.visit(this),
        arguments.stream().map(arg -> arg.visit(this)).toArray(ExpressionNode[]::new));
  }

  @Override
  public ExpressionNode visitIf(AstExpression cond, AstExpression ifTrue, AstExpression ifFalse) {
    return new IfZeroNode(cond.visit(this), ifTrue.visit(this), ifFalse.visit(this));
  }

  @Override
  public ExpressionNode visitAssignment(String varName, AstExpression expr) {
    currentVarName = varName;
    FrameSlot slot = scope.createVarSlot(varName);
    return AssignmentNodeGen.create(expr.visit(this), slot);
  }

  @Override
  public ExpressionNode visitPrint(AstExpression body) {
    return new PrintNode(body.visit(this));
  }
}
