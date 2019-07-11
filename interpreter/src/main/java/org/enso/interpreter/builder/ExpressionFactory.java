package org.enso.interpreter.builder;

import com.oracle.truffle.api.RootCallTarget;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.nodes.RootNode;
import org.enso.interpreter.*;
import org.enso.interpreter.node.EnsoRootNode;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.node.StatementNode;
import org.enso.interpreter.node.controlflow.*;
import org.enso.interpreter.node.expression.builtin.PrintNode;
import org.enso.interpreter.node.expression.literal.IntegerLiteralNode;
import org.enso.interpreter.node.expression.operator.AddOperatorNodeGen;
import org.enso.interpreter.node.expression.operator.DivideOperatorNodeGen;
import org.enso.interpreter.node.expression.operator.MultiplyOperatorNodeGen;
import org.enso.interpreter.node.expression.operator.SubtractOperatorNodeGen;
import org.enso.interpreter.node.function.CreateFunctionNode;
import org.enso.interpreter.node.function.FunctionBodyNode;
import org.enso.interpreter.node.function.InvokeNode;
import org.enso.interpreter.node.function.ReadArgumentNode;
import org.enso.interpreter.node.local.AssignmentNode;
import org.enso.interpreter.node.local.AssignmentNodeGen;
import org.enso.interpreter.node.local.ReadLocalVariableNodeGen;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class ExpressionFactory
    implements AstExpressionVisitor<ExpressionNode>,
        AstStatementVisitor<StatementNode, ExpressionNode> {

  private final LocalScope scope;
  private final Language language;
  private final String scopeName;

  private String currentVarName = "<root>";

  public ExpressionFactory(Language language, LocalScope scope, String name) {
    this.language = language;
    this.scope = scope;
    this.scopeName = name;
  }

  public ExpressionFactory(Language language) {
    this(language, new LocalScope(), "<root>");
  }

  public ExpressionFactory createChild(String name) {
    return new ExpressionFactory(language, scope.createChild(), name);
  }

  public ExpressionNode run(AstExpression body) {
    ExpressionNode result = body.visitExpression(this);
    result.markNotTail();
    return result;
  }

  public ExpressionNode visitLong(long l) {
    return new IntegerLiteralNode(l);
  }

  @Override
  public ExpressionNode visitArithOp(
      String operator, AstExpression leftAst, AstExpression rightAst) {
    ExpressionNode left = leftAst.visitExpression(this);
    ExpressionNode right = rightAst.visitExpression(this);
    if (operator.equals("+")) return AddOperatorNodeGen.create(left, right);
    if (operator.equals("-")) return SubtractOperatorNodeGen.create(left, right);
    if (operator.equals("*")) return MultiplyOperatorNodeGen.create(left, right);
    if (operator.equals("/")) return DivideOperatorNodeGen.create(left, right);
    return null;
  }

  @Override
  public ExpressionNode visitForeign(String lang, String code) {
    return null;
  }

  @Override
  public ExpressionNode visitVariable(String name) {
    FramePointer framePointer = scope.getSlot(name);
    return ReadLocalVariableNodeGen.create(
        framePointer.getFrameSlot(), framePointer.getParentLevel());
  }

  public ExpressionNode processFunctionBody(
      List<String> arguments, List<AstStatement> statements, AstExpression retValue) {
    List<StatementNode> argRewrites = new ArrayList<>();
    for (int i = 0; i < arguments.size(); i++) {
      FrameSlot slot = scope.createVarSlot(arguments.get(i));
      ReadArgumentNode readArg = new ReadArgumentNode(i);
      AssignmentNode assignArg = AssignmentNodeGen.create(readArg, slot);
      argRewrites.add(assignArg);
    }
    List<StatementNode> statementNodes =
        statements.stream().map(stmt -> stmt.visit(this)).collect(Collectors.toList());
    List<StatementNode> allStatements = new ArrayList<>();
    allStatements.addAll(argRewrites);
    allStatements.addAll(statementNodes);
    ExpressionNode expr = retValue.visitExpression(this);
    FunctionBodyNode functionBodyNode =
        new FunctionBodyNode(allStatements.toArray(new StatementNode[0]), expr);
    RootNode rootNode =
        new EnsoRootNode(
            language, scope.getFrameDescriptor(), functionBodyNode, null, "lambda::" + scopeName);
    RootCallTarget callTarget = Truffle.getRuntime().createCallTarget(rootNode);
    return new CreateFunctionNode(callTarget);
  }

  @Override
  public ExpressionNode visitFunction(
      List<String> arguments, List<AstStatement> statements, AstExpression retValue) {
    ExpressionFactory child = createChild(currentVarName);
    return child.processFunctionBody(arguments, statements, retValue);
  }

  @Override
  public ExpressionNode visitApplication(AstExpression function, List<AstExpression> arguments) {
    return new InvokeNode(
        function.visitExpression(this),
        arguments.stream().map(arg -> arg.visitExpression(this)).toArray(ExpressionNode[]::new));
  }

  @Override
  public ExpressionNode visitIf(AstExpression cond, AstExpression ifTrue, AstExpression ifFalse) {
    return new IfZeroNode(
        cond.visitExpression(this), ifTrue.visitExpression(this), ifFalse.visitExpression(this));
  }

  @Override
  public StatementNode visitAssignment(String varName, AstExpression expr) {
    currentVarName = varName;
    FrameSlot slot = scope.createVarSlot(varName);
    return AssignmentNodeGen.create(expr.visitExpression(this), slot);
  }

  @Override
  public StatementNode visitPrint(AstExpression body) {
    return new PrintNode(body.visitExpression(this));
  }
}
