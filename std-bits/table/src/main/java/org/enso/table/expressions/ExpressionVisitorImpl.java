package org.enso.table.expressions;

import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.function.Function;

public class ExpressionVisitorImpl extends ExpressionBaseVisitor<Value> {
  private static final DateTimeFormatter DATE_TIME_FORMATTER =
      DateTimeFormatter.ISO_ZONED_DATE_TIME.withZone(ZoneId.systemDefault());

  public static Value evaluate(String expression, Function<String, Value> getColumn, Function<Object, Value> makeConstantColumn, String moduleName) {
    var lexer = new ExpressionLexer(CharStreams.fromString(expression));
    var tokens = new CommonTokenStream(lexer);
    var parser = new ExpressionParser(tokens);
    var expr = parser.prog();
    var visitor = new ExpressionVisitorImpl(getColumn, makeConstantColumn, moduleName);
    return visitor.visit(expr);
  }

  private final Function<String, Value> getColumn;
  private final Function<Object, Value> makeConstantColumn;
  private final Function<String, Value> getMethod;

  private ExpressionVisitorImpl(Function<String, Value> getColumn, Function<Object, Value> makeConstantColumn, String moduleName) {
    this.getColumn = getColumn;
    this.makeConstantColumn = makeConstantColumn;

    final Value module = Context.getCurrent().getBindings("enso").invokeMember("get_module", moduleName);
    final Value type = module.invokeMember("get_type", "Column");
    this.getMethod = name -> module.invokeMember("get_method", type, name);
  }

  private Value executeMethod(String name, Value self, Value... args) {
    return getMethod.apply(name).execute(makeConstantColumn.apply(self), args);
  }

  @Override
  public Value visitProg(ExpressionParser.ProgContext ctx) {
    Value base = visit(ctx.expr());
    return makeConstantColumn.apply(base);
  }

  @Override
  public Value visitColumn(ExpressionParser.ColumnContext ctx) {
    var text = ctx.getText();
    return getColumn.apply(text.substring(1, text.length() - 1).replace("]]", "]"));
  }

  @Override
  public Value visitPower(ExpressionParser.PowerContext ctx) {
    return executeMethod("^", visit(ctx.expr(0)), visit(ctx.expr(1)));
  }

  @Override
  public Value visitMultDivMod(ExpressionParser.MultDivModContext ctx) {
    return executeMethod(ctx.op.getText(), visit(ctx.expr(0)), visit(ctx.expr(1)));
  }

  @Override
  public Value visitCompare(ExpressionParser.CompareContext ctx) {
    var op = ctx.op.getText();
    if (op.equals("=")) {
      op = "==";
    }
    if (op.equals("<>")) {
      op = "!=";
    }

    return executeMethod(op, visit(ctx.expr(0)), visit(ctx.expr(1)));
  }

  @Override
  public Value visitLike(ExpressionParser.LikeContext ctx) {
    return executeMethod("like", visit(ctx.expr(0)), visit(ctx.expr(1)));
  }

  @Override
  public Value visitIsNull(ExpressionParser.IsNullContext ctx) {
    var op = ctx.IS_NULL() != null ? "is_missing" : "is_empty";
    return executeMethod(op, visit(ctx.expr()));
  }

  @Override
  public Value visitAddSub(ExpressionParser.AddSubContext ctx) {
    return executeMethod(ctx.op.getText(), visit(ctx.expr(0)), visit(ctx.expr(1)));
  }

  @Override
  public Value visitAnd(ExpressionParser.AndContext ctx) {
    return executeMethod("&&", visit(ctx.expr(0)), visit(ctx.expr(1)));
  }

  @Override
  public Value visitOr(ExpressionParser.OrContext ctx) {
    return executeMethod("||", visit(ctx.expr(0)), visit(ctx.expr(1)));
  }

  @Override
  public Value visitUnaryNot(ExpressionParser.UnaryNotContext ctx) {
    return executeMethod("not", visit(ctx.expr()));
  }

  @Override
  public Value visitUnaryMinus(ExpressionParser.UnaryMinusContext ctx) {
    return executeMethod("*", visit(ctx.expr()), Value.asValue(-1));
  }

  @Override
  public Value visitNullOrNothing(ExpressionParser.NullOrNothingContext ctx) {
    return Value.asValue(null);
  }

  @Override
  public Value visitBoolean(ExpressionParser.BooleanContext ctx) {
    return Value.asValue(ctx.TRUE() != null);
  }

  @Override
  public Value visitNumber(ExpressionParser.NumberContext ctx) {
    var text = ctx.getText().replace("_", "");
    if (text.contains(".")) {
      return Value.asValue(Double.parseDouble(text));
    } else {
      return Value.asValue(Long.parseLong(text));
    }
  }

  @Override
  public Value visitExcelString(ExpressionParser.ExcelStringContext ctx) {
    var text = ctx.getText();
    return Value.asValue(text.substring(1, text.length() - 1).replace("\"\"", "\""));
  }

  @Override
  public Value visitPythonString(ExpressionParser.PythonStringContext ctx) {
    var text = ctx.getText();
    return Value.asValue(text.substring(1, text.length() - 1).replaceAll("\\\\(.)", "$1"));
  }

  @Override
  public Value visitDate(ExpressionParser.DateContext ctx) {
    return Value.asValue(LocalDate.parse(ctx.text.getText()));
  }

  @Override
  public Value visitTime(ExpressionParser.TimeContext ctx) {
    return Value.asValue(LocalTime.parse(ctx.text.getText()));
  }

  @Override
  public Value visitDatetime(ExpressionParser.DatetimeContext ctx) {
    return Value.asValue(ZonedDateTime.parse(ctx.text.getText(), DATE_TIME_FORMATTER));
  }

  @Override
  public Value visitParen(ExpressionParser.ParenContext ctx) {
    return visit(ctx.expr());
  }

  @Override
  public Value visitBetween(ExpressionParser.BetweenContext ctx) {
    var self = visit(ctx.expr(0));
    var lower = visit(ctx.expr(1));
    var upper = visit(ctx.expr(2));
    return executeMethod("&&", executeMethod(">=", self, lower), executeMethod("<=", self, upper));
  }

  @Override
  public Value visitIn(ExpressionParser.InContext ctx) {
    var args = Value.asValue(ctx.expr().stream().skip(1).map(this::visit).toArray(Value[]::new));
    return executeMethod("is_in", visit(ctx.expr(0)), args);
  }

  @Override
  public Value visitFunction(ExpressionParser.FunctionContext ctx) {
    var name = ctx.IDENTIFIER().getText().toLowerCase();
    var args = ctx.expr().stream().skip(1).map(this::visit).toArray(Value[]::new);
    return executeMethod(name, visit(ctx.expr(0)), args);
  }
}
