package org.enso.table.expressions;

import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;

import org.enso.base.Time_Utils;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeParseException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;
import java.util.regex.Pattern;

public class ExpressionVisitorImpl extends ExpressionBaseVisitor<Value> {
  private static class ThrowOnErrorListener extends BaseErrorListener {
    public static final ThrowOnErrorListener INSTANCE = new ThrowOnErrorListener();

    @Override
    public void syntaxError(
        Recognizer<?, ?> recognizer,
        Object offendingSymbol,
        int line,
        int charPositionInLine,
        String msg,
        RecognitionException e)
        throws SyntaxErrorException {
      throw new SyntaxErrorException(msg, line, charPositionInLine);
    }
  }

  public static class SyntaxErrorException extends RuntimeException {
    private final int line;
    private final int column;

    public SyntaxErrorException(String message, int line, int column) {
      super(message);
      this.line = line;
      this.column = column;
    }

    public int getLine() {
      return line;
    }

    public int getColumn() {
      return column;
    }
  }

  public static Value evaluate(
      String expression,
      Function<String, Value> getColumn,
      Function<Object, Value> makeConstantColumn,
      String moduleName,
      String typeName,
      String[] variableArgumentFunctions)
      throws UnsupportedOperationException, IllegalArgumentException {
    var lexer = new ExpressionLexer(CharStreams.fromString(expression));
    lexer.removeErrorListeners();
    lexer.addErrorListener(ThrowOnErrorListener.INSTANCE);

    var tokens = new CommonTokenStream(lexer);
    var parser = new ExpressionParser(tokens);
    parser.removeErrorListeners();
    parser.addErrorListener(ThrowOnErrorListener.INSTANCE);

    var visitor =
        new ExpressionVisitorImpl(
            getColumn, makeConstantColumn, moduleName, typeName, variableArgumentFunctions);

    var expr = parser.prog();
    return visitor.visit(expr);
  }

  private final Function<String, Value> getColumn;
  private final Function<Object, Value> makeConstantColumn;
  private final Function<String, Value> getMethod;
  private final Set<String> variableArgumentFunctions;

  private ExpressionVisitorImpl(
      Function<String, Value> getColumn,
      Function<Object, Value> makeConstantColumn,
      String moduleName,
      String typeName,
      String[] variableArgumentFunctions) {
    this.getColumn = getColumn;
    this.makeConstantColumn = makeConstantColumn;

    final Value module =
        Context.getCurrent().getBindings("enso").invokeMember("get_module", moduleName);
    final Value type = module.invokeMember("get_type", typeName);
    this.getMethod = name -> module.invokeMember("get_method", type, name);

    this.variableArgumentFunctions = new HashSet<>(Arrays.asList(variableArgumentFunctions));
  }

  private Value wrapAsColumn(Value value) {
    if (value.isNull()) {
      return makeConstantColumn.apply(value);
    }

    var metaObject = value.getMetaObject();
    return metaObject != null && metaObject.isHostObject() && metaObject.asHostObject() instanceof Class<?>
        ? makeConstantColumn.apply(value)
        : value;
  }

  private Value executeMethod(String name, Value... args) {
    Value method = getMethod.apply(name);
    if (!method.canExecute()) {
      throw new UnsupportedOperationException(name);
    }

    Object[] objects;
    if (this.variableArgumentFunctions.contains(name)) {
      objects = new Object[2];
      objects[0] = args[0];
      objects[1] = Arrays.copyOfRange(args, 1, args.length, Object[].class);
    } else {
      objects = Arrays.copyOf(args, args.length, Object[].class);
    }
    objects[0] = wrapAsColumn(args[0]);

    try {
      var result = method.execute(objects);
      if (result.canExecute()) {
        throw new IllegalArgumentException("Insufficient arguments for method " + name + ".");
      }
      return result;
    } catch (PolyglotException e) {
      if (e.getMessage().startsWith("Type error: expected a function")) {
        throw new IllegalArgumentException("Too many arguments for method " + name + ".");
      }
      throw e;
    }
  }

  @Override
  public Value visitProg(ExpressionParser.ProgContext ctx) {
    Value base = visit(ctx.expr());
    return wrapAsColumn(base);
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
    var condition = executeMethod("like", visit(ctx.expr(0)), visit(ctx.expr(1)));
    return ctx.NOT_LIKE() != null ? executeMethod("not", condition) : condition;
  }

  @Override
  public Value visitIsNull(ExpressionParser.IsNullContext ctx) {
    var op = ctx.IS_NULL() != null || ctx.IS_NOT_NULL() != null ? "is_nothing" : "is_empty";
    var condition = executeMethod(op, visit(ctx.expr()));
    return ctx.IS_NOT_NULL() != null || ctx.IS_NOT_EMPTY() != null
        ? executeMethod("not", condition)
        : condition;
  }

  @Override
  public Value visitIf(ExpressionParser.IfContext ctx) {
    return executeMethod("iif", visit(ctx.expr(0)), visit(ctx.expr(1)), visit(ctx.expr(2)));
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

  private static final Pattern pythonRegex = Pattern.compile("(\\\\[abtnfrv\"'\\\\])|(\\\\(x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{4}))|\\\\|([^\\\\]+)");

  private static String unescapePython(String text) {
    var matcher = pythonRegex.matcher(text);
    var builder = new StringBuilder(text.length());
    while (matcher.find()) {
      if (matcher.group(1) != null) {
        builder.append(switch (matcher.group(1).charAt(1)) {
          case 'a' -> (char) 0x07;
          case 'f' -> (char) 0x0c;
          case 'b' -> '\b';
          case 't' -> '\t';
          case 'r' -> '\r';
          case 'n' -> '\n';
          case 'v' -> (char) 0x0b;
          case '\\' -> '\\';
          case '\'' -> '\'';
          case '"' -> '"';
          default -> throw new IllegalArgumentException("Unknown Python escape sequence.");
        });
      } else if (matcher.group(2) != null) {
        builder.append((char) Integer.parseInt(matcher.group(2).substring(2), 16));
      } else {
        builder.append(matcher.group(0));
      }
    }
    return builder.toString();
  }

  @Override
  public Value visitPythonString(ExpressionParser.PythonStringContext ctx) {
    var text = ctx.getText();
    return Value.asValue(unescapePython(text.substring(1, text.length() - 1)));
  }

  @Override
  public Value visitDate(ExpressionParser.DateContext ctx) {
    var text = ctx.text.getText();
    try {
      return Value.asValue(LocalDate.parse(ctx.text.getText()));
    } catch (DateTimeParseException e) {
      throw new SyntaxErrorException(
          "Invalid Date format: " + text,
          ctx.getStart().getLine(),
          ctx.getStart().getCharPositionInLine());
    }
  }

  @Override
  public Value visitTime(ExpressionParser.TimeContext ctx) {
    var text = ctx.text.getText();
    try {
      return Value.asValue(LocalTime.parse(ctx.text.getText()));
    } catch (DateTimeParseException e) {
      throw new SyntaxErrorException(
          "Invalid Time format: " + text,
          ctx.getStart().getLine(),
          ctx.getStart().getCharPositionInLine());
    }
  }

  @Override
  public Value visitDatetime(ExpressionParser.DatetimeContext ctx) {
    var text = Time_Utils.normaliseISODateTime(ctx.text.getText());

    try {
      var datetime = Time_Utils.default_zoned_date_time_formatter().parseBest(text, ZonedDateTime::from, LocalDateTime::from);
      if (datetime instanceof ZonedDateTime zonedDateTime) {
        return Value.asValue(zonedDateTime);
      } else if (datetime instanceof LocalDateTime localDateTime) {
        return Value.asValue(localDateTime.atZone(ZoneId.systemDefault()));
      }
    } catch (DateTimeParseException ignored) {
    }

    throw new SyntaxErrorException(
            "Invalid Date_Time format: " + text,
            ctx.getStart().getLine(),
            ctx.getStart().getCharPositionInLine());
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
    var condition = executeMethod("between", self, lower, upper);
    return ctx.NOT_BETWEEN() != null ? executeMethod("not", condition) : condition;
  }

  @Override
  public Value visitIn(ExpressionParser.InContext ctx) {
    var args = ctx.expr().stream().map(this::visit).toArray(Value[]::new);
    var condition = executeMethod("is_in", args);
    return  ctx.NOT_IN() != null ? executeMethod("not", condition) : condition;
  }

  @Override
  public Value visitFunction(ExpressionParser.FunctionContext ctx) {
    var name = ctx.IDENTIFIER().getText().toLowerCase();
    var args = ctx.expr().stream().map(this::visit).toArray(Value[]::new);
    return executeMethod(name, args);
  }
}
