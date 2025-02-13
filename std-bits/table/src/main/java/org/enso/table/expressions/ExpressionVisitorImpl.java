package org.enso.table.expressions;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.format.DateTimeParseException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.function.Function;
import java.util.regex.Pattern;

import org.antlr.v4.runtime.BaseErrorListener;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.RecognitionException;
import org.antlr.v4.runtime.Recognizer;
import org.enso.base.time.EnsoDateTimeFormatter;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.PolyglotException;
import org.graalvm.polyglot.Value;

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

  public static class Method {
    public Value ensoMethod;
    public Boolean isVariableArgumentMethod;
    public Boolean isStaticMethod;
    public Value staticsType;
    Method(Value module, Value type, String name, Boolean variableArgumentMethod){
      var context = Context.getCurrent().getBindings("enso");
      final Value staticsModule = context.invokeMember("get_module", "Standard.Table.Expression_Statics");
      staticsType = staticsModule.invokeMember("get_type", "Expression_Statics");
      // first we look in the list of static methods
      var staticMethod = staticsModule.invokeMember("get_method", staticsType, name);
      if (staticMethod.canExecute()) {
        ensoMethod = staticMethod;
        isStaticMethod = true;
      } else {
        // if not a static we look in the list of column methods
        ensoMethod = module.invokeMember("get_method", type, name);
        if (!ensoMethod.canExecute()) {
          throw new UnsupportedOperationException(name);
        }
        isStaticMethod = false;
      }
      this.isVariableArgumentMethod = variableArgumentMethod;
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
    var context = Context.getCurrent().getBindings("enso");
    final Value module = context.invokeMember("get_module", moduleName);
    final Value type = module.invokeMember("get_type", typeName);
    final var setVariableArgumentFunctions = new HashSet<>(Arrays.asList(variableArgumentFunctions));
    Function<String, Method> getMethod = name -> new Method(module, type, name, setVariableArgumentFunctions.contains(name));
    Function<String, Value> makeConstructor =
        name -> module.invokeMember("eval_expression", ".." + name);

    return evaluateImpl(
        expression,
        getColumn,
        makeConstantColumn,
        getMethod,
        makeConstructor);
  }

  public static Value evaluateImpl(
      String expression,
      Function<String, Value> getColumn,
      Function<Object, Value> makeConstantColumn,
      Function<String, Method> getMethod,
      Function<String, Value> makeConstructor) {
    var lexer = new ExpressionLexer(CharStreams.fromString(expression));
    lexer.removeErrorListeners();
    lexer.addErrorListener(ThrowOnErrorListener.INSTANCE);

    var tokens = new CommonTokenStream(lexer);
    var parser = new ExpressionParser(tokens);
    parser.removeErrorListeners();
    parser.addErrorListener(ThrowOnErrorListener.INSTANCE);

    var visitor =
        new ExpressionVisitorImpl(
            getColumn, makeConstantColumn, getMethod, makeConstructor);

    var expr = parser.prog();
    return visitor.visit(expr);
  }

  private final Function<String, Value> getColumn;
  private final Function<Object, Value> makeConstantColumn;
  private final Function<String, Method> getMethod;
  private final Function<String, Value> makeConstructor;

  private ExpressionVisitorImpl(
      Function<String, Value> getColumn,
      Function<Object, Value> makeConstantColumn,
      Function<String, Method> getMethod,
      Function<String, Value> makeConstructor) {
    this.getColumn = getColumn;
    this.makeConstantColumn = makeConstantColumn;
    this.getMethod = getMethod;
    this.makeConstructor = makeConstructor;
  }

  private Value wrapAsColumn(Value value) {
    if (value.isNull()) {
      return makeConstantColumn.apply(value);
    }

    var metaObject = value.getMetaObject();
    return metaObject != null
            && metaObject.isHostObject()
            && metaObject.asHostObject() instanceof Class<?>
        ? makeConstantColumn.apply(value)
        : value;
  }

  private Value executeMethod(String name, Value... args) {
    var method = getMethod.apply(name);
    Object[] objects = prepareArguments(method, args);
    try {
      var result = method.ensoMethod.execute(objects);
      if (result.canExecute()) {
        throw new IllegalArgumentException("Insufficient arguments for method " + name + ".");
      }
      return makeConstantColumn.apply(result);
    } catch (PolyglotException e) {
      if (e.getMessage().startsWith("Type error: expected a function")) {
        throw new IllegalArgumentException("Too many arguments for method " + name + ".");
      }
      throw e;
    }
  }

    private Object[] prepareArguments(Method method, Value[] args) {
        Object[] objects;
        if (method.isVariableArgumentMethod) {
            objects = new Object[2];
            objects[0] = wrapAsColumn(args[0]);
            objects[1] = Arrays.copyOfRange(args, 1, args.length, Object[].class);
        } else if (method.isStaticMethod) {
            // The static method takes the module as the synthetic 'self' argument, so we need to prepend
            // it:
            objects = new Object[args.length + 1];
            objects[0] = method.staticsType;
            System.arraycopy(args, 0, objects, 1, args.length);
        } else {
            objects = Arrays.copyOf(args, args.length, Object[].class);
            objects[0] = wrapAsColumn(args[0]);
        }   
        return objects;
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
    var v = visit(ctx.expr());
    if (v.isNumber() && v.fitsInLong()) {
      return Value.asValue(Math.negateExact(v.asLong()));
    }
    return executeMethod("*", v, Value.asValue(-1));
  }

  @Override
  public Value visitAtom(ExpressionParser.AtomContext ctx) {
    var atomName = ctx.IDENTIFIER().getText();
    return makeConstructor.apply(toCamelCase(atomName));
  }

  private static String toCamelCase(String name) {
    var builder = new StringBuilder(name.length());
    boolean convertNext = true;
    for (var c : name.toCharArray()) {
      if (convertNext) {
        builder.append(Character.toUpperCase(c));
        convertNext = c == '_';
      } else if (c == '_') {
        convertNext = true;
        builder.append(c);
      } else {
        builder.append(Character.toLowerCase(c));
      }
    }
    return builder.toString();
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

  private static final Pattern pythonRegex =
      Pattern.compile(
          "(\\\\[abtnfrv\"'\\\\])|(\\\\(x[0-9a-fA-F]{2}|u[0-9a-fA-F]{4}|U[0-9a-fA-F]{4}))|\\\\|([^\\\\]+)");

  private static String unescapePython(String text) {
    var matcher = pythonRegex.matcher(text);
    var builder = new StringBuilder(text.length());
    Context context = Context.getCurrent();
    while (matcher.find()) {
      if (matcher.group(1) != null) {
        builder.append(
            switch (matcher.group(1).charAt(1)) {
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

      context.safepoint();
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

  private static final EnsoDateTimeFormatter dateTimeFormatter =
      EnsoDateTimeFormatter.default_enso_zoned_date_time_formatter();

  @Override
  public Value visitDatetime(ExpressionParser.DatetimeContext ctx) {
    var text = ctx.text.getText();

    try {
      var dateTime = dateTimeFormatter.parseZonedDateTime(text);
      return Value.asValue(dateTime);
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
    return ctx.NOT_IN() != null ? executeMethod("not", condition) : condition;
  }

  @Override
  public Value visitFunction(ExpressionParser.FunctionContext ctx) {
    var name = ctx.IDENTIFIER().getText().toLowerCase();
    var args = ctx.expr().stream().map(this::visit).toArray(Value[]::new);
    return switch (name) {
      case "now" -> Value.asValue(LocalDateTime.now().atZone(ZoneId.systemDefault()));
      case "time" -> Value.asValue(LocalTime.now());
      default -> executeMethod(name, args);
    };
  }
}
