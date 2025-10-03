package org.enso.table.expressions;

import java.time.LocalDate;
import java.time.LocalTime;
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

  public static class TypeErrorException extends RuntimeException {
    public TypeErrorException(String message) {
      super(message);
    }
  }

  public interface MethodInterface {
    Value execute(Value[] args, Function<Object, Value> makeConstantColumn);

    Object[] prepareArguments(Value[] args, Function<Object, Value> makeConstantColumn);
  }

  public record MethodResolver(
      Value module, Value type, boolean isStaticMethod, Function<Object, Value> makeTypedColumn) {
    private MethodResolver(
        Context ctx,
        String moduleName,
        String typeName,
        boolean isStaticMethod,
        Function<Object, Value> makeTypedColumn) {
      this(
          ctx.getBindings("enso").invokeMember("get_module", moduleName),
          typeName,
          isStaticMethod,
          makeTypedColumn);
    }

    private MethodResolver(
        Value module,
        String typeName,
        boolean isStaticMethod,
        Function<Object, Value> makeTypedColumn) {
      this(module, module.invokeMember("get_type", typeName), isStaticMethod, makeTypedColumn);
    }

    public boolean canResolve(String methodName) {
      return resolve(methodName).canExecute();
    }

    public Value resolve(String methodName) {
      return module.invokeMember("get_method", type, methodName);
    }
  }

  public static MethodResolver newMethodResolver(
      Value module, Value type, boolean isStaticMethod, Function<Object, Value> makeTypedColumn) {
    var moduleName = module.getMetaQualifiedName();
    var typeName = type.getMetaSimpleName();
    return new MethodResolver(
        module.getContext(), moduleName, typeName, isStaticMethod, makeTypedColumn);
  }

  public static class Method implements MethodInterface {
    protected final MethodResolver methodResolver;
    protected final String name;

    public Method(MethodResolver methodResolver, String name) {
      this.methodResolver = methodResolver;
      this.name = name;
    }

    public static Method create(
        Iterable<MethodResolver> methodResolvers,
        String methodName,
        boolean isVariableArgumentMethod,
        boolean isMappingFunction,
        Function<Value, Value> mapColumn) {
      for (var resolver : methodResolvers) {
        if (resolver.canResolve(methodName)) {
          if (isVariableArgumentMethod) {
            return new VariableArgumentMethod(resolver, methodName);
          } else if (isMappingFunction) {
            return new StaticMapArgumentMethod(resolver, methodName, mapColumn);
          } else if (resolver.isStaticMethod) {
            return new StaticArgumentMethod(resolver, methodName);
          } else {
            return new Method(resolver, methodName);
          }
        }
      }
      throw new UnsupportedOperationException("Method not found: " + methodName);
    }

    protected final Object[] prepareArgumentsForExecute(
        Value[] args, Function<Object, Value> makeConstantColumn) {
      try {
        return prepareArguments(args, makeConstantColumn);
      } catch (PolyglotException e) {
        if (e.getMessage().startsWith("Type error: expected expression to be")) {
          throw new TypeErrorException(
              e.getMessage()
                  .replace(
                      "Type error: expected expression",
                      "method '" + name + "' expected first argument"));
        }
        throw e;
      }
    }

    protected Value doExecute(Object[] objects) {
      return methodResolver.resolve(this.name).execute(objects);
    }

    protected static boolean isFunction(Value value) {
      return value.canExecute() && !value.isDate() && !value.isTime();
    }

    @Override
    public Value execute(Value[] args, Function<Object, Value> makeConstantColumn) {
      Object[] objects = prepareArgumentsForExecute(args, makeConstantColumn);

      try {
        var result = doExecute(objects);

        // Date and Time objects report as can execute() but we want to treat as a value.
        if (isFunction(result)) {
          throw new IllegalArgumentException("Insufficient arguments for method " + name);
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
    public Object[] prepareArguments(Value[] args, Function<Object, Value> makeConstantColumn) {
      Object[] objects = Arrays.copyOf(args, args.length, Object[].class);
      objects[0] = this.methodResolver.makeTypedColumn.apply(makeConstantColumn.apply(args[0]));
      return objects;
    }
  }

  public static class VariableArgumentMethod extends Method {
    public VariableArgumentMethod(MethodResolver methodResolver, String name) {
      super(methodResolver, name);
    }

    @Override
    public Object[] prepareArguments(Value[] args, Function<Object, Value> makeConstantColumn) {
      return new Object[] {
        this.methodResolver.makeTypedColumn.apply(makeConstantColumn.apply(args[0])),
        Arrays.copyOfRange(args, 1, args.length, Object[].class)
      };
    }
  }

  public static class StaticArgumentMethod extends Method {
    public StaticArgumentMethod(MethodResolver methodResolver, String name) {
      super(methodResolver, name);
    }

    @Override
    public Object[] prepareArguments(Value[] args, Function<Object, Value> makeConstantColumn) {
      Object[] objects = new Object[args.length + 1];
      objects[0] = this.methodResolver.module;
      System.arraycopy(args, 0, objects, 1, args.length);
      return objects;
    }
  }

  public static class StaticMapArgumentMethod extends StaticArgumentMethod {
    private final Function<Value, Value> mapColumn;

    public StaticMapArgumentMethod(
        MethodResolver methodResolver, String name, Function<Value, Value> mapColumn) {
      super(methodResolver, name);
      this.mapColumn = mapColumn;
    }

    @Override
    protected Value doExecute(Object[] objects) {
      var result = methodResolver.resolve(this.name).execute(objects);
      if (!isFunction(result)) {
        return result;
      }

      // We have a function back, so we need to map this over the column
      return mapColumn.apply(result);
    }
  }

  public static Value evaluate(
      String expression,
      Function<String, Value> getColumn,
      Function<Object, Value> makeConstantColumn,
      Function<Value, Boolean> isColumn,
      MethodResolver[] methodResolvers,
      String[] variableArgumentFunctions,
      String[] mappingFunctions,
      Function<Value, Value> mapColumn)
      throws UnsupportedOperationException, IllegalArgumentException {
    final var setVariableArgumentFunctions =
        new HashSet<>(Arrays.asList(variableArgumentFunctions));
    final var setMappingFunctions = new HashSet<>(Arrays.asList(mappingFunctions));
    Function<String, MethodInterface> getMethod =
        name ->
            Method.create(
                java.util.Arrays.stream(methodResolvers).toList(),
                name,
                setVariableArgumentFunctions.contains(name),
                setMappingFunctions.contains(name),
                mapColumn);
    Function<String, Value> makeConstructor =
        name -> methodResolvers[0].module.invokeMember("eval_expression", ".." + name);

    return evaluateImpl(
        expression, getColumn, makeConstantColumn, isColumn, getMethod, makeConstructor);
  }

  public static Value evaluateImpl(
      String expression,
      Function<String, Value> getColumn,
      Function<Object, Value> makeConstantColumn,
      Function<Value, Boolean> isColumn,
      Function<String, MethodInterface> getMethod,
      Function<String, Value> makeConstructor) {
    var lexer = new ExpressionLexer(CharStreams.fromString(expression));
    lexer.removeErrorListeners();
    lexer.addErrorListener(ThrowOnErrorListener.INSTANCE);

    var tokens = new CommonTokenStream(lexer);
    checkTokenLimit(tokens, 1024);

    var parser = new ExpressionParser(tokens);
    parser.removeErrorListeners();
    parser.addErrorListener(ThrowOnErrorListener.INSTANCE);

    var visitor =
        new ExpressionVisitorImpl(
            getColumn, makeConstantColumn, isColumn, getMethod, makeConstructor);

    var expr = parser.prog();
    var result = visitor.visit(expr);
    return makeConstantColumn.apply(result);
  }

  private static void checkTokenLimit(CommonTokenStream tokens, int tokenLimit) {
    tokens.fill(); // Ensure the token stream is fully populated
    int tokenCount = 0;
    for (var token : tokens.getTokens()) {
      tokenCount++;
      if (tokenCount > tokenLimit) {
        throw new SyntaxErrorException(
            "Expression is too complex: "
                + tokens.size()
                + " tokens (exceeds "
                + tokenLimit
                + "). "
                + "Consider splitting into multiple expressions.",
            token.getLine(),
            token.getCharPositionInLine());
      }
    }
  }

  private final Function<String, Value> getColumn;
  private final Function<Object, Value> makeConstantColumn;
  private final Function<Value, Boolean> isColumn;
  private final Function<String, MethodInterface> getMethod;
  private final Function<String, Value> makeConstructor;

  private ExpressionVisitorImpl(
      Function<String, Value> getColumn,
      Function<Object, Value> makeConstantColumn,
      Function<Value, Boolean> isColumn,
      Function<String, MethodInterface> getMethod,
      Function<String, Value> makeConstructor) {
    this.getColumn = getColumn;
    this.makeConstantColumn = makeConstantColumn;
    this.isColumn = isColumn;
    this.getMethod = getMethod;
    this.makeConstructor = makeConstructor;
  }

  private Value executeMethod(String name, Value... args) {
    var method = getMethod.apply(name);
    Value result = method.execute(args, makeConstantColumn);
    return result;
  }

  private Value standardiseTypesAndExecuteMethod(String name, Value arg1, Value arg2) {
    // If we do 2 + [Column1] then we want to use Column addition for this
    // So we convert the 2 to a column before we execute the +
    // In the case of 2 + 5 we want to add these as integers so do not convert either
    // to columns
    Value typedArg1 = isColumn.apply(arg2) ? makeConstantColumn.apply(arg1) : arg1;
    return executeMethod(name, typedArg1, arg2);
  }

  @Override
  public Value visitProg(ExpressionParser.ProgContext ctx) {
    Value base = visit(ctx.expr());
    return base;
  }

  @Override
  public Value visitColumn(ExpressionParser.ColumnContext ctx) {
    var text = ctx.getText();
    return getColumn.apply(text.substring(1, text.length() - 1).replace("]]", "]"));
  }

  @Override
  public Value visitPower(ExpressionParser.PowerContext ctx) {
    return standardiseTypesAndExecuteMethod("^", visit(ctx.expr(0)), visit(ctx.expr(1)));
  }

  @Override
  public Value visitMultDivMod(ExpressionParser.MultDivModContext ctx) {
    return standardiseTypesAndExecuteMethod(
        ctx.op.getText(), visit(ctx.expr(0)), visit(ctx.expr(1)));
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

    return standardiseTypesAndExecuteMethod(op, visit(ctx.expr(0)), visit(ctx.expr(1)));
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
    return standardiseTypesAndExecuteMethod(
        ctx.op.getText(), visit(ctx.expr(0)), visit(ctx.expr(1)));
  }

  @Override
  public Value visitAnd(ExpressionParser.AndContext ctx) {
    return standardiseTypesAndExecuteMethod("&&", visit(ctx.expr(0)), visit(ctx.expr(1)));
  }

  @Override
  public Value visitOr(ExpressionParser.OrContext ctx) {
    return standardiseTypesAndExecuteMethod("||", visit(ctx.expr(0)), visit(ctx.expr(1)));
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
    // A Nothing token in an expression is assumed to mean a column of Nothings (or null column) and
    // so we convert it here.
    return makeConstantColumn.apply(Value.asValue(null));
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
  public Value visitRegexLiteral(ExpressionParser.RegexLiteralContext ctx) {
    String regexPattern = ctx.REGEX_LITERAL().getText();
    // Remove leading 'r/' and trailing '/'
    regexPattern = regexPattern.substring(2, regexPattern.length() - 1);
    return executeMethod("regex", Value.asValue(regexPattern));
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
    return executeMethod(name, args);
  }
}
