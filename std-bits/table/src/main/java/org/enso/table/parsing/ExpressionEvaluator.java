package org.enso.table.parsing;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

public class ExpressionEvaluator {

  public static Value evaluate(String expression, Function<String, Value> getColumn, Function<Object, Value> makeConstantColumn) {
    var tokens = ExpressionParser.tokenise(expression);
    List<Value> values = new ArrayList<>(tokens.size());

    // Convert Columns and Constants
    Value unary_minus = Value.asValue(new ExpressionParser.Token(ExpressionParser.TokenType.UNARY_MINUS, "-"));
    Map<String, Value> operations= new HashMap<>();
    for (var token : tokens) {
      switch (token.type()) {
        case NOTHING -> values.add(makeConstantColumn.apply(null));
        case NUMBER -> values.add(makeConstantColumn.apply(Double.parseDouble(token.value())));
        case BOOLEAN -> values.add(makeConstantColumn.apply(token.value().equals("True")));
        case STRING -> values.add(makeConstantColumn.apply(token.value()));
        case COLUMN_NAME -> values.add(getColumn.apply(token.value()));
        case UNARY_MINUS -> values.add(unary_minus);
        case OPERATOR -> {
          operations.computeIfAbsent(token.value(), (key) -> Value.asValue(new ExpressionParser.Token(ExpressionParser.TokenType.OPERATOR, key)));
          values.add(operations.get(token.value()));
        }
        default -> throw new UnsupportedOperationException("Unsupported token type: " + token.type());
      }
    }

    // Get Enso Column
    final Value module = Context.getCurrent().getBindings("enso").invokeMember("get_module", "Standard.Table.Data.Column");
    final Value type = module.invokeMember("get_type", "Column");
    final Function<String, Value> getMethod = name -> module.invokeMember("get_method", type, name);

    // Handle Unary Minus
    int idx = 0;
    while (idx < values.size()) {
      if (values.get(idx) == unary_minus) {
        var mult = getMethod.apply("*");
        values.set(idx, mult.execute(values.get(idx + 1), -1));
        values.remove(idx + 1);
      }
      idx++;
    }

    // Do other operators
    idx = 0;
    while (idx < values.size()) {
      if (operations.containsValue(values.get(idx))) {
        var op = ensoOperationName(values.get(idx).as(ExpressionParser.Token.class).value());
        var left = values.get(idx - 1);
        var right = values.get(idx + 1);
        var method = getMethod.apply(op);
        values.set(idx - 1, method.execute(left, right));
        values.remove(idx);
        values.remove(idx);
      } else {
        idx++;
      }
    }

    // If single item good
    if (values.size() > 1) {
      throw new UnsupportedOperationException("Unsupported expression");
    }
    return values.get(0);
  }

  private static String ensoOperationName(String operation) {
    return switch (operation) {
      case "+", "-", "*", "/", "<", "<=", "!=", "==", ">=", ">", "&&", "||", "%" -> operation;
      default -> throw new UnsupportedOperationException("Unsupported operation: " + operation);
    };
  }
}
