package org.enso.table.parsing;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

public class ExpressionEvaluator {

  public static Value evaluate(String expression, Function<String, Value> getColumn, Function<Object, Value> makeConstantColumn, String moduleName)
    throws UnsupportedOperationException {
    var tokens = ExpressionTokeniser.tokenise(expression);
    List<Value> values = new ArrayList<>(tokens.size());

    // Convert Columns and Constants
    for (var token : tokens) {
      switch (token.type()) {
        case NOTHING -> values.add(makeConstantColumn.apply(null));
        case NUMBER -> values.add(makeConstantColumn.apply(Double.parseDouble(token.value())));
        case BOOLEAN -> values.add(makeConstantColumn.apply(token.value().equals("True")));
        case STRING -> values.add(makeConstantColumn.apply(token.value()));
        case COLUMN_NAME -> values.add(getColumn.apply(token.value()));
        case UNARY_OPERATOR -> values.add(Value.asValue(token.value().equals("-") ? "negate" : token.value()));
        case BRACKET_START, BRACKET_END, OPERATOR -> values.add(Value.asValue(token.value()));
        default -> throw new UnsupportedOperationException("Unsupported token type: " + token.type());
      }
    }

    // Get Enso Column
    final Value module = Context.getCurrent().getBindings("enso").invokeMember("get_module", moduleName);
    final Value type = module.invokeMember("get_type", "Column");
    final Function<String, Value> getMethod = name -> module.invokeMember("get_method", type, name);

    // Evaluate
    evaluate(values, getMethod);

    // If single item good
    if (values.size() > 1) {
      throw new UnsupportedOperationException("Unsupported expression");
    }
    return values.get(0);
  }

  private static void evaluate(List<Value> values, Function<String, Value> getMethod) {
    // Contains Brackets
    int count = 0;
    int start = 0;
    int idx = 0;
    while (idx < values.size()) {
      if (values.get(idx).isString() && values.get(idx).asString().equals("(")) {
        count++;
        start = idx + 1;
      }

      if (values.get(idx).isString() && values.get(idx).asString().equals(")")) {
        count--;
        if (count < 0) {
          throw new UnsupportedOperationException("Unmatched bracket");
        } else if (count == 0) {
          // Evaluate a sub expression
          List<Value> subValues = values.stream().skip(start).limit(idx - start).collect(Collectors.toList());
          evaluate(subValues, getMethod);
          if (subValues.size() != 1) {
            throw new UnsupportedOperationException("Unsupported expression");
          }

          values.set(start - 1, subValues.get(0));
          if (idx >= start) {
            values.subList(start, idx + 1).clear();
          }
          idx = start - 1;
        }
      }

      idx++;
    }
    if (count > 0) {
      throw new UnsupportedOperationException("Unmatched bracket");
    }

    evaluateSubExpression(values, getMethod);
  }

  private static void evaluateSubExpression(List<Value> values, Function<String, Value> getMethod)
  {
    String[][] precedence = {{"negate", "~"}, {"^"}, {"*", "/", "%"}, {"+", "-"}, {"<", "<=", ">", ">=", "==", "!="}, {"IS NULL", "IS NOT NULL", "IS EMPTY", "IS NOT EMPTY", "LIKE", "IN", "BETWEEN"}, {"!"}, {"&&"}, {"||"}};

    // Do other operators
    for (String[] operators : precedence) {
      int idx = 0;
      while (idx < values.size()) {
        if (values.get(idx).isString() && Arrays.asList(operators).contains(values.get(idx).asString())) {
          String operator = values.get(idx).asString();
          switch (operator) {
            case "negate" -> unaryMinus(idx, values, getMethod);
            case "!", "~" -> unaryOperation(ensoOperationName(operator), idx, values, getMethod);
            case "IS NULL", "IS NT NOT NULL", "IS EMPTY", "IS NOT EMPTY" -> unaryBackOperation(ensoOperationName(operator), idx, values, getMethod);
            default -> binaryOperation(ensoOperationName(operator), idx, values, getMethod);
          }
        } else {
          idx++;
        }
      }
    }
  }

  private static void unaryMinus(int idx, List<Value> values, Function<String, Value> getMethod) {
    values.set(idx, getMethod.apply("*").execute(values.get(idx + 1), -1));
    values.remove(idx + 1);
  }

  private static void unaryOperation(String operation, int idx, List<Value> values, Function<String, Value> getMethod) {
    values.set(idx, getMethod.apply(operation).execute(values.get(idx + 1)));
    values.remove(idx + 1);
  }

  private static void unaryBackOperation(String operation, int idx, List<Value> values, Function<String, Value> getMethod) {
    values.set(idx, getMethod.apply(operation).execute(values.get(idx - 1)));
    values.remove(idx);
  }

  private static void binaryOperation(String operation, int idx, List<Value> values, Function<String, Value> getMethod) {
    var op = ensoOperationName(operation);
    var left = values.get(idx - 1);
    var right = values.get(idx + 1);
    var method = getMethod.apply(op);
    values.set(idx - 1, method.execute(left, right));
    values.remove(idx);
    values.remove(idx);
  }

  private static String ensoOperationName(String operation) {
    return switch (operation) {
      case "+", "-", "*", "/", "<", "<=", "!=", "==", ">=", ">", "&&", "||", "%", "^" -> operation;
      case "~" -> "bit_not";
      case "&" -> "bit_and";
      case "|" -> "bit_or";
      case "!" -> "not";
      case "IS NULL" -> "is_missing";
      case "IS NOT NULL" -> "is_present";
      case "IS EMPTY" -> "is_empty";
      case "IS NOT EMPTY" -> "is_not_empty";
      default -> throw new UnsupportedOperationException("Unsupported operation: " + operation);
    };
  }
}
