package org.enso.table.parsing;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class ExpressionParser {
  public enum TokenType {
    NOTHING,
    NUMBER,
    BOOLEAN,
    STRING,
    COLUMN_NAME,
    OPERATOR,
    UNARY_OPERATOR,
    BRACKET_START,
    BRACKET_END,
    IDENTIFIER,
    COMMA
  }

  public record Token(TokenType type, String value) {
    @Override
    public boolean equals(Object obj) {
      if (obj instanceof Token other) {
        return type == other.type && value.equals(other.value);
      }
      return false;
    }
  }

  private static boolean isOperator(char c) {
    return switch (c) {
      case '+', '-', '*', '/', '%', '&', '|', '^', '=', '<', '>', '!', '~' -> true;
      default -> false;
    };
  }

  private static boolean isWhitespace(char c) {
    return switch (c) {
      case ' ', '\t', '\r', '\n' -> true;
      default -> false;
    };
  }

  private static boolean isDigit(char c, boolean allowDecimalPoint, boolean allowUnderscore) {
    return switch (c) {
      case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' -> true;
      case '.' -> allowDecimalPoint;
      case '_' -> allowUnderscore;
      default -> false;
    };
  }

  private static boolean isLetter(char c) {
    return switch (c) {
      case 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
          'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z' -> true;
      default -> false;
    };
  }

  private static boolean isIdentifier(char c) {
    return c == '_' || isLetter(c) || isDigit(c, false, false);
  }

  public static List<Token> tokenise(String expression) throws IllegalArgumentException {
    int index = 0;

    List<Token> output = new ArrayList<>();
    while (index < expression.length()) {
      char c = expression.charAt(index);
      if (isWhitespace(c)) {
        index++;
      } else if (isOperator(c)) {
        index = handleOperator(index, output, c);
        index++;
      } else if (c == '(') {
        output.add(new Token(TokenType.BRACKET_START, "("));
        index++;
      } else if (c == ')') {
        output.add(new Token(TokenType.BRACKET_END, ")"));
        index++;
      } else if (isDigit(c, false, false)) {
        var start = index;

        boolean foundDecimalPoint = false;
        while (index < expression.length() && isDigit(expression.charAt(index), !foundDecimalPoint, true)) {
          if (expression.charAt(index) == '.') {
            foundDecimalPoint = true;
          }
          index++;
        }
        var value = expression.substring(start, index).replace("_", "");
        if (lastMatches(output, TokenType.UNARY_OPERATOR)) {
          output.set(output.size() - 1, new Token(TokenType.NUMBER, "-" + value));
        } else {
          output.add(new Token(TokenType.NUMBER, value));
        }
      } else if (expression.charAt(index) == '"') {
        // Enter Excel Style String
        var end = findEnd(expression, index + 1, '"', '"');
        output.add(new Token(TokenType.STRING, expression.substring(index + 1, end - 1).replace("\"\"", "\"")));
        index = end;
      } else if (expression.charAt(index) == '\'') {
        // Enter Enso Style String
        var end = findEnd(expression, index + 1, '\'', '\\');
        output.add(new Token(TokenType.STRING, expression.substring(index + 1, end - 1).replaceAll("\\\\(.)", "$1")));
        index = end;
      } else if (expression.charAt(index) == '[') {
        // Column Name
        var end = findEnd(expression, index + 1, ']', ']');
        output.add(new Token(TokenType.COLUMN_NAME, expression.substring(index + 1, end - 1).replace("]]", "]")));
        index = end;
      } else if (isLetter(c)) {
        index = handleIdentifier(expression, index, output);
      } else if (c == ',') {
        output.add(new Token(TokenType.COMMA, ","));
        index++;
      } else {
        throw new IllegalArgumentException("Unexpected character " + c + " at position " + index);
      }
    }

    return output.stream()
        .map(ExpressionParser::MapSingleEquals)         // Map `=` to `==`
        .collect(Collectors.toList());
  }

  private static Token MapSingleEquals(Token token) {
    if (token.equals(new Token(TokenType.OPERATOR, "="))) {
      return new Token(TokenType.OPERATOR, "==");
    } else {
      return token;
    }
  }

  private static int handleOperator(int index, List<Token> output, char c) {
    if (output.size() > 0 && output.get(output.size() - 1).type() == TokenType.OPERATOR) {
      var prev = output.get(output.size() - 1).value;
      if (c == '=' && (prev.equals("<") || prev.equals(">") || prev.equals("!") || prev.equals("="))) {
        output.set(output.size() - 1, new Token(TokenType.OPERATOR, prev + c));
        index++;
      } else if (c == '&' && prev.equals("&")) {
        output.set(output.size() - 1, new Token(TokenType.OPERATOR, "&&"));
        index++;
      } else if (c == '|' && prev.equals("|")) {
        output.set(output.size() - 1, new Token(TokenType.OPERATOR, "||"));
        index++;
      } else if (c == '>' && prev.equals("<")) {
        output.set(output.size() - 1, new Token(TokenType.OPERATOR, "!="));
        index++;
      } else if (c == '-' || c == '!') {
        output.add(new Token(TokenType.OPERATOR, Character.toString(c)));
      } else {
        throw new IllegalArgumentException("Unexpected operator '" + prev + c + "' at index " + (index - 1));
      }
    } else {
      if (c == '-' && (output.size() == 0 || lastMatches(output, TokenType.BRACKET_START, TokenType.COMMA, TokenType.IDENTIFIER, TokenType.OPERATOR))) {
        output.add(new Token(TokenType.UNARY_OPERATOR, "-"));
      } else {
        output.add(new Token(TokenType.OPERATOR, Character.toString(c)));
      }
    }
    return index;
  }

  private static boolean lastMatches(List<Token> output, TokenType... types) {
    if (output.size() == 0) {
      return false;
    }
    var last = output.get(output.size() - 1);
    for (var type : types) {
      if (last.type() == type) {
        return true;
      }
    }
    return false;
  }

  private static int handleIdentifier(String expression, int index, List<Token> output) {
    var start = index;
    while (index < expression.length() && isIdentifier(expression.charAt(index))) {
      index++;
    }
    var identifer = expression.substring(start, index);
    switch (identifer.toLowerCase()) {
      case "true" -> output.add(new Token(TokenType.BOOLEAN, "True"));
      case "false" -> output.add(new Token(TokenType.BOOLEAN, "False"));
      case "and" -> output.add(new Token(TokenType.OPERATOR, "&&"));
      case "or" -> output.add(new Token(TokenType.OPERATOR, "||"));
      case "is" -> output.add(new Token(TokenType.UNARY_OPERATOR, "IS"));
      case "not" -> {
        if (output.size() > 0 && output.get(output.size() - 1).equals(new Token(TokenType.UNARY_OPERATOR, "IS"))) {
          output.set(output.size() - 1, new Token(TokenType.UNARY_OPERATOR, "IS NOT"));
        } else {
          output.add(new Token(TokenType.UNARY_OPERATOR, "!"));
        }
      }
      case "null" -> {
        if (output.size() > 0 && output.get(output.size() - 1).equals(new Token(TokenType.UNARY_OPERATOR, "IS"))) {
          output.set(output.size() - 1, new Token(TokenType.UNARY_OPERATOR, "IS NULL"));
        } else if (output.size() > 0 && output.get(output.size() - 1).equals(new Token(TokenType.UNARY_OPERATOR, "IS NOT"))) {
          output.set(output.size() - 1, new Token(TokenType.UNARY_OPERATOR, "IS NOT NULL"));
        } else {
          output.add(new Token(TokenType.NOTHING, "Nothing"));
        }
      }
      case "empty" -> {
        if (output.size() > 0 && output.get(output.size() - 1).equals(new Token(TokenType.UNARY_OPERATOR, "IS"))) {
          output.set(output.size() - 1, new Token(TokenType.UNARY_OPERATOR, "IS EMPTY"));
        } else if (output.size() > 0 && output.get(output.size() - 1) .equals(new Token(TokenType.UNARY_OPERATOR, "IS NOT"))) {
          output.set(output.size() - 1, new Token(TokenType.UNARY_OPERATOR, "IS NOT EMPTY"));
        } else {
          throw new IllegalArgumentException("Unexpected identifier 'Empty' at index " + start);
        }
      }
      default -> output.add(new Token(TokenType.IDENTIFIER, identifer));
    }
    return index;
  }

  private static int findEnd(CharSequence expression, int index, char label, char escape) throws IllegalArgumentException {
    while (index < expression.length()) {
      char c = expression.charAt(index);
      if (c == escape) {
        if (escape == label && (index + 1 == expression.length() || expression.charAt(index + 1) != label)) {
          return index + 1;
        }
        index += 2;
      } else if (c == label) {
        return index + 1;
      } else {
        index++;
      }
    }

    throw  new IllegalArgumentException("Unmatched " + label);
  }
}
