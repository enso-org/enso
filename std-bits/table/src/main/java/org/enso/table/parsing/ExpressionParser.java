package org.enso.table.parsing;

import java.util.ArrayList;
import java.util.List;

/*
Variables: designated with [ and ].  Right bracket is escaped by doubling - ]]
Quotes: like Enso, we will accept either ‘ or “ for a string.
Literals: Int, Float
Operators: +, -, *, /, %, &, |, ^, =, <, >, <=, >=, <>, !=, &&, ||, !, ~
Note - there are no assignment operators.  This defines an immutable expression which is incompatible with things like +=
*/

public class ExpressionParser {
  public enum TokenType {
    NUMBER,
    STRING,
    COLUMN_NAME,
    OPERATOR,
    BRACKET_START,
    BRACKET_END,
    IDENTIFIER,
    COMMA
  }

  public record Token(TokenType type, String value) {}

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

  private static boolean isDigit(char c, boolean allowDecimalPoint) {
    return switch (c) {
      case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' -> true;
      case '.' -> allowDecimalPoint;
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
    return switch (c) {
      case '_', ' ' -> true;
      default -> isLetter(c) || isDigit(c, false);
    };
  }

  public static List<Token> tokenise(String expression) throws IllegalArgumentException {
    int index = 0;

    List<Token> output = new ArrayList<>();
    while (index < expression.length()) {
      char c = expression.charAt(index);
      if (isWhitespace(c)) {
        index++;
      } else if (isOperator(c)) {
        output.add(new Token(TokenType.OPERATOR, Character.toString(c)));
        index++;
      } else if (c == '(') {
        output.add(new Token(TokenType.BRACKET_START, "("));
        index++;
      } else if (c == ')') {
        output.add(new Token(TokenType.BRACKET_START, ")"));
        index++;
      } else if (isDigit(c, false)) {
        var start = index;
        while (index < expression.length() && isDigit(expression.charAt(index), true)) {
          index++;
        }
        output.add(new Token(TokenType.NUMBER, expression.substring(start, index)));
      } else if (expression.charAt(index) == '"') {
        // Enter Excel Style String
        var end = findEnd(expression, index + 1, '"', '"');
        output.add(new Token(TokenType.STRING, expression.substring(index + 1, end - 1).replace("\"\"", "\"")));
        index = end + 1;
      } else if (expression.charAt(index) == '\'') {
        // Enter Enso Style String
        var end = findEnd(expression, index + 1, '\'', '\\');
        output.add(new Token(TokenType.STRING, expression.substring(index + 1, end - 1).replaceAll("\\(.)", "$1")));
        index = end + 1;
      } else if (expression.charAt(index) == '[') {
        // Column Name
        var end = findEnd(expression, index + 1, ']', ']');
        output.add(new Token(TokenType.COLUMN_NAME, expression.substring(index + 1, end - 1).replace("]]", "]")));
        index = end + 1;
      } else if (isLetter(c)) {
        var start = index;
        while (index < expression.length() && isIdentifier(expression.charAt(index))) {
          index++;
        }
        output.add(new Token(TokenType.IDENTIFIER, expression.substring(start, index)));
      } else if (c == ',') {
        output.add(new Token(TokenType.COMMA, ","));
        index++;
      } else {
        throw new IllegalArgumentException("Unexpected character " + c + " at position " + index);
      }
    }
    return output;
  }

  private static int findEnd(CharSequence expression, int index, char label, char escape) throws IllegalArgumentException {
    while (index < expression.length()) {
      switch (expression.charAt(index)) {
        case escape -> {
          if (escape == label && (index + 1 == expression.length() || expression.charAt(index + 1) != label)) {
            return index + 1;
          }
          index += 1;
        }
        case label -> {
          return index;
        }
        default -> {
          index++;
        }
      }
    }

    throw  new IllegalArgumentException("Unmatched " + label);
  }
}
