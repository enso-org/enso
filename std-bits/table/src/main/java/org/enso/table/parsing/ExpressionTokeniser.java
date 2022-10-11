package org.enso.table.parsing;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class ExpressionTokeniser {
  private static final Pattern WHITE_SPACE = Pattern.compile("^\\s+");

  public enum TokenType {
    NOTHING("(NOTHING|NULL)"),
    NUMBER("[0-9][0-9_]*(\\.[0-9][0-9_]*)?"),
    BOOLEAN("(TRUE|FALSE)"),
    STRING("(\"(\"\"|[^\"])+\")|('(\\\\.|[^\\\\])+')"),
    COLUMN_NAME("\\[(\\]\\]|[^\\]])+\\]"),
    OPERATOR("(<>|<=|>=|==|!=|&&|\\|\\||-|\\+|\\*|/|%|>|<|\\^|=|AND|OR)"),
    UNARY_OPERATOR("(NOT|-|!|IS NOT NULL|IS NULL|IS NOT EMPTY|IS EMPTY|IS NOTHING|IS NOT NOTHING)"),
    BRACKET_START("\\("),
    BRACKET_END("\\)"),
    FUNCTION("[A-Z][A-Z0-9_]*\\("),
    COMMA(",");

    private final Pattern regex;

    TokenType(String pattern) {
      this.regex = Pattern.compile("^(" + pattern + ")", Pattern.CASE_INSENSITIVE);
    }
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

  public static List<Token> tokenise(String expression) throws IllegalArgumentException {
    int index = 0;
    int length = expression.length();

    List<Token> output = new ArrayList<>();
    while (index < length) {
      boolean matched = false;
      for (TokenType type : TokenType.values()) {
        var matcher = type.regex.matcher(expression.subSequence(index,length));
        if (matcher.find()) {
          var value = matcher.group();
          output.add(new Token(type, value));
          index += value.length();
          matched = true;
          break;
        }
      }
      if (!matched) {
        var matcher = WHITE_SPACE.matcher(expression.subSequence(index,length));
        if (matcher.find()) {
          index += matcher.group().length();
        } else {
          throw new IllegalArgumentException("Unexpected character at position " + index + ": " + expression.subSequence(index, length));
        }
      }
    }

    return resolveUnaryMinuses(output).stream()
        .map(ExpressionTokeniser::normaliseNumbers)
        .map(ExpressionTokeniser::normaliseStrings)
        .map(ExpressionTokeniser::normaliseColumnNames)
        .map(ExpressionTokeniser::normaliseCaseAndNames)
        .collect(Collectors.toList());
  }

  private static List<Token> resolveUnaryMinuses(List<Token> tokens) {
    var output = new ArrayList<Token>();
    for (Token token : tokens) {
      if (token.equals(new Token(TokenType.OPERATOR, "-")) &&
          (output.size() == 0 || typeIn(output.get(output.size() - 1), TokenType.BRACKET_START, TokenType.COMMA, TokenType.FUNCTION, TokenType.OPERATOR))) {
        output.add(new Token(TokenType.UNARY_OPERATOR, "-"));
      } else if (token.type() == TokenType.NUMBER && output.size() > 0 && output.get(output.size() - 1).equals(new Token(TokenType.UNARY_OPERATOR, "-"))) {
        output.set(output.size() - 1, new Token(TokenType.NUMBER, "-" + token.value()));
      } else {
        output.add(token);
      }
    }
    return output;
  }

  private static Token normaliseNumbers(Token token) {
    if (token.type() == TokenType.NUMBER) {
      var value = token.value();
      var newValue = value.replace("_", "");
      return new Token(TokenType.NUMBER, newValue);
    } else {
      return token;
    }
  }

  private static Token normaliseColumnNames(Token token) {
    if (token.type() == TokenType.COLUMN_NAME) {
      var value = token.value();
      var newValue = value.substring(1, value.length() - 1).replace("]]", "]");
      return new Token(TokenType.COLUMN_NAME, newValue);
    } else {
      return token;
    }
  }

  private static Token normaliseStrings(Token token) {
    if (token.type() == TokenType.STRING) {
      var value = token.value();
      var newValue = (value.charAt(0) == '"')
          ? value.substring(1, value.length() - 1).replace("\"\"", "\"")
          : value.substring(1, value.length() - 1).replaceAll("\\\\(.)", "$1");
      return new Token(TokenType.STRING, newValue);
    } else {
      return token;
    }
  }

  private static Token normaliseCaseAndNames(Token token) {
    if (typeIn(token, TokenType.OPERATOR, TokenType.UNARY_OPERATOR, TokenType.BOOLEAN, TokenType.NOTHING)) {
      var value = token.value().toUpperCase().trim();
      var newValue = switch (value) {
        case "=" -> "==";
        case "<>" -> "!=";
        case "AND" -> "&&";
        case "OR" -> "||";
        case "NULL", "NOTHING" -> "Nothing";
        case "TRUE" -> "True";
        case "FALSE" -> "False";
        default -> value;
      };
      return new Token(token.type(), newValue);
    } else if (token.type() == TokenType.FUNCTION) {
      var value = token.value().toLowerCase().trim();
      return new Token(TokenType.FUNCTION, value);
    } else {
      return token;
    }
  }

  private static boolean typeIn(Token token, TokenType... types) {
    for (var type : types) {
      if (token.type() == type) {
        return true;
      }
    }
    return false;
  }
}
