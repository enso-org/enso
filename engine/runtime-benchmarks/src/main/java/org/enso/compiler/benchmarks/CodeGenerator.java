package org.enso.compiler.benchmarks;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

// TODO: Make sure all identifiers are used, so that there is no unused variable warning
public class CodeGenerator {
  private static final int MAX_IDENT_SIZE = 10;
  private static final int MAX_TEXT_LITERAL_SIZE = 20;
  private static final int SMALL_LETTERS_CNT = 26;
  private static final int SEED = 42;

  private static final List<String> OPERATORS = List.of(
      "+", "-", "*", "/", "&&", "||", "!", "==", "!=", "<", ">", "<=", ">="
  );

  private static final List<String> METHODS = List.of(
      "map", "filter", "foldl", "foldr", "head", "tail", "init", "last", "length", "reverse"
  );

  private final Random random = new Random(SEED);
  private int identifierCnt = 0;


  List<String> createIdentifiers(int count) {
    List<String> idents = new ArrayList<>(count);
    for (int i = 0; i < count; i++) {
      idents.add(nextIdentifier());
    }
    return idents;
  }

  private String nextIdentifier() {
    identifierCnt++;
    return "operator" + identifierCnt;
  }

  private String nextMethod() {
    var idx = random.nextInt(METHODS.size());
    return METHODS.get(idx);
  }

  private String nextTextLiteral() {
    var size = random.nextInt(MAX_TEXT_LITERAL_SIZE);
    var sb = new StringBuilder();
    for (int i = 0; i < size; i++) {
      var rndChar = (char) ('a' + random.nextInt(SMALL_LETTERS_CNT));
      sb.append(rndChar);
    }
    return sb.toString();
  }

  private String nextIntLiteral() {
    return Integer.toString(random.nextInt());
  }

  private String nextDecimalLiteral() {
    var part1 = random.nextInt();
    var decimalPart = Math.abs(random.nextInt());
    return part1 + "." + decimalPart;
  }

  private String nextLiteral() {
    var rndInt = random.nextInt(3);
    return switch (rndInt) {
      case 0 -> nextTextLiteral();
      case 1 -> nextIntLiteral();
      case 2 -> nextDecimalLiteral();
      default -> throw new UnsupportedOperationException("unimplemented");
    };
  }

  String createExpression(List<String> identifiers, int size) {
    switch (size) {
      // Literal
      case 0 -> {
        return nextLiteral();
      }
      // Either a single identifier or a method call on the identifier
      case 1 -> {
        var sb = new StringBuilder();
        var ident = chooseIdentifier(identifiers);
        sb.append(ident);
        var shouldCallMethod = random.nextBoolean();
        if (shouldCallMethod) {
          sb.append(".")
            .append(nextMethod());
        }
        return sb.toString();
      }
      // Method call or binary operator
      case 2 -> {
        var sb = new StringBuilder();
        var shouldCallMethod = random.nextBoolean();
        var ident1 = chooseIdentifier(identifiers);
        if (shouldCallMethod) {
          var methodArg = createExpression(identifiers, 1);
          sb.append(ident1)
              .append(".")
              .append(nextMethod())
              .append(" (")
              .append(methodArg)
              .append(")");
        } else {
          var ident2 = chooseIdentifier(identifiers);
          // Binary operator
          sb.append(ident1)
              .append(nextOperator())
              .append(ident2);
        }
        return sb.toString();
      }
      // Split into two expressions with random size
      default -> {
        var sb = new StringBuilder();
        var shouldCallMethod = random.nextBoolean();
        if (shouldCallMethod) {
          var ident = chooseIdentifier(identifiers);
          var methodArity = size - 1;
          List<String> methodArgs = new ArrayList<>();
          for (int i = 0; i < methodArity; i++) {
            methodArgs.add(
                createExpression(identifiers, size - 1)
            );
          }
          sb.append(ident)
              .append(".")
              .append(nextMethod())
              .append(" ");
          for (var methodArg : methodArgs) {
            sb.append(methodArg)
                .append(" ");
          }
        } else {
          var rndIdx = Math.max(2, random.nextInt(size));
          var size1 = rndIdx;
          var size2 = size - rndIdx;
          var expr1 = createExpression(identifiers, size1);
          var expr2 = createExpression(identifiers, size2);
          var op = nextOperator();
          sb.append("(")
              .append(expr1)
              .append(")")
              .append(op)
              .append("(")
              .append(expr2)
              .append(")");
        }
        return sb.toString();
      }
    }
  }

  private String nextOperator() {
    var idx = random.nextInt(OPERATORS.size());
    return OPERATORS.get(idx);
  }

  private String chooseIdentifier(List<String> identifiers) {
    assert !identifiers.isEmpty();
    var randomIdx = random.nextInt(identifiers.size());
    return identifiers.get(randomIdx);
  }

}
