package org.enso.compiler.benchmarks;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

public class CodeGenerator {
  private static final int MAX_TEXT_LITERAL_SIZE = 20;
  private static final int SMALL_LETTERS_CNT = 26;
  private static final int SEED = 42;

  private static final List<String> OPERATORS =
      List.of("+", "-", "*", "/", "&&", "||", "==", "!=", "<", ">", "<=", ">=");

  private static final List<String> METHODS =
      List.of(
          "map", "filter", "foldl", "foldr", "head", "tail", "init", "last", "length", "reverse");

  /**
   * A collection of identifiers that were defined. I.e., for these identifiers, there exist
   * assignments in the generated code.
   */
  private final List<String> definedIdentifiers = new ArrayList<>();

  /** A collection of identifiers that were used in some expression. */
  private final Set<String> usedIdentifiers = new HashSet<>();

  private final Random random = new Random(SEED);
  private int identifierCnt = 0;

  /** Creates a new empty code generator. */
  public CodeGenerator() {}

  /**
   * @param identifiers Collection of already defined identifiers, like arguments to a function.
   */
  public CodeGenerator(Collection<String> identifiers) {
    definedIdentifiers.addAll(identifiers);
  }

  /**
   * Creates an expression that initializes a new variable.
   *
   * @param expressionSize Size of the expression that will be used to initialize the variable.
   * @return The assignment expression that defines the new variable
   */
  public String defineNewVariable(int expressionSize) {
    assert expressionSize >= 0;
    var ident = nextIdentifier();
    definedIdentifiers.add(ident);
    var initExpr = createExpressionFromDefinedIdentifiers(expressionSize);
    return ident + " = " + initExpr;
  }

  /** Returns a new identifiers and records it as a defined variable. */
  public String defineNewVariable() {
    var ident = nextIdentifier();
    definedIdentifiers.add(ident);
    return ident;
  }

  /**
   * Randomly chooses one of the defined identifiers.
   *
   * @return The chosen identifier.
   */
  public String chooseDefinedIdentifier() {
    assert !definedIdentifiers.isEmpty();
    var randomIdx = random.nextInt(definedIdentifiers.size());
    return definedIdentifiers.get(randomIdx);
  }

  /**
   * Returns a collection of identifiers that are not used in any expression. These identifiers were
   * defined, but not used, so the compiler should generate a warning about them.
   *
   * @return A collection of unused identifiers.
   */
  public Set<String> getUnusedIdentifiers() {
    var allDefined = new HashSet<>(definedIdentifiers);
    allDefined.removeAll(usedIdentifiers);
    return allDefined;
  }

  public void markVariableAsUsed(String variable) {
    assert definedIdentifiers.contains(variable);
    usedIdentifiers.add(variable);
  }

  public List<String> createIdentifiers(int count) {
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
    return "\"" + sb + "\"";
  }

  private String nextIntLiteral() {
    return Integer.toString(random.nextInt());
  }

  private String nextDecimalLiteral() {
    var part1 = random.nextInt();
    var decimalPart = Math.abs(random.nextInt());
    return part1 + "." + decimalPart;
  }

  public String nextLiteral() {
    var rndInt = random.nextInt(3);
    return switch (rndInt) {
      case 0 -> nextTextLiteral();
      case 1 -> nextIntLiteral();
      case 2 -> nextDecimalLiteral();
      default -> throw new UnsupportedOperationException("unimplemented");
    };
  }

  /**
   * Creates an expression using only the defined identifiers.
   *
   * @param size Arity of the expression.
   */
  public String createExpressionFromDefinedIdentifiers(int size) {
    return createExpression(definedIdentifiers, size);
  }

  /**
   * Creates an expression with the given size.
   *
   * @param identifiers A collection of all the identifiers that can be used in the expression.
   *     These may be undefined identifiers. In that case, the compiler should throw an error.
   * @param size Arity of the expression.
   * @return A string representing the expression.
   */
  public String createExpression(List<String> identifiers, int size) {
    if (identifiers.isEmpty()) {
      return nextLiteral();
    }

    return switch (size) {
      case 0 -> nextLiteral();
        // Either a single identifier or a method call on the identifier
      case 1 -> {
        var sb = new StringBuilder();
        var ident = chooseIdentifier(identifiers);
        usedIdentifiers.add(ident);
        sb.append(ident);
        var shouldCallMethod = random.nextBoolean();
        if (shouldCallMethod) {
          sb.append(".").append(nextMethod());
        }
        yield sb.toString();
      }
        // Method call or binary operator
      case 2 -> {
        var sb = new StringBuilder();
        var shouldCallMethod = random.nextBoolean();
        var ident1 = chooseIdentifier(identifiers);
        usedIdentifiers.add(ident1);
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
          usedIdentifiers.add(ident2);
          // Binary operator
          sb.append(ident1).append(nextOperator()).append(ident2);
        }
        yield sb.toString();
      }
        // Split into two expressions with random size
      default -> {
        var sb = new StringBuilder();
        var shouldCallMethod = random.nextBoolean();
        if (shouldCallMethod) {
          var ident = chooseIdentifier(identifiers);
          usedIdentifiers.add(ident);
          var methodArity = size - 1;
          List<String> methodArgs = new ArrayList<>();
          for (int i = 0; i < methodArity; i++) {
            methodArgs.add(createExpression(identifiers, size - 1));
          }
          sb.append(ident).append(".").append(nextMethod()).append(" ");
          for (var methodArg : methodArgs) {
            sb.append(methodArg).append(" ");
          }
        } else {
          var rndIdx = Math.max(2, random.nextInt(size));
          var size1 = rndIdx;
          var size2 = size - rndIdx;
          var expr1 = createExpression(identifiers, size1);
          var expr2 = createExpression(identifiers, size2);
          var op = nextOperator();
          sb.append("(").append(expr1).append(")").append(op).append("(").append(expr2).append(")");
        }
        yield sb.toString();
      }
    };
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
