package org.enso.compiler;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.enso.interpreter.test.TestBase;
import org.graalvm.polyglot.Context;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.experimental.theories.DataPoints;
import org.junit.experimental.theories.Theories;
import org.junit.experimental.theories.Theory;
import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runner.RunWith;
import org.junit.runners.model.Statement;

/**
 * If a symbol is importable by an import statement, then it should also be accessible via FQN
 * without any import, or with an import of its parent module. This rule is enforced by this test.
 * It uses symbols from standard libraries.
 */
@RunWith(Theories.class)
public class ImportsAndFQNConsistencyTest extends TestBase {
  private static Context ctx;

  /** Used for description in {@link PrintCodeRule} test rule. */
  private static String code;

  @Rule public final TestRule printCodeRule = new PrintCodeRule();

  @DataPoints
  public static final List<Symbol> symbols =
      List.of(new Symbol("Standard.Base.Data.Vector"), new Symbol("Standard.Table.Data.Table"));

  @BeforeClass
  public static void initCtx() {
    ctx = TestBase.createDefaultContext();
  }

  @AfterClass
  public static void disposeCtx() {
    ctx.close();
  }

  private void evalCode(Symbol symbol) {
    var res = TestBase.evalModule(ctx, code);
    assertThat(res.isString(), is(true));
    assertThat(res.asString(), is(symbol.getTypeName()));
  }

  /** Tests that a symbol can be used via its simple name with an FQN import of that symbol. */
  @Theory
  public void testSymbolCanBeAccessedBySimpleNameWithFQNImport(Symbol symbol) {
    var sb = new StringBuilder();
    sb.append("from ")
        .append(symbol.getLibName())
        .append(" import ")
        .append(symbol.getTypeName())
        .append(System.lineSeparator());
    sb.append("main = ")
        .append(symbol.getTypeName())
        .append(".to_text")
        .append(System.lineSeparator());
    code = sb.toString();
    evalCode(symbol);
  }

  /** Tests that a symbol can be used via FQN with an FQN import of that symbol. */
  @Theory
  public void testSymbolCanBeAccessedByFQNWithFQNImport(Symbol symbol) {
    var sb = new StringBuilder();
    sb.append("from ")
        .append(symbol.getLibName())
        .append(" import ")
        .append(symbol.getTypeName())
        .append(System.lineSeparator());
    sb.append("main = ").append(symbol.getFqn()).append(".to_text").append(System.lineSeparator());
    code = sb.toString();
    evalCode(symbol);
  }

  /** Tests that a symbol can be used via FQN with an import of the library. */
  @Theory
  public void testSymbolCanBeAccessedByFQNWithLibImport(Symbol symbol) {
    var sb = new StringBuilder();
    sb.append("import ").append(symbol.getLibName()).append(System.lineSeparator());
    sb.append("main = ").append(symbol.getFqn()).append(".to_text").append(System.lineSeparator());
    code = sb.toString();
    evalCode(symbol);
  }

  public static class Symbol {
    private final String fqn;
    private final List<String> pathItems;

    private Symbol(String fqn) {
      this.fqn = fqn;
      this.pathItems = Arrays.stream(fqn.split("\\.")).collect(Collectors.toList());
      assertThat(pathItems, hasSize(greaterThan(2)));
    }

    String getFqn() {
      return fqn;
    }

    String getLibName() {
      return pathItems.get(0) + "." + pathItems.get(1);
    }

    String getTypeName() {
      return pathItems.get(pathItems.size() - 1);
    }

    /** Returns qualified typename, without the library prefix. */
    String getQualifiedTypeName() {
      return String.join(".", pathItems.subList(2, pathItems.size()));
    }

    @Override
    public String toString() {
      return fqn;
    }
  }

  /**
   * A simple JUnit rule that attaches the evaluated Enso code that failed into the {@link
   * AssertionError}.
   */
  public static final class PrintCodeRule implements TestRule {
    @Override
    public Statement apply(Statement base, Description description) {
      return new Statement() {
        @Override
        public void evaluate() {
          try {
            base.evaluate();
          } catch (Throwable e) {
            String msg = "Test failed executing code:" + System.lineSeparator() + code;
            throw new AssertionError(msg, e);
          }
        }
      };
    }
  }
}
