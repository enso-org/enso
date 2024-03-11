package org.enso.testkit;

import java.util.HashSet;
import java.util.Set;
import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;

/**
 * Flaky test specification for JUnit.
 *
 * <p>Inspired by <a href="https://stackoverflow.com/a/8301639/4816269">this SO answer</a>.
 *
 * <p>Use it like this:
 *
 * <pre>
 * public class MyTest {
 *   &#64;Rule
 *   public RetryTestRule retry = new RetryTestRule(3);
 *   &#64;Test
 *   public void myTest() {...}
 * }
 * </pre>
 */
public class RetryTestRule implements TestRule {
  private int retryCount;

  public RetryTestRule(int retryCount) {
    this.retryCount = retryCount;
  }

  @Override
  public Statement apply(Statement base, Description description) {
    return statement(base, description);
  }

  private Statement statement(final Statement base, final Description description) {
    return new Statement() {
      @Override
      public void evaluate() {
        Set<Throwable> caughtThrowables = new HashSet<>();

        for (int i = 0; i < retryCount; i++) {
          try {
            base.evaluate();
            return;
          } catch (Throwable t) {
            caughtThrowables.add(t);
            System.err.println(
                description.getClassName()
                    + "."
                    + description.getDisplayName()
                    + ": run "
                    + (i + 1)
                    + " failed");
          }
        }
        var err =
            new AssertionError(
                description.getDisplayName() + ": giving up after " + retryCount + " failures");
        for (var t : caughtThrowables) {
          Throwable n = err;
          while (n.getCause() != null) {
            n = n.getCause();
          }
          n.initCause(t);
        }
        System.err.println(
            description.getDisplayName() + ": giving up after " + retryCount + " failures");
        throw err;
      }
    };
  }
}
