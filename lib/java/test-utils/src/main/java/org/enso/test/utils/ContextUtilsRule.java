package org.enso.test.utils;

import java.io.ByteArrayOutputStream;
import java.util.function.Supplier;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;

/**
 * Test rule that ensures that {@link Context} is initialized and discarded in tests. If used as
 * {@link org.junit.ClassRule}, {@link Context} will be initialized just once for the whole test
 * class. If used as {@link org.junit.Rule}, a new {@link Context} will be initialized for each test
 * method.
 */
public final class ContextUtilsRule implements TestRule {
  private static final ThreadLocal<Context> CURRENT = new ThreadLocal<>();
  private final Supplier<Context> contextSupplier;
  private final ByteArrayOutputStream out;

  private ContextUtilsRule(Supplier<Context> contextSupplier, ByteArrayOutputStream out) {
    this.contextSupplier = contextSupplier;
    this.out = out;
  }

  public static ContextUtilsRule createDefault() {
    var out = new ByteArrayOutputStream();
    Supplier<Context> supplier =
        () -> {
          return ContextUtils.defaultContextBuilder().out(out).err(out).build();
        };
    return new ContextUtilsRule(supplier, out);
  }

  public static ContextUtilsRule createCustom(Supplier<Context> contextSupplier) {
    return new ContextUtilsRule(contextSupplier, null);
  }

  @Override
  public Statement apply(Statement base, Description description) {
    return new CustomStatement(base, description);
  }

  public Value evalModule(CharSequence src) {
    var ctx = currentCtx();
    return ContextUtils.evalModule(ctx, src);
  }

  public Value eval(Source src) {
    return currentCtx().eval(src);
  }

  private static Context currentCtx() {
    var ctx = CURRENT.get();
    assert ctx != null : "ContextUtilsRule must be used with @ClassRule or @Rule";
    return ctx;
  }

  private final class CustomStatement extends Statement {
    private final Statement base;
    private final Description description;

    private CustomStatement(Statement base, Description description) {
      this.base = base;
      this.description = description;
    }

    @Override
    public void evaluate() {
      log(description);
      var prev = CURRENT.get();
      try (var ctx = contextSupplier.get()) {
        System.out.println("[ContextUtilsRule] Creating new Context");
        CURRENT.set(ctx);
        base.evaluate();
      } catch (Throwable t) {
        throw new FailureWithOutput("Compiler output: " + out.toString(), t);
      } finally {
        CURRENT.set(prev);
      }
    }

    private static void log(Description descr) {
      System.out.printf(
          "[ContextUtilsRule] Running description: className=%s, methodName=%s, displayName=%s,"
              + " testCount=%d %n",
          descr.getClassName(), descr.getMethodName(), descr.getDisplayName(), descr.testCount());
    }
  }

  private static final class FailureWithOutput extends RuntimeException {
    private FailureWithOutput(String out, Throwable cause) {
      super(out, cause);
    }
  }
}
