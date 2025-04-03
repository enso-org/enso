package org.enso.test.utils;

import java.io.ByteArrayOutputStream;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.function.Supplier;
import org.enso.interpreter.runtime.EnsoContext;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
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
 *
 * <p>This class simply delegates most of the methods either directly to {@link Context} or to
 * {@link ContextUtils}.
 *
 * <p>Note that {@link ContextRule} cannot be used inside methods annotated with {@link
 * org.junit.runners.Parameterized.Parameters}.
 */
public final class ContextRule implements TestRule {
  private static final ThreadLocal<Context> CURRENT = new ThreadLocal<>();
  private final Supplier<Context> contextSupplier;
  private final ByteArrayOutputStream out;

  private ContextRule(Supplier<Context> contextSupplier, ByteArrayOutputStream out) {
    this.contextSupplier = contextSupplier;
    this.out = out;
  }

  public static ContextRule createDefault() {
    var out = new ByteArrayOutputStream();
    Supplier<Context> supplier =
        () -> {
          return ContextUtils.defaultContextBuilder().out(out).err(out).build();
        };
    return new ContextRule(supplier, out);
  }

  public static ContextRule createCustom(Supplier<Context> contextSupplier) {
    return new ContextRule(contextSupplier, null);
  }

  public static ContextRule createWithCapturedOut(ByteArrayOutputStream out) {
    Supplier<Context> supplier = () -> ContextUtils.createDefaultContext(out);
    return new ContextRule(supplier, out);
  }

  @Override
  public Statement apply(Statement base, Description description) {
    return new CustomStatement(base, description);
  }

  /**
   * @see ContextUtils#evalModule(Context, CharSequence)
   */
  public Value evalModule(CharSequence src) {
    var ctx = currentCtx();
    return ContextUtils.evalModule(ctx, src);
  }

  public Context context() {
    return currentCtx();
  }

  /**
   * @see ContextUtils#leakContext(Context)
   */
  public EnsoContext leakContext() {
    var ctx = currentCtx();
    return ContextUtils.leakContext(ctx);
  }

  public Engine getEngine() {
    return currentCtx().getEngine();
  }

  /**
   * @see ContextUtils#evalModule(Context, CharSequence, String, String)
   */
  public Value evalModule(CharSequence src, String name, String methodName) {
    return ContextUtils.evalModule(currentCtx(), src, name, methodName);
  }

  /**
   * @see ContextUtils#evalModule(Context, Source, String)
   */
  public Value evalModule(Source src, String methodName) {
    return ContextUtils.evalModule(currentCtx(), src, methodName);
  }

  public Value eval(Source src) {
    return currentCtx().eval(src);
  }

  public Value eval(String languageId, CharSequence code) {
    return currentCtx().eval(languageId, code);
  }

  /**
   * @see ContextUtils#evalModule(Context, CharSequence, String)
   */
  public Object unwrapValue(Value value) {
    return ContextUtils.unwrapValue(currentCtx(), value);
  }

  public Value createValue(String src, String imports) {
    return ContextUtils.createValue(currentCtx(), src, imports);
  }

  public Value createValue(String src) {
    return ContextUtils.createValue(currentCtx(), src);
  }

  public Value asValue(Object obj) {
    return currentCtx().asValue(obj);
  }

  /**
   * @see ContextUtils#executeInContext(Context, Callable)
   */
  public <T> Value executeInContext(Callable<T> callable) {
    var ctx = currentCtx();
    return ContextUtils.executeInContext(ctx, callable);
  }

  private static Context currentCtx() {
    var ctx = CURRENT.get();
    assert ctx != null : "ContextUtilsRule must be used with @ClassRule or @Rule";
    return ctx;
  }

  /**
   * Parses the given module and returns a method by the given name from the module.
   *
   * @param moduleSrc Source of the whole module
   * @return Reference to the method.
   */
  public Value getMethodFromModule(String moduleSrc, String methodName) {
    return ContextUtils.getMethodFromModule(currentCtx(), moduleSrc, methodName);
  }

  /**
   * Returns set of all the builtin methods from Any. These methods are present even if the module
   * was not imported - they are present on the Any builtin type. This is in contrast to {@link
   * #allMethodsFromAny()} which requires the {@code Standard.Base.Any} module to be first
   * imported.
   */
  public Set<String> builtinMethodsFromAny() {
    return ContextUtils.builtinMethodsFromAny(currentCtx());
  }

  /**
   * Returns set of all the methods on the {@code Standard.Base.Any} type. This includes both
   * builtin and non-builtin types. For this to work, {@code Standard.Base.Any} module must be
   * imported first in the context, otherwise an assertion will fail.
   */
  public Set<String> allMethodsFromAny() {
    return ContextUtils.allMethodsFromAny(currentCtx());
  }

  private final class CustomStatement extends Statement {
    private final Statement base;
    private final Description description;

    private CustomStatement(Statement base, Description description) {
      this.base = base;
      this.description = description;
    }

    @Override
    public void evaluate() throws Throwable {
      var prev = CURRENT.get();
      try (var ctx = contextSupplier.get()) {
        System.out.println("[ContextUtilsRule] Creating new Context");
        CURRENT.set(ctx);
        base.evaluate();
      } catch (Throwable t) {
        if (out != null) {
          throw new FailureWithOutput("Compiler output: " + out, t);
        } else {
          throw t;
        }
      } finally {
        if (out != null) {
          out.reset();
        }
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
