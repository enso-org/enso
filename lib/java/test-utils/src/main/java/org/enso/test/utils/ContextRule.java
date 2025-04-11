package org.enso.test.utils;

import java.io.ByteArrayOutputStream;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.function.Function;
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
 */
public final class ContextRule implements TestRule {
  private final ByteArrayOutputStream stdOut;
  private final ByteArrayOutputStream stdErr;
  private final Context.Builder ctxBldr;
  private Context context;

  private ContextRule(
      Context.Builder ctxBldr,
      ByteArrayOutputStream stdOut,
      ByteArrayOutputStream stdErr) {
    this.stdOut = Objects.requireNonNull(stdOut);
    this.stdErr = Objects.requireNonNull(stdErr);
    this.ctxBldr = Objects.requireNonNull(ctxBldr);
  }

  /**
   * The created builder starts with {@link ContextUtils#defaultContextBuilder(String...)} default
   * polyglot context builder.
   *
   * @param permittedLanguages List of languages that are allowed to be used in the context. If
   *     empty, all installed languages are enabled.
   * @see Context#newBuilder(String...)
   */
  public static Builder newBuilder(String... permittedLanguages) {
    return new Builder(permittedLanguages);
  }

  /** Shortcut for {@code ContextRule.newBuilder().build()}. */
  public static ContextRule createDefault() {
    var stdout = new ByteArrayOutputStream();
    var stderr = new ByteArrayOutputStream();
    var ctxBldr = ContextUtils.defaultContextBuilder();
    ctxBldr.out(stdout).err(stderr).logHandler(stdout);
    return new ContextRule(ctxBldr, stdout, stderr, true);
  }

  /**
   * Returns the combined stdout and stderr streams captured by this rule. Shortcut for {@code
   * getStdOut() + getStdErr()}.
   */
  public String getOut() {
    return stdOut + stdErr.toString();
  }

  /** Returns the stdout stream captured by this rule. */
  public String getStdOut() {
    return stdOut.toString();
  }

  /** Returns the stderr stream captured by this rule. */
  public String getStdErr() {
    return stdErr.toString();
  }

  /**
   * Resets (clears) ste stdout and stderr streams captured by this rule. This may be handy if the
   * rule is annotated with {@link org.junit.ClassRule}, and you need to clean the output after
   * every test in {@link org.junit.After} method.
   */
  public void resetOut() {
    stdOut.reset();
    stdErr.reset();
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

  private Context currentCtx() {
    if (context == null) {
      context = ctxBldr.build();
    }
    return context;
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
   * #allMethodsFromAny()} which requires the {@code Standard.Base.Any} module to be first imported.
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

  public static final class Builder {
    private Context.Builder polyglotCtxBldr;
    private final ByteArrayOutputStream stdout = new ByteArrayOutputStream();
    private final ByteArrayOutputStream stderr = new ByteArrayOutputStream();

    private Builder(String... permittedLanguages) {
      this.polyglotCtxBldr = ContextUtils.defaultContextBuilder(permittedLanguages);
      this.polyglotCtxBldr.out(stdout).err(stderr).logHandler(stdout);
    }

    public Builder withModifiedContext(Function<Context.Builder, Context.Builder> modifier) {
      polyglotCtxBldr = modifier.apply(polyglotCtxBldr);
      return this;
    }

    public Builder alwaysExecuteInContext(boolean b) {
      this.alwaysExecuteInContext = b;
      return this;
    }

    public ContextRule build() {
      return new ContextRule(polyglotCtxBldr, stdout, stderr, alwaysExecuteInContext);
    }
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
      try (var ctx = currentCtx()) {
        base.evaluate();
      } catch (Throwable t) {
        throw new FailureWithOutput("Compiler output: " + stdOut, t);
      } finally {
        if (context != null) {
          context.close();
          context = null;
        }
        resetOut();
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
