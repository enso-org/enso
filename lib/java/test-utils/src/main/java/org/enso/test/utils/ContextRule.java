package org.enso.test.utils;

import com.oracle.truffle.api.nodes.Node;
import java.io.ByteArrayOutputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.function.Function;
import java.util.logging.Level;
import org.enso.common.LanguageInfo;
import org.enso.common.RuntimeOptions;
import org.enso.interpreter.runtime.EnsoContext;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;
import org.junit.rules.TestRule;
import org.junit.runner.Description;
import org.junit.runners.model.Statement;

/**
 * This class ensures that a polyglot {@link Context} is properly initialized and disposed along
 * with its resources. It can be used both as a field in a test annotated with jUnit rule ({@link
 * org.junit.ClassRule} or {@link org.junit.Rule}), or as a simple {@link AutoCloseable} resource.
 *
 * <p>Along with the simple functionality of {@link Context#initialize(String) initializing the
 * context} and {@link Context#close() closing it}, this class also contains various useful utility
 * methods specific for the Enso language, like {@link #getMethodFromModule(String, String)}.
 *
 * <p>Moreover, the output (stdout, stderr, loghandler) of the context is automatically captured and
 * can be accessed via {@link #getOut()}, and cleared with {@link #resetOut()}. The output usually
 * comes from logging inside the interpreter and the compiler, as well as from {@code IO.println}
 * used from Enso.
 *
 * <p>To configure initialization of the context, use {@link Builder#withModifiedContext(Function)}.
 *
 * <p>All the methods in this class that execute some Enso code, e.g., {@link
 * #evalModule(CharSequence)}, are, by default, guaranteed to {@link Context#enter() enter} and
 * {@link Context#leave() leave} the context. If this behavior is not desired, use {@link
 * Builder#alwaysExecuteInContext(boolean)} to disable it.
 *
 * <p>If used as {@link org.junit.ClassRule}, {@link Context} will be initialized just once for the
 * whole test class. If used as {@link org.junit.Rule}, a new {@link Context} will be initialized
 * for each test method.
 *
 * <p>Delegates most of the methods directly to {@link Context}.
 */
public final class ContextRule implements TestRule, AutoCloseable {
  private final ByteArrayOutputStream stdOut;
  private final ByteArrayOutputStream stdErr;
  private final Context.Builder ctxBldr;
  private final boolean alwaysExecuteInContext;
  private Context context;

  private ContextRule(
      Context.Builder ctxBldr,
      ByteArrayOutputStream stdOut,
      ByteArrayOutputStream stdErr,
      boolean alwaysExecuteInContext) {
    this.stdOut = Objects.requireNonNull(stdOut);
    this.stdErr = Objects.requireNonNull(stdErr);
    this.ctxBldr = Objects.requireNonNull(ctxBldr);
    this.alwaysExecuteInContext = alwaysExecuteInContext;
  }

  /**
   * The created builder starts with the <emph>default</emph> context. The default context is
   * roughly equivalent to the one that is created for standard command line execution via engine
   * runner.
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
    var ctxBldr = Builder.defaultContextBuilder();
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
  public void close() {
    if (context != null) {
      context.close();
      context = null;
    }
    resetOut();
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

  /**
   * Compiles a single module. Compiling two modules with the same name in the same context results
   * in undefined behavior.
   *
   * @param src Source code of the module. Can be arbitrary Enso code. If polyglot methods are used,
   *     ensure that the context was created with appropriate {@link #newBuilder(String...)
   *     permitted languages}.
   * @param moduleName Name of the module, may be qualified. Should start with uppercase letter.
   * @return IR of the module
   * @throws org.graalvm.polyglot.PolyglotException if compilation fails.
   */
  public org.enso.compiler.core.ir.Module compileModule(String src, String moduleName) {
    var source = Source.newBuilder(LanguageInfo.ID, src, moduleName + ".enso").buildLiteral();
    var ctx = currentCtx();
    var module = ctx.eval(source);
    var runtimeMod = (org.enso.interpreter.runtime.Module) unwrapValue(module);
    if (runtimeMod.getIr() == null) {
      runtimeMod.compileScope(leakContext());
    }
    return runtimeMod.getIr();
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

  private Context currentCtx() {
    if (context == null) {
      context = ctxBldr.build();
    }
    return context;
  }

  public static final class Builder {
    private Context.Builder polyglotCtxBldr;
    private final ByteArrayOutputStream stdout = new ByteArrayOutputStream();
    private final ByteArrayOutputStream stderr = new ByteArrayOutputStream();
    private boolean alwaysExecuteInContext = true;

    private Builder(String... permittedLanguages) {
      this.polyglotCtxBldr = defaultContextBuilder(permittedLanguages);
      this.polyglotCtxBldr.out(stdout).err(stderr).logHandler(stdout);
    }

    private static Context.Builder defaultContextBuilder(String... permittedLanguages) {
      return Context.newBuilder(permittedLanguages)
          .allowExperimentalOptions(true)
          .allowIO(IOAccess.ALL)
          .allowAllAccess(true)
          .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
          .option(RuntimeOptions.DISABLE_IR_CACHES, "true")
          .option(RuntimeOptions.STRICT_ERRORS, "true")
          .option(
              RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
              Paths.get("../../distribution/component").toFile().getAbsolutePath());
    }

    public Builder withModifiedContext(Function<Context.Builder, Context.Builder> modifier) {
      polyglotCtxBldr = modifier.apply(polyglotCtxBldr);
      return this;
    }

    /**
     * Shortcut for {@code withModifiedContext(b -> b.option(RuntimeOptions.PROJECT_ROOT,
     * projRoot)}.
     */
    public Builder withProjectRoot(Path projectRootDir) {
      polyglotCtxBldr.option(
          RuntimeOptions.PROJECT_ROOT, projectRootDir.toAbsolutePath().toString());
      return this;
    }

    /**
     * Whether the code in the tests should be executed in the polyglot context. A necessity for
     * executing artificially created Truffle nodes. This basically ensures that executing {@link
     * EnsoContext#get(Node)} will always return non-null value in the test.
     *
     * <p>Is true by default.
     *
     * @param b true for automatically wrapping the test code in the context. If false, the context
     *     entering must be done manually.
     */
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

    /** Evaluates jUnit {@link org.junit.Test}. */
    @Override
    public void evaluate() throws Throwable {
      try (var ctx = currentCtx()) {
        if (alwaysExecuteInContext) {
          ContextUtils.executeInContext(
              ctx,
              () -> {
                try {
                  base.evaluate();
                } catch (Throwable e) {
                  throw new RuntimeException(e);
                }
                return null;
              });
        } else {
          base.evaluate();
        }
      } catch (Throwable t) {
        throw new FailureWithOutput("Compiler output: " + stdOut, t);
      } finally {
        close();
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
