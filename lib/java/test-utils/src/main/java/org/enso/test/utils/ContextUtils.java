package org.enso.test.utils;

import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import java.io.ByteArrayOutputStream;
import java.lang.ref.Reference;
import java.lang.ref.WeakReference;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.function.Function;
import java.util.logging.Level;
import java.util.stream.Collectors;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.common.MethodNames.Module;
import org.enso.common.MethodNames.TopScope;
import org.enso.common.RuntimeOptions;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.polyglot.PolyglotContext;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Engine;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;
import org.graalvm.polyglot.proxy.ProxyExecutable;
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
public final class ContextUtils implements TestRule, AutoCloseable {
  private final ByteArrayOutputStream stdOut;
  private final ByteArrayOutputStream stdErr;
  private final Context.Builder ctxBldr;
  private final boolean alwaysExecuteInContext;
  private final Boolean assertGC;
  private Context context;

  private ContextUtils(
      Context.Builder ctxBldr,
      ByteArrayOutputStream stdOut,
      ByteArrayOutputStream stdErr,
      boolean alwaysExecuteInContext,
      Boolean assertGC) {
    this.stdOut = Objects.requireNonNull(stdOut);
    this.stdErr = Objects.requireNonNull(stdErr);
    this.ctxBldr = Objects.requireNonNull(ctxBldr);
    this.alwaysExecuteInContext = alwaysExecuteInContext;
    this.assertGC = assertGC;
  }

  /**
   * The created builder starts with the <em>default</em> context. The default context is roughly
   * equivalent to the one that is created for standard command line execution via engine runner.
   *
   * @param permittedLanguages List of languages that are allowed to be used in the context. If
   *     empty, all installed languages are enabled.
   * @return new instance of builder
   * @see Context#newBuilder(String...)
   */
  public static Builder newBuilder(String... permittedLanguages) {
    return new Builder(permittedLanguages);
  }

  /** Shortcut for {@code ContextRule.newBuilder().build()}. */
  public static ContextUtils createDefault() {
    var stdout = new ByteArrayOutputStream();
    var stderr = new ByteArrayOutputStream();
    var ctxBldr = Builder.defaultContextBuilder();
    ctxBldr.out(stdout).err(stderr).logHandler(stdout);
    return new ContextUtils(ctxBldr, stdout, stderr, true, null);
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
    var checkGC = Boolean.TRUE.equals(assertGC);
    close(checkGC);
  }

  private void close(boolean checkGC) {
    if (context != null) {
      Reference<Object> ref = null;
      if (checkGC) {
        ref = new WeakReference<>(ensoContext());
      }
      context.close();
      context = null;
      if (ref != null) {
        MemoryUtils.assertGC("EnsoContext can be GCed when context is closed", true, ref);
      }
    }
    resetOut();
  }

  @Override
  public Statement apply(Statement base, Description description) {
    return new CustomStatement(base, description);
  }

  public Context context() {
    return currentCtx();
  }

  public org.enso.polyglot.TopScope topScope() {
    return new PolyglotContext(currentCtx()).getTopScope();
  }

  /** Leaks the underlying {@link EnsoContext} from this context. */
  public EnsoContext ensoContext() {
    var ctx = currentCtx();
    return ctx.getBindings(LanguageInfo.ID)
        .invokeMember(TopScope.LEAK_CONTEXT)
        .as(EnsoContext.class);
  }

  public Engine getEngine() {
    return currentCtx().getEngine();
  }

  /**
   * Evaluates the given source as if it was in a module with given name.
   *
   * @param src The source code of the module
   * @param moduleName Name of the module, for which the source will be created and evaluated.
   *     Should start with capital letter.
   * @param methodName Name of the method to invoke.
   * @return The value returned from the {@code methodName} method of the module.
   */
  public Value evalModule(CharSequence src, String moduleName, String methodName) {
    var source = Source.newBuilder(LanguageInfo.ID, src, moduleName).buildLiteral();
    return evalModule(source, methodName);
  }

  /**
   * Evaluates the given source as if it was in a module with given name.
   *
   * @param src The source code of the module
   * @param methodName name of main method to invoke
   * @return The value returned from the {@code methodName} method of the unnamed module.
   */
  public Value evalModule(CharSequence src, String methodName) {
    var source = Source.create(LanguageInfo.ID, src);
    return evalModule(source, methodName);
  }

  public Value evalModule(CharSequence src) {
    return evalModule(src, "main");
  }

  public Value evalModule(Source src, String methodName) {
    var module = currentCtx().eval(src);
    var assocType = module.invokeMember(Module.GET_ASSOCIATED_TYPE);
    var method = module.invokeMember(Module.GET_METHOD, assocType, methodName);
    return "main".equals(methodName) ? method.execute() : method.execute(assocType);
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
      runtimeMod.compileScope(ensoContext());
    }
    return runtimeMod.getIr();
  }

  public Value eval(Source src) {
    return currentCtx().eval(src);
  }

  public Value eval(String languageId, CharSequence code) {
    return currentCtx().eval(languageId, code);
  }

  /**
   * Unwraps the `receiver` field from the Value. This is a hack to allow us to test execute methods
   * of artificially created ASTs, e.g., single nodes. More specifically, only unwrapped values are
   * eligible to be passed to node's execute methods, we cannot pass {@link Value} directly to the
   * node's execute methods.
   *
   * <p>Does something similar to what {@code
   * com.oracle.truffle.tck.DebuggerTester#getSourceImpl(Source)} does, but uses a different hack
   * than reflective access.
   */
  public Object unwrapValue(Value value) {
    var unwrapper = new Unwrapper();
    var unwrapperValue = asValue(unwrapper);
    unwrapperValue.execute(value);
    assert unwrapper.args != null;
    return unwrapper.args[0];
  }

  /**
   * Creates an Enso value from the given source.
   *
   * @param src One-line assignment into a variable
   * @param imports Imports, may be empty.
   */
  public Value createValue(String src, String imports) {
    if (src.lines().count() > 1 || imports == null) {
      throw new IllegalArgumentException("src should have one line, imports must not be null");
    }
    var sb = new StringBuilder();
    sb.append(imports);
    sb.append(System.lineSeparator());
    sb.append("my_var = ").append(src);
    sb.append(System.lineSeparator());
    Value tmpModule = eval("enso", sb.toString());
    return tmpModule.invokeMember(MethodNames.Module.EVAL_EXPRESSION, "my_var");
  }

  public Value createValue(String src) {
    return createValue(src, "");
  }

  public Value asValue(Object obj) {
    return currentCtx().asValue(obj);
  }

  /**
   * Executes the given callable in the given context.A necessity for executing artificially created
   * Truffle ASTs.
   *
   * @param <T> type of the return value
   * @param callable action to invoke with given return type
   * @return Object returned from {@code callable} wrapped in {@link Value}.
   */
  private <T> Value executeInContext(Callable<T> callable) {
    var ctx = currentCtx();
    try {
      // Force initialization of the context
      ctx.eval("enso", "value = 0");
    } catch (Exception ex) {
      if (!"Access to language 'enso' is not permitted. ".equals(ex.getMessage())) {
        throw ex;
      }
    }
    var err = new Exception[1];
    ctx.getPolyglotBindings()
        .putMember(
            "testSymbol",
            (ProxyExecutable)
                (Value... args) -> {
                  try {
                    return callable.call();
                  } catch (Exception e) {
                    err[0] = e;
                    return null;
                  }
                });
    var res = ctx.getPolyglotBindings().getMember("testSymbol").execute();
    if (err[0] != null) {
      throw raise(RuntimeException.class, err[0]);
    }
    return res;
  }

  @SuppressWarnings("unchecked")
  private static <E extends Throwable> E raise(Class<E> clazz, Throwable t) throws E {
    throw (E) t;
  }

  /**
   * Parses the given module and returns a method by the given name from the module.
   *
   * @param moduleSrc Source of the whole module
   * @return Reference to the method.
   */
  public Value getMethodFromModule(String moduleSrc, String methodName) {
    var module = currentCtx().eval(LanguageInfo.ID, moduleSrc);
    Value method;
    if (methodName.equals("main")) {
      var assocType = module.invokeMember(Module.GET_ASSOCIATED_TYPE);
      method = module.invokeMember(Module.GET_METHOD, assocType, methodName);
    } else {
      method = module.invokeMember(Module.EVAL_EXPRESSION, methodName);
    }
    if (!method.canExecute()) {
      throw new AssertionError("Method " + method + " should be executable");
    }
    return method;
  }

  /**
   * Returns method reference. The module must already be compiled and loaded in the context.
   *
   * <p>Note that module methods cannot be accessed with this method.
   *
   * @param moduleName Fully qualified name of the module.
   * @param typeName Unqualified type name in the module.
   * @param methodName Unqualified method name in the type.
   * @return Reference to the method.
   */
  public Value getMethodFromLoadedModule(String moduleName, String typeName, String methodName) {
    var modOpt = ensoContext().getPackageRepository().getLoadedModule(moduleName);
    if (modOpt.isEmpty()) {
      throw new IllegalArgumentException("Module " + moduleName + " is not loaded");
    }
    var mod = modOpt.get();
    var runtimeMod = org.enso.interpreter.runtime.Module.fromCompilerModule(mod);
    var modScope = runtimeMod.getScope();
    var type = modScope.getType(typeName, true);
    if (type == null) {
      throw new IllegalArgumentException("Type " + typeName + " not found in module " + moduleName);
    }
    var method = modScope.getMethodForType(type, methodName);
    if (method == null) {
      throw new IllegalArgumentException(
          "Method " + methodName + " not found in type " + typeName + " in module " + moduleName);
    }
    return asValue(method);
  }

  /**
   * Returns set of all the builtin methods from Any. These methods are present even if the module
   * was not imported - they are present on the Any builtin type. This is in contrast to {@link
   * #allMethodsFromAny()} which requires the {@code Standard.Base.Any} module to be first imported.
   */
  public Set<String> builtinMethodsFromAny() {
    var ensoCtx = ensoContext();
    // This is a builtin Any type, so only the builtin methods will be included.
    var anyBuiltinType = ensoCtx.getBuiltins().any();
    var anyBuiltinMethods = anyBuiltinType.getDefinitionScope().getMethodsForType(anyBuiltinType);
    assert anyBuiltinMethods != null;
    return anyBuiltinMethods.stream()
        .map(m -> unqualifiedName(m.getName()))
        .collect(Collectors.toUnmodifiableSet());
  }

  private static String unqualifiedName(String name) {
    if (name.contains(".")) {
      return name.substring(name.lastIndexOf('.') + 1);
    }
    return name;
  }

  /**
   * Returns set of all the methods on the {@code Standard.Base.Any} type. This includes both
   * builtin and non-builtin types. For this to work, {@code Standard.Base.Any} module must be
   * imported first in the context, otherwise an assertion will fail.
   */
  public Set<String> allMethodsFromAny() {
    // Includes, e.g., `Any.to`.
    var ensoCtx = ensoContext();
    var anyMod = ensoCtx.findModule("Standard.Base.Any");
    assert anyMod.isPresent() : "Standard.Base.Any module must be imported first";
    var anyModScope = anyMod.get().getScope();
    var anyType = anyModScope.getType("Any", true);
    var anyMethods = anyModScope.getMethodsForType(anyType);
    assert anyMethods != null;
    return anyMethods.stream()
        .map(m -> unqualifiedName(m.getName()))
        .collect(Collectors.toUnmodifiableSet());
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
    private Boolean assertGC;

    private Builder(String... permittedLanguages) {
      this.polyglotCtxBldr = defaultContextBuilder(permittedLanguages);
      this.polyglotCtxBldr
          .out(stdout)
          .err(stderr)
          .logHandler(stdout)
          .environment("NO_COLOR", "true");
    }

    private static Context.Builder defaultContextBuilder(String... permittedLanguages) {
      return Context.newBuilder(permittedLanguages)
          .allowExperimentalOptions(true)
          .allowIO(IOAccess.ALL)
          .allowAllAccess(true)
          .environment("NO_COLOR", "true")
          .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
          .option(RuntimeOptions.CHECK_CWD, "false")
          .option(RuntimeOptions.DISABLE_IR_CACHES, "true")
          .option(RuntimeOptions.STRICT_ERRORS, "true")
          .option(
              RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
              Paths.get("../../distribution/component").toFile().getAbsolutePath());
    }

    /** Modifies the {@link Context.Builder}. Can be called multiple times. */
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
     * @return this builder
     */
    public Builder alwaysExecuteInContext(boolean b) {
      this.alwaysExecuteInContext = b;
      return this;
    }

    /**
     * Enables or disables an "assert GC" check to be performed at the end of {@link ContextUtils}
     * usage. Explicitly setting this flag overrides any defaults. Not calling this method leaves
     * the <em>default behavior</em> on:
     *
     * <ul>
     *   <li>when the context utils are used as a {@code @Rule} or {@code @ClassRule} then the
     *       "assert GC mode" is <strong>on by default</strong> and checked at the end of {@link
     *       ContextUtils#apply} invocation
     *   <li>when the context utils are used manually - for example in a <em>try with resources</em>
     *       block, then the "assert GC mode" is <strong>off by default</strong>
     * </ul>
     *
     * The motivation for the above described default behavior is based on presence of local
     * variables on stack - with manual usage, there are likely to be local variables and hold some
     * references when the {@link ContextUtils#close()} is called. Hence one has to opt-in to enable
     * the "assert GC mode". When used as JUnit rule, there are no local variables anymore and thus
     * the major source of "test only leaks" is avoided.
     *
     * @param check explicitly enables/disables GC check
     * @return this builder
     */
    public Builder assertGC(boolean check) {
      this.assertGC = check;
      return this;
    }

    public ContextUtils build() {
      return new ContextUtils(polyglotCtxBldr, stdout, stderr, alwaysExecuteInContext, assertGC);
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
      try {
        if (alwaysExecuteInContext) {
          executeInContext(
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
        var avoidAssertGC = Boolean.FALSE.equals(assertGC);
        close(!avoidAssertGC);
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

  @ExportLibrary(InteropLibrary.class)
  static final class Unwrapper implements TruffleObject {

    Object[] args;

    @ExportMessage
    Object execute(Object[] args) {
      this.args = args;
      return this;
    }

    @ExportMessage
    boolean isExecutable() {
      return true;
    }
  }
}
