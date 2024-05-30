package org.enso.test.utils;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.RootNode;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.logging.Level;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames.Module;
import org.enso.common.MethodNames.TopScope;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.PolyglotContext;
import org.enso.polyglot.RuntimeOptions;
import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Context.Builder;
import org.graalvm.polyglot.Language;
import org.graalvm.polyglot.Source;
import org.graalvm.polyglot.Value;
import org.graalvm.polyglot.io.IOAccess;
import org.graalvm.polyglot.proxy.ProxyExecutable;

/**
 * A collection of classes and methods useful for testing, mostly Truffle-related stuff.
 */
public final class TestUtils {
  private TestUtils() {}

  public static Context createDefaultContext() {
    var context = defaultContextBuilder().build();
    final Map<String, Language> langs = context.getEngine().getLanguages();
    assert langs.get("enso") != null : "Enso found in languages: " + langs;
    return context;
  }

  public static Context createDefaultContext(OutputStream out) {
    var context = defaultContextBuilder().out(out).build();
    final Map<String, Language> langs = context.getEngine().getLanguages();
    assert langs.get("enso") != null : "Enso found in languages: " + langs;
    return context;
  }

  public static Context.Builder defaultContextBuilder(String... languages) {
    return Context.newBuilder(languages)
        .allowExperimentalOptions(true)
        .allowIO(IOAccess.ALL)
        .allowAllAccess(true)
        .option(RuntimeOptions.LOG_LEVEL, Level.WARNING.getName())
        .option(RuntimeOptions.DISABLE_IR_CACHES, "true")
        .logHandler(System.err)
        .option(RuntimeOptions.STRICT_ERRORS, "true")
        .option(
            RuntimeOptions.LANGUAGE_HOME_OVERRIDE,
            Paths.get("../../distribution/component").toFile().getAbsolutePath());
  }

  public static EnsoContext leakContext(Context ctx) {
    return ctx.getBindings(LanguageInfo.ID)
        .invokeMember(TopScope.LEAK_CONTEXT)
        .as(EnsoContext.class);
  }

  /**
   * Executes the given callable in the given context. A necessity for executing artificially
   * created Truffle ASTs.
   *
   * @return Object returned from {@code callable} wrapped in {@link Value}.
   */
  public static Value executeInContext(Context ctx, Callable<Object> callable) {
    // Force initialization of the context
    ctx.eval("enso", "value = 0");
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
   * Unwraps the `receiver` field from the Value. This is a hack to allow us to test execute methods
   * of artificially created ASTs, e.g., single nodes. More specifically, only unwrapped values are
   * eligible to be passed to node's execute methods, we cannot pass {@link Value} directly to the
   * node's execute methods.
   *
   * <p>Does something similar to what {@link
   * com.oracle.truffle.tck.DebuggerTester#getSourceImpl(Source)} does, but uses a different hack
   * than reflective access.
   */
  public static Object unwrapValue(Context ctx, Value value) {
    var unwrapper = new Unwrapper();
    var unwrapperValue = ctx.asValue(unwrapper);
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
  public static Value createValue(Context ctx, String src, String imports) {
    if (src.lines().count() > 1 || imports == null) {
      throw new IllegalArgumentException("src should have one line, imports must not be null");
    }
    var sb = new StringBuilder();
    sb.append(imports);
    sb.append(System.lineSeparator());
    sb.append("my_var = ").append(src);
    sb.append(System.lineSeparator());
    Value tmpModule = ctx.eval("enso", sb.toString());
    return tmpModule.invokeMember(Module.EVAL_EXPRESSION, "my_var");
  }

  public static Value createValue(Context ctx, String src) {
    return createValue(ctx, src, "");
  }

  /**
   * Evaluates the given source as if it was in an unnamed module.
   *
   * @param src The source code of the module
   * @return The value returned from the main method of the unnamed module.
   */
  public static Value evalModule(Context ctx, String src) {
    Value module = ctx.eval(Source.create("enso", src));
    Value assocType = module.invokeMember(Module.GET_ASSOCIATED_TYPE);
    Value mainMethod = module.invokeMember(Module.GET_METHOD, assocType, "main");
    return mainMethod.execute();
  }

  /**
   * Parses the given module and returns a method by the given name from the module.
   *
   * @param moduleSrc Source of the whole module
   * @return Reference to the method.
   */
  public static Value getMethodFromModule(Context ctx, String moduleSrc, String methodName) {
    Value module = ctx.eval(Source.create("enso", moduleSrc));
    return module.invokeMember(Module.EVAL_EXPRESSION, methodName);
  }

  /**
   * Creates temporary project directory structure with a given main source content. No need to
   * clean it up, as it is managed by JUnit TemporaryFolder rule. Note that we need to create a
   * project, otherwise the private stuff won't work.
   *
   * @param projName Name of the project (as defined in package.yaml).
   * @param mainSrc Main.enso source content
   * @param projDir Root directory of the project. Will be populated with the project structure.
   */
  public static void createProject(String projName, String mainSrc, Path projDir)
      throws IOException {
    var modules = Set.of(new SourceModule(QualifiedName.fromString("Main"), mainSrc));
    createProject(projName, modules, projDir);
  }

  /**
   * Creates a temporary project directory structure with all the given modules and their content.
   * Creates also the package descriptor. The created project directory structure is eligible for
   * running via {@code enso --run <projDir>}.
   *
   * @param projName Name of the project
   * @param modules Set of modules. Must contain `Main` module.
   * @param projDir A directory in which the whole project structure will be created.
   *                Must exist and be a directory.
   */
  public static void createProject(
      String projName, Set<SourceModule> modules, Path projDir) throws IOException {
    if (!projDir.toFile().exists() || !projDir.toFile().isDirectory()) {
      throw new IllegalArgumentException("Project directory " + projDir + " must already be created");
    }
    var projYaml =
        """
name: %s
version: 0.0.1
prefer-local-libraries: true
        """.formatted(projName);
    var yamlPath = projDir.resolve("package.yaml");
    Files.writeString(yamlPath, projYaml);
    assert yamlPath.toFile().exists();
    var srcDir = Files.createDirectory(projDir.resolve("src"));
    assert srcDir.toFile().exists();
    boolean mainModuleFound = false;
    for (var module : modules) {
      var relativePath = String.join(File.pathSeparator, module.name().pathAsJava());
      var modDirPath = srcDir.resolve(relativePath);
      Files.createDirectories(modDirPath);
      var modPath = modDirPath.resolve(module.name().item() + ".enso");
      Files.writeString(modPath, module.code());
      if (module.name().equals(QualifiedName.fromString("Main"))) {
        mainModuleFound = true;
      }
    }
    assert mainModuleFound;
  }

  /**
   * Tests running the project located in the given {@code projDir}. Is equal to running {@code enso
   * --run <projDir>}.
   *
   * @param ctxBuilder A context builder that might be initialized with some specific options.
   * @param projDir Root directory of the project.
   * @param resultConsumer Any action that is to be evaluated on the result of running the {@code
   *     main} method
   */
  public static void testProjectRun(
      Context.Builder ctxBuilder, Path projDir, Consumer<Value> resultConsumer) {
    assert projDir.toFile().exists() && projDir.toFile().isDirectory();
    try (var ctx =
        ctxBuilder
            .option(RuntimeOptions.PROJECT_ROOT, projDir.toAbsolutePath().toString())
            .option(RuntimeOptions.STRICT_ERRORS, "true")
            .option(RuntimeOptions.DISABLE_IR_CACHES, "true")
            .build()) {
      var polyCtx = new PolyglotContext(ctx);
      var mainSrcPath = projDir.resolve("src").resolve("Main.enso");
      var mainMod = polyCtx.evalModule(mainSrcPath.toFile());
      var assocMainModType = mainMod.getAssociatedType();
      var mainMethod = mainMod.getMethod(assocMainModType, "main").get();
      var res = mainMethod.execute();
      resultConsumer.accept(res);
    }
  }

  /**
   * Just a wrapper for {@link TestUtils#testProjectRun(Builder, Path, Consumer)}.
   *
   * @param projDir Root directory of the project.
   * @param resultConsumer Any action that is to be evaluated on the result of running the {@code
   *     main} method
   */
  public static void testProjectRun(Path projDir, Consumer<Value> resultConsumer) {
    testProjectRun(defaultContextBuilder(), projDir, resultConsumer);
  }

  /**
   * An artificial RootNode. Used for tests of nodes that need to be adopted. Just create this root
   * node inside a context, all the other nodes, and insert them via {@link
   * #insertChildren(Node...)}.
   */
  public static final class TestRootNode extends RootNode {
    private final Function<VirtualFrame, Object> callback;

    public TestRootNode() {
      this(null);
    }

    public TestRootNode(Function<VirtualFrame, Object> callback) {
      super(EnsoLanguage.get(null));
      this.callback = callback;
    }

    public void insertChildren(Node... children) {
      for (Node child : children) {
        insert(child);
      }
    }

    /** In the tests, do not execute this root node, but execute directly the child nodes. */
    @Override
    public Object execute(VirtualFrame frame) {
      if (callback == null) {
        throw new AssertionError("should not reach here");
      } else {
        return callback.apply(frame);
      }
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
