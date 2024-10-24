package org.enso.interpreter.runtime;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.ThreadLocalAction;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLanguage.Env;
import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.interop.InteropException;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnknownIdentifierException;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.io.TruffleProcessBuilder;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.object.Shape;
import com.oracle.truffle.api.source.Source;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.net.MalformedURLException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.Consumer;
import java.util.logging.Level;
import org.enso.common.LanguageInfo;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.Compiler;
import org.enso.compiler.core.EnsoParser;
import org.enso.compiler.data.CompilerConfig;
import org.enso.compiler.dump.IRDumper;
import org.enso.distribution.DistributionManager;
import org.enso.distribution.locking.LockManager;
import org.enso.editions.LibraryName;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.OptionsHelper;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.data.atom.Atom;
import org.enso.interpreter.runtime.error.DataflowError;
import org.enso.interpreter.runtime.error.PanicException;
import org.enso.interpreter.runtime.instrument.NotificationHandler;
import org.enso.interpreter.runtime.scope.TopLevelScope;
import org.enso.interpreter.runtime.state.ExecutionEnvironment;
import org.enso.interpreter.runtime.state.State;
import org.enso.interpreter.runtime.state.WithContextNode;
import org.enso.interpreter.runtime.util.TruffleFileSystem;
import org.enso.librarymanager.ProjectLoadingFailure;
import org.enso.librarymanager.resolved.LibraryRoot;
import org.enso.logger.masking.MaskedPath$;
import org.enso.pkg.Package;
import org.enso.pkg.PackageManager;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.debugger.IdExecutionService;
import org.graalvm.options.OptionKey;
import scala.jdk.javaapi.OptionConverters;

/**
 * The language context is the internal state of the language that is associated with each thread in
 * a running Enso program.
 */
public final class EnsoContext {

  private static final TruffleLanguage.ContextReference<EnsoContext> REFERENCE =
      TruffleLanguage.ContextReference.create(EnsoLanguage.class);

  private final EnsoLanguage language;
  private final Env environment;
  private final HostClassLoader hostClassLoader = new HostClassLoader();
  private final boolean assertionsEnabled;
  private final boolean isPrivateCheckDisabled;
  private final boolean isStaticTypeAnalysisEnabled;
  private @CompilationFinal Compiler compiler;
  private final PrintStream out;
  private final PrintStream err;
  private final InputStream in;
  private final BufferedReader inReader;
  private @CompilationFinal DefaultPackageRepository packageRepository;
  private @CompilationFinal TopLevelScope topScope;
  private final ThreadManager threadManager;
  private final ThreadExecutors threadExecutors;
  private final ResourceManager resourceManager;
  private final boolean isInlineCachingDisabled;
  private final boolean isIrCachingDisabled;
  private final boolean shouldWaitForPendingSerializationJobs;
  private final Builtins builtins;
  private final String home;
  private final CompilerConfig compilerConfig;
  private final NotificationHandler notificationHandler;
  private final TruffleLogger logger = TruffleLogger.getLogger(LanguageInfo.ID, EnsoContext.class);
  private final DistributionManager distributionManager;
  private final LockManager lockManager;
  private final AtomicLong clock = new AtomicLong();

  private final Shape rootStateShape = Shape.newBuilder().layout(State.Container.class).build();
  private ExecutionEnvironment globalExecutionEnvironment;

  private final int warningsLimit;

  /**
   * Creates a new Enso context.
   *
   * @param language the language identifier
   * @param home language home
   * @param environment the execution environment of the {@link TruffleLanguage}
   * @param notificationHandler a handler for notifications
   * @param lockManager the lock manager instance
   * @param distributionManager a distribution manager
   */
  public EnsoContext(
      EnsoLanguage language,
      String home,
      Env environment,
      NotificationHandler notificationHandler,
      LockManager lockManager,
      DistributionManager distributionManager) {
    this.language = language;
    this.environment = environment;
    this.out = new PrintStream(environment.out());
    this.err = new PrintStream(environment.err());
    this.in = environment.in();
    this.inReader = new BufferedReader(new InputStreamReader(environment.in()));
    this.threadManager = new ThreadManager(environment);
    this.threadExecutors = new ThreadExecutors(this);
    this.resourceManager = new ResourceManager(this);
    this.isInlineCachingDisabled = getOption(RuntimeOptions.DISABLE_INLINE_CACHES_KEY);
    var isParallelismEnabled = getOption(RuntimeOptions.ENABLE_AUTO_PARALLELISM_KEY);
    this.isIrCachingDisabled =
        getOption(RuntimeOptions.DISABLE_IR_CACHES_KEY) || isParallelismEnabled;
    this.isPrivateCheckDisabled = getOption(RuntimeOptions.DISABLE_PRIVATE_CHECK_KEY);
    this.isStaticTypeAnalysisEnabled = getOption(RuntimeOptions.ENABLE_STATIC_ANALYSIS_KEY);
    this.globalExecutionEnvironment = getOption(EnsoLanguage.EXECUTION_ENVIRONMENT);
    this.assertionsEnabled = shouldAssertionsBeEnabled();
    this.shouldWaitForPendingSerializationJobs =
        getOption(RuntimeOptions.WAIT_FOR_PENDING_SERIALIZATION_JOBS_KEY);
    var dumpIrs = Boolean.parseBoolean(System.getProperty(IRDumper.SYSTEM_PROP));
    this.compilerConfig =
        new CompilerConfig(
            isParallelismEnabled,
            true,
            !isPrivateCheckDisabled,
            isStaticTypeAnalysisEnabled,
            dumpIrs,
            getOption(RuntimeOptions.STRICT_ERRORS_KEY),
            getOption(RuntimeOptions.DISABLE_LINTING_KEY),
            scala.Option.empty());
    this.home = home;
    this.builtins = new Builtins(this);
    this.notificationHandler = notificationHandler;
    this.lockManager = lockManager;
    this.distributionManager = distributionManager;
    this.warningsLimit = getOption(RuntimeOptions.WARNINGS_LIMIT_KEY);
  }

  /** Perform expensive initialization logic for the context. */
  public void initialize() {
    TruffleFileSystem fs = new TruffleFileSystem();
    PackageManager<TruffleFile> packageManager = new PackageManager<>(fs);

    Optional<TruffleFile> projectRoot = OptionsHelper.getProjectRoot(environment);
    checkWorkingDirectory(projectRoot);
    Optional<Package<TruffleFile>> projectPackage =
        projectRoot.map(
            file ->
                packageManager
                    .loadPackage(file)
                    .fold(
                        err -> {
                          throw new ProjectLoadingFailure(file.getName(), err);
                        },
                        res -> res));

    var languageHome = OptionsHelper.findLanguageHome(environment);
    var editionOverride = OptionsHelper.getEditionOverride(environment);
    var resourceManager = new org.enso.distribution.locking.ResourceManager(lockManager);

    packageRepository =
        DefaultPackageRepository.initializeRepository(
            OptionConverters.toScala(projectPackage),
            OptionConverters.toScala(languageHome),
            OptionConverters.toScala(editionOverride),
            distributionManager,
            resourceManager,
            this,
            builtins,
            notificationHandler);
    topScope = new TopLevelScope(builtins, packageRepository);
    this.compiler =
        new Compiler(new TruffleCompilerContext(this), packageRepository, compilerConfig);

    projectPackage.ifPresent(
        pkg -> packageRepository.registerMainProjectPackage(pkg.libraryName(), pkg));

    var preinit = environment.getOptions().get(RuntimeOptions.PREINITIALIZE_KEY);
    if (preinit != null && preinit.length() > 0) {
      var epb = environment.getInternalLanguages().get("epb");
      @SuppressWarnings("unchecked")
      var run = (Consumer<String>) environment.lookup(epb, Consumer.class);
      if (run != null) {
        run.accept(preinit);
      }
    }
  }

  /** Checks if the working directory is as expected and reports a warning if not. */
  private void checkWorkingDirectory(Optional<TruffleFile> maybeProjectRoot) {
    if (maybeProjectRoot.isPresent()) {
      var root = maybeProjectRoot.get();
      var parent = root.getAbsoluteFile().normalize().getParent();
      var cwd = environment.getCurrentWorkingDirectory().getAbsoluteFile().normalize();
      try {
        if (!cwd.isSameFile(parent)) {
          var maskedPath = MaskedPath$.MODULE$.apply(Path.of(parent.toString()));
          logger.log(
              Level.WARNING,
              "Initializing the context in a different working directory than the one containing"
                  + " the project root. This may lead to relative paths not behaving as advertised"
                  + " by `File.new`. Please run the engine inside of `{}` directory.",
              maskedPath);
        }
      } catch (IOException e) {
        logger.severe("Error checking working directory: " + e.getMessage());
      }
    }
  }

  /**
   * @param node the location of context access. Pass {@code null} if not in a node.
   * @return the proper context instance for the current {@link
   *     com.oracle.truffle.api.TruffleContext}.
   */
  public static EnsoContext get(Node node) {
    var ctx = REFERENCE.get(node);
    if (checkNodes.isValid() && !CompilerDirectives.isPartialEvaluationConstant(ctx)) {
      reportSlowContextAccess(node);
    }
    return ctx;
  }

  private static final Assumption checkNodes =
      Truffle.getRuntime().createAssumption("context check");
  private static final Set<Node> reportedNullRootNodes = new HashSet<>();
  private static long checkUntil = Long.MAX_VALUE;

  @TruffleBoundary
  private static void reportSlowContextAccess(Node n) {
    if (System.currentTimeMillis() > checkUntil) {
      checkNodes.invalidate();
    }
    if (reportedNullRootNodes.add(n)) {
      var ex =
          new AssertionError(
              """
        no root node for {n}
        with section: {s}
        with root nodes: {r}
        """
                  .replace("{n}", "" + n)
                  .replace("{s}", "" + n.getEncapsulatingSourceSection())
                  .replace("{r}", "" + n.getRootNode()));
      ex.printStackTrace();
      checkUntil = System.currentTimeMillis() + 10000;
      var assertsOn = false;
      assert assertsOn = true;
      if (assertsOn) {
        throw ex;
      }
    }
  }

  public static TruffleLanguage.ContextReference<EnsoContext> getReference() {
    return REFERENCE;
  }

  /** Performs eventual cleanup before the context is disposed of. */
  public void shutdown() {
    threadExecutors.shutdown();
    threadManager.shutdown();
    resourceManager.shutdown();
    compiler.shutdown(shouldWaitForPendingSerializationJobs);
    packageRepository.shutdown();
    guestJava = null;
    topScope = null;
    hostClassLoader.close();
    EnsoParser.freeAll();
  }

  private boolean shouldAssertionsBeEnabled() {
    var envVar = environment.getEnvironment().get("ENSO_ENABLE_ASSERTIONS");
    if (envVar != null) {
      return Boolean.parseBoolean(envVar);
    }
    return isJvmAssertionsEnabled();
  }

  private static boolean isJvmAssertionsEnabled() {
    boolean assertionsEnabled = false;
    assert assertionsEnabled = true;
    return assertionsEnabled;
  }

  /**
   * Creates a truffle file for a given standard file.
   *
   * @param file the file to wrap
   * @return the truffle wrapper for {@code file}
   */
  public TruffleFile getTruffleFile(File file) {
    return environment.getInternalTruffleFile(file.getAbsolutePath());
  }

  /**
   * Gets the compiler instance.
   *
   * <p>The compiler is the portion of the interpreter that performs static analysis and
   * transformation passes on the input program. A handle to the compiler lets you execute various
   * portions of the compilation pipeline, including parsing, analysis, and final code generation.
   *
   * <p>Having this access available means that Enso programs can metaprogram Enso itself.
   *
   * @return a handle to the compiler
   */
  public final Compiler getCompiler() {
    return compiler;
  }

  /**
   * Gets the language to which this context belongs.
   *
   * @return the language to which this context belongs
   */
  public EnsoLanguage getLanguage() {
    return language;
  }

  /**
   * Returns the standard output stream for this context.
   *
   * @return the standard output stream for this context.
   */
  public PrintStream getOut() {
    return out;
  }

  /**
   * Returns the standard error stream for this context.
   *
   * @return the standard error stream for this context
   */
  public PrintStream getErr() {
    return err;
  }

  /**
   * Returns the standard input stream for this context.
   *
   * @return the standard input stream of bytes.
   */
  public InputStream getIn() {
    return in;
  }

  /**
   * @return the standard input stream of characters.
   */
  public BufferedReader getInReader() {
    return inReader;
  }

  /**
   * Fetches the module name associated with a given file, using the environment packages
   * information.
   *
   * @param path the path to decode.
   * @return a qualified name of the module corresponding to the file, if exists.
   */
  public Optional<QualifiedName> getModuleNameForFile(File path) {
    TruffleFile p = getTruffleFile(path);
    return getModuleNameForFile(p);
  }

  /**
   * Fetches the module name associated with a given file, using the environment packages
   * information.
   *
   * @param file the path to decode.
   * @return a qualified name of the module corresponding to the file, if exists.
   */
  public Optional<QualifiedName> getModuleNameForFile(TruffleFile file) {
    return scala.jdk.javaapi.CollectionConverters.asJava(packageRepository.getLoadedPackages())
        .stream()
        .filter(pkg -> file.startsWith(pkg.sourceDir()))
        .map(pkg -> pkg.moduleNameForFile(file))
        .findFirst();
  }

  /**
   * Renames project in packages and modules.
   *
   * @param namespace the namespace the renamed project belongs to
   * @param oldName the old project name
   * @param newName the new project name
   */
  public void renameProject(String namespace, String oldName, String newName) {
    packageRepository.renameProject(namespace, oldName, newName);
  }

  /**
   * Fetches a module associated with a given file.
   *
   * @param path the module path to lookup.
   * @return the relevant module, if exists.
   */
  public Optional<Module> getModuleForFile(File path) {
    return getModuleNameForFile(path).flatMap(n -> getTopScope().getModule(n.toString()));
  }

  /**
   * Ensures that a module is preloaded if it can be loaded at all.
   *
   * @param moduleName name of the module to preload
   */
  public void ensureModuleIsLoaded(String moduleName) {
    LibraryName.fromModuleName(moduleName).foreach(packageRepository::ensurePackageIsLoaded);
  }

  /**
   * Fetches a module with a given name.
   *
   * @param moduleName the qualified name of the module to lookup.
   * @return the relevant module, if exists.
   */
  public Optional<Module> findModule(String moduleName) {
    return getTopScope().getModule(moduleName);
  }

  /**
   * Find a module containing the given expression id.
   *
   * @param expressionId the expression id to lookup.
   * @return the relevant module, if exists.
   */
  public Optional<Module> findModuleByExpressionId(UUID expressionId) {
    return getTopScope().getModules().stream()
        .filter(m -> m.containsUUID(expressionId))
        .findFirst();
  }

  /**
   * Modifies the classpath to use to lookup {@code polyglot java} imports.
   *
   * @param file the file to register
   */
  @TruffleBoundary
  public void addToClassPath(TruffleFile file) {
    if (findGuestJava() == null) {
      try {
        var url = file.toUri().toURL();
        hostClassLoader.add(url);
      } catch (MalformedURLException ex) {
        throw new IllegalStateException(ex);
      }
    } else {
      try {
        var path = new File(file.toUri()).getAbsoluteFile();
        if (!path.exists()) {
          throw new IllegalStateException("File not found " + path);
        }
        InteropLibrary.getUncached().invokeMember(findGuestJava(), "addPath", path.getPath());
      } catch (InteropException ex) {
        throw new IllegalStateException(ex);
      }
    }
  }

  /**
   * Checks whether provided object comes from Java. Either Java system libraries or libraries added
   * by {@link #addToClassPath(TruffleFile)}.
   *
   * @param obj the object to check
   * @return {@code true} or {@code false}
   */
  public boolean isJavaPolyglotObject(Object obj) {
    return environment.isHostObject(obj);
  }

  /**
   * Checks whether provided object comes from Java and represents a function.
   *
   * @param obj the object to check
   * @return {@code true} or {@code false}
   */
  public boolean isJavaPolyglotFunction(Object obj) {
    return environment.isHostFunction(obj);
  }

  /**
   * Converts an interop object into underlying Java representation.
   *
   * @param obj object that {@link #isJavaPolyglotObject}
   * @return underlying object
   */
  public Object asJavaPolyglotObject(Object obj) {
    return environment.asHostObject(obj);
  }

  /**
   * Wraps a Java object into interop object.
   *
   * @param obj java object
   * @return wrapper object
   */
  //  @Deprecated(forRemoval=true)
  public Object asGuestValue(Object obj) {
    return environment.asGuestValue(obj);
  }

  /**
   * Returns true if the output is a terminal that supports ANSI colors. {@see
   * https://github.com/termstandard/colors/} {@see https://no-color.org/}
   */
  public boolean isColorTerminalOutput() {
    var envVars = environment.getEnvironment();
    if (envVars.get("NO_COLOR") != null) {
      return false;
    }
    if (envVars.get("COLORTERM") != null) {
      return true;
    }
    if (envVars.get("TERM") != null) {
      var termEnv = envVars.get("TERM").toLowerCase();
      return Arrays.stream(termEnv.split("-"))
          .anyMatch(str -> str.equals("color") || str.equals("256color"));
    }
    return false;
  }

  /**
   * Tries to lookup a Java class (host symbol in Truffle terminology) by its fully qualified name.
   * This method also tries to lookup inner classes. More specifically, if the provided name
   * resolves to an inner class, then the import of the outer class is resolved, and the inner class
   * is looked up by iterating the members of the outer class via Truffle's interop protocol.
   *
   * @param className Fully qualified class name, can also be nested static inner class.
   * @return If the java class is found, return it, otherwise return {@link DataflowError}.
   */
  @TruffleBoundary
  public TruffleObject lookupJavaClass(String className) {
    var binaryName = new StringBuilder(className);
    var collectedExceptions = new ArrayList<Exception>();
    for (; ; ) {
      var fqn = binaryName.toString();
      try {
        var hostSymbol = lookupHostSymbol(fqn);
        if (hostSymbol != null) {
          return (TruffleObject) hostSymbol;
        }
      } catch (ClassNotFoundException | RuntimeException | InteropException ex) {
        collectedExceptions.add(ex);
      }
      var at = fqn.lastIndexOf('.');
      if (at < 0) {
        break;
      }
      binaryName.setCharAt(at, '$');
    }
    var level = Level.WARNING;
    for (var ex : collectedExceptions) {
      logger.log(level, ex.getMessage());
      level = Level.FINE;
      logger.log(Level.FINE, null, ex);
    }
    return getBuiltins().error().makeMissingPolyglotImportError(className);
  }

  private Object lookupHostSymbol(String fqn)
      throws ClassNotFoundException, UnknownIdentifierException, UnsupportedMessageException {
    try {
      if (findGuestJava() == null) {
        return environment.asHostSymbol(hostClassLoader.loadClass(fqn));
      } else {
        return InteropLibrary.getUncached().readMember(findGuestJava(), fqn);
      }
    } catch (Error e) {
      throw new ClassNotFoundException("Error loading " + fqn, e);
    }
  }

  private Object guestJava = this;

  @TruffleBoundary
  private Object findGuestJava() throws IllegalStateException {
    if (guestJava != this) {
      return guestJava;
    }
    guestJava = null;
    var envJava = System.getenv("ENSO_JAVA");
    if (envJava == null) {
      return guestJava;
    }
    if ("espresso".equals(envJava)) {
      var src = Source.newBuilder("java", "<Bindings>", "getbindings.java").build();
      try {
        guestJava = environment.parsePublic(src).call();
        logger.log(Level.SEVERE, "Using experimental Espresso support!");
      } catch (Exception ex) {
        if (ex.getMessage().contains("No language for id java found.")) {
          logger.log(
              Level.SEVERE,
              "Environment variable ENSO_JAVA=" + envJava + ", but " + ex.getMessage());
          logger.log(Level.SEVERE, "Copy missing libraries to components directory");
          logger.log(Level.SEVERE, "Continuing in regular Java mode");
        } else {
          var ise = new IllegalStateException(ex.getMessage());
          ise.setStackTrace(ex.getStackTrace());
          throw ise;
        }
      }
    } else {
      throw new IllegalStateException(
          "Specify ENSO_JAVA=espresso to use Espresso. Was: " + envJava);
    }
    return guestJava;
  }

  /**
   * Finds the package the provided module belongs to.
   *
   * @param file the module to find the package of
   * @return {@code module}'s package, if exists
   */
  public Optional<Package<TruffleFile>> getPackageOf(TruffleFile file) {
    return TruffleCompilerContext.getPackageOf(packageRepository, file);
  }

  /**
   * Registers a new module corresponding to a given file.
   *
   * @param path the file to register.
   * @return the newly created module, if the file is a source file.
   */
  public Optional<Module> createModuleForFile(File path) {
    TruffleFile f = getTruffleFile(path);
    return getModuleNameForFile(path)
        .map(name -> getTopScope().createModule(name, getPackageOf(f).orElse(null), f));
  }

  /**
   * Gets the builtin functions from the compiler.
   *
   * @return an object containing the builtin functions
   */
  public Builtins getBuiltins() {
    return this.builtins;
  }

  /**
   * Gets the top-level language scope.
   *
   * @return an object containing the top level language scope
   */
  public TopLevelScope getTopScope() {
    return this.topScope;
  }

  /**
   * Returns the atom constructor corresponding to the {@code Nothing} type, for builtin constructs
   * that need to return an atom of this type.
   *
   * @return the builtin {@code Nothing} atom constructor
   */
  public Type getNothing() {
    return getBuiltins().nothing();
  }

  /**
   * Checks whether the strict errors option was set for this context.
   *
   * @return true if the strict errors option is enabled, false otherwise.
   */
  public boolean isStrictErrors() {
    return getOption(RuntimeOptions.STRICT_ERRORS_KEY);
  }

  /**
   * Checks whether the suggestions indexing is enabled for project files.
   *
   * @return true if project-level suggestion indexing is enabled.
   */
  public boolean isProjectSuggestionsEnabled() {
    return getOption(RuntimeOptions.ENABLE_PROJECT_SUGGESTIONS_KEY);
  }

  /**
   * Checks whether global caches are to be used.
   *
   * @return true if so
   */
  public boolean isUseGlobalCache() {
    return getOption(RuntimeOptions.USE_GLOBAL_IR_CACHE_LOCATION_KEY);
  }

  public boolean isAssertionsEnabled() {
    return assertionsEnabled;
  }

  /**
   * Checks whether we are running in interactive mode.
   *
   * @return true if so
   */
  public boolean isInteractiveMode() {
    return getOption(RuntimeOptions.INTERACTIVE_MODE_KEY);
  }

  /**
   * Checks value of {@link RuntimeOptions#INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION_KEY}.
   *
   * @return the value of the option
   */
  public boolean isInterpreterSequentialCommandExection() {
    return getOption(RuntimeOptions.INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION_KEY);
  }

  /**
   * Checks value of {@link RuntimeOptions#INTERPRETER_RANDOM_DELAYED_COMMAND_EXECUTION_KEY}.
   *
   * @return the value of the option
   */
  public boolean isRandomDelayedCommandExecution() {
    return getOption(RuntimeOptions.INTERPRETER_RANDOM_DELAYED_COMMAND_EXECUTION_KEY);
  }

  /**
   * Checks whether the suggestion indexing is enabled for external libraries.
   *
   * @return true if the suggestions indexing is enabled for external libraries.
   */
  public boolean isGlobalSuggestionsEnabled() {
    return getOption(RuntimeOptions.ENABLE_GLOBAL_SUGGESTIONS_KEY);
  }

  /** The job parallelism or 1 */
  public int getJobParallelism() {
    var n = getOption(RuntimeOptions.JOB_PARALLELISM_KEY);
    var base = n == null ? 1 : n.intValue();
    var optimal = Math.round(base * 0.5);
    return optimal < 1 ? 1 : (int) optimal;
  }

  /**
   * @param name human-readable name of the pool
   * @param min minimal number of threads kept-alive in the pool
   * @param max maximal number of available threads
   * @param maxQueueSize maximal number of pending tasks
   * @param systemThreads use system threads or polyglot threads
   * @return new execution service for this context
   */
  public ExecutorService newCachedThreadPool(
      String name, int min, int max, int maxQueueSize, boolean systemThreads) {
    return threadExecutors.newCachedThreadPool(name, systemThreads, min, max, maxQueueSize);
  }

  /**
   * @param parallel amount of parallelism for the pool
   * @param name human-readable name of the pool
   * @param systemThreads use system threads or polyglot threads
   * @return new execution service for this context
   */
  public ExecutorService newFixedThreadPool(int parallel, String name, boolean systemThreads) {
    return threadExecutors.newFixedThreadPool(parallel, name, systemThreads);
  }

  /**
   * @return the thread manager for this context.
   */
  public ThreadManager getThreadManager() {
    return threadManager;
  }

  /**
   * @return the resource manager for this context
   */
  public ResourceManager getResourceManager() {
    return resourceManager;
  }

  /**
   * @return whether inline caches should be disabled for this context.
   */
  public boolean isInlineCachingDisabled() {
    return isInlineCachingDisabled;
  }

  /**
   * @return when {@code private} keyword should be checked.
   */
  public boolean isPrivateCheckDisabled() {
    return isPrivateCheckDisabled;
  }

  /**
   * @return whether IR caching should be disabled for this context.
   */
  public boolean isIrCachingDisabled() {
    return isIrCachingDisabled;
  }

  /**
   * @return the compiler configuration for this language
   */
  public CompilerConfig getCompilerConfig() {
    return compilerConfig;
  }

  /**
   * @return the distribution manager for this language
   */
  public DistributionManager getDistributionManager() {
    return distributionManager;
  }

  /**
   * @return The logger for this language
   */
  public TruffleLogger getLogger() {
    return logger;
  }

  /**
   * @return the package repository
   */
  public DefaultPackageRepository getPackageRepository() {
    return packageRepository;
  }

  /**
   * Gets a logger for the specified class that is bound to this engine. Such logger may then be
   * safely used in threads defined in a thread-pool.
   *
   * @param clazz the class to name log entries with
   * @return a new logger for the specified {@code path}
   */
  public TruffleLogger getLogger(Class<?> clazz) {
    return environment.getLogger(clazz);
  }

  /**
   * Returns the current clock value and atomically increments the counter by one.
   *
   * <p>The counter is used to track the creation time of warnings.
   */
  public long nextSequenceId() {
    return clock.getAndIncrement();
  }

  public ExecutionEnvironment getGlobalExecutionEnvironment() {
    return globalExecutionEnvironment;
  }

  public ExecutionEnvironment getExecutionEnvironment() {
    ExecutionEnvironment env = language.getExecutionEnvironment();
    return env == null ? getGlobalExecutionEnvironment() : env;
  }

  /** Set the runtime execution environment of this context. */
  public void setExecutionEnvironment(ExecutionEnvironment executionEnvironment) {
    this.globalExecutionEnvironment = executionEnvironment;
    language.setExecutionEnvironment(executionEnvironment);
  }

  /**
   * Enable execution context in the execution environment.
   *
   * @param context the execution context
   * @param environmentName the execution environment name
   * @return the execution environment version before modification
   */
  public ExecutionEnvironment enableExecutionEnvironment(Atom context, String environmentName) {
    ExecutionEnvironment original = globalExecutionEnvironment;
    if (original.getName().equals(environmentName)) {
      var newExecEnv =
          WithContextNode.getUncached().executeEnvironmentUpdate(original, context, true);
      setExecutionEnvironment(newExecEnv);
    }
    return original;
  }

  /**
   * Enable execution context in the execution environment.
   *
   * @param context the execution context
   * @param environmentName the execution environment name
   * @return the execution environment version before modification
   */
  public ExecutionEnvironment disableExecutionEnvironment(Atom context, String environmentName) {
    ExecutionEnvironment original = globalExecutionEnvironment;
    if (original.getName().equals(environmentName)) {
      var newExecEnv =
          WithContextNode.getUncached().executeEnvironmentUpdate(original, context, false);
      setExecutionEnvironment(newExecEnv);
    }
    return original;
  }

  /** Returns a maximal number of warnings that can be attached to a value */
  public int getWarningsLimit() {
    return this.warningsLimit;
  }

  public Shape getRootStateShape() {
    return rootStateShape;
  }

  public State emptyState() {
    return State.create(this);
  }

  /**
   * @return the notification handler.
   */
  public NotificationHandler getNotificationHandler() {
    return notificationHandler;
  }

  public TruffleFile findLibraryRootPath(LibraryRoot root) {
    return environment.getInternalTruffleFile(
        root.location().toAbsolutePath().normalize().toString());
  }

  public TruffleFile getPublicTruffleFile(String path) {
    return environment.getPublicTruffleFile(path);
  }

  public TruffleFile getCurrentWorkingDirectory() {
    return environment.getCurrentWorkingDirectory();
  }

  public TruffleProcessBuilder newProcessBuilder(String... args) {
    return environment.newProcessBuilder(args);
  }

  public boolean isCreateThreadAllowed() {
    return environment.isCreateThreadAllowed();
  }

  public Thread createThread(boolean systemThread, Runnable run) {
    return systemThread
        ? environment.createSystemThread(run)
        : environment.newTruffleThreadBuilder(run).build();
  }

  public Future<Void> submitThreadLocal(Thread[] threads, ThreadLocalAction action) {
    return environment.submitThreadLocal(threads, action);
  }

  public CallTarget parseInternal(Source src, String... argNames) {
    return environment.parseInternal(src, argNames);
  }

  public boolean isLanguageInstalled(String name) {
    return environment.getPublicLanguages().get(name) != null;
  }

  public IdExecutionService getIdValueExtractor() {
    var instrument = environment.getInstruments().get("id-value-extractor");
    if (instrument != null) {
      return environment.lookup(instrument, IdExecutionService.class);
    } else {
      return null;
    }
  }

  /**
   * Helper method to use when an unexpected state happens that should raise a panic, but not crash
   * the interpreter. Creates a {@link PanicException} with <em>assertion error</em> payload.
   *
   * @param node where the problem happened (may be {@code null})
   * @param message {@code null} (then {@code e.getMessage()} is used) or a special message to use
   *     in the panic
   * @param e external exception to extract message and stack from or {@code null}
   * @return this method never returns it throws the {@link PanicException}
   * @throws PanicException with <em>assertion error</em> payload
   */
  @CompilerDirectives.TruffleBoundary
  public PanicException raiseAssertionPanic(Node node, String message, Throwable e)
      throws PanicException {
    String msg;
    String sep;
    if (e != null) {
      msg = e.getClass().getName();
      if (message == null) {
        message = e.getMessage();
      }
      sep = ": ";
    } else {
      msg = "";
      sep = "";
    }
    if (message != null) {
      msg = msg + sep + message;
    }
    var err = getBuiltins().error().makeAssertionError(msg);
    throw new PanicException(err, e, node);
  }

  private <T> T getOption(OptionKey<T> key) {
    var options = environment.getOptions();
    var safely = false;
    assert safely = true;
    if (safely) {
      for (var d : options.getDescriptors()) {
        if (d.getKey() == key) {
          return options.get(key);
        }
      }
      return null;
    } else {
      return options.get(key);
    }
  }
}
