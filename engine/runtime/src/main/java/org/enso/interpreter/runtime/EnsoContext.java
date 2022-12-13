package org.enso.interpreter.runtime;

import com.oracle.truffle.api.Assumption;
import com.oracle.truffle.api.CompilerDirectives.CompilationFinal;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLanguage.Env;
import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.object.Shape;
import org.enso.compiler.Compiler;
import org.enso.compiler.PackageRepository;
import org.enso.compiler.data.CompilerConfig;
import org.enso.distribution.DistributionManager;
import org.enso.distribution.locking.LockManager;
import org.enso.editions.LibraryName;
import org.enso.interpreter.EnsoLanguage;
import org.enso.interpreter.OptionsHelper;
import org.enso.interpreter.instrument.NotificationHandler;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.scope.TopLevelScope;
import org.enso.interpreter.runtime.state.IOPermissions;
import org.enso.interpreter.runtime.state.State;
import org.enso.interpreter.runtime.util.TruffleFileSystem;
import org.enso.interpreter.util.ScalaConversions;
import org.enso.librarymanager.ProjectLoadingFailure;
import org.enso.pkg.Package;
import org.enso.pkg.PackageManager;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.RuntimeOptions;
import scala.jdk.javaapi.OptionConverters;

import java.io.*;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicLong;

/**
 * The language context is the internal state of the language that is associated with each thread in
 * a running Enso program.
 */
public class EnsoContext {

  private static final TruffleLanguage.ContextReference<EnsoContext> REFERENCE =
      TruffleLanguage.ContextReference.create(EnsoLanguage.class);

  private final EnsoLanguage language;
  private final Env environment;
  private @CompilationFinal Compiler compiler;
  private final PrintStream out;
  private final PrintStream err;
  private final InputStream in;
  private final BufferedReader inReader;
  private @CompilationFinal PackageRepository packageRepository;
  private @CompilationFinal TopLevelScope topScope;
  private final ThreadManager threadManager;
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

  private final Assumption chromeInspectorNotAttached =
      Truffle.getRuntime().createAssumption("chromeInspectorNotAttached");

  private final Shape rootStateShape = Shape.newBuilder().layout(State.Container.class).build();
  private final IOPermissions rootIOPermissions;

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
    this.resourceManager = new ResourceManager(this);
    this.isInlineCachingDisabled =
        environment.getOptions().get(RuntimeOptions.DISABLE_INLINE_CACHES_KEY);
    var isParallelismEnabled =
        environment.getOptions().get(RuntimeOptions.ENABLE_AUTO_PARALLELISM_KEY);
    this.isIrCachingDisabled =
        environment.getOptions().get(RuntimeOptions.DISABLE_IR_CACHES_KEY) || isParallelismEnabled;
    this.rootIOPermissions = environment.getOptions().get(EnsoLanguage.IO_ENVIRONMENT);

    this.shouldWaitForPendingSerializationJobs =
        environment.getOptions().get(RuntimeOptions.WAIT_FOR_PENDING_SERIALIZATION_JOBS_KEY);
    this.compilerConfig =
        new CompilerConfig(
            isParallelismEnabled,
            true,
            environment.getOptions().get(RuntimeOptions.STRICT_ERRORS_KEY),
            scala.Option.empty());
    this.home = home;
    this.builtins = new Builtins(this);
    this.notificationHandler = notificationHandler;
    this.lockManager = lockManager;
    this.distributionManager = distributionManager;
  }

  /** Perform expensive initialization logic for the context. */
  public void initialize() {
    TruffleFileSystem fs = new TruffleFileSystem();
    PackageManager<TruffleFile> packageManager = new PackageManager<>(fs);

    Optional<TruffleFile> projectRoot = OptionsHelper.getProjectRoot(environment);
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

    Optional<String> languageHome =
        OptionsHelper.getLanguageHomeOverride(environment).or(() -> Optional.ofNullable(home));
    var editionOverride = OptionsHelper.getEditionOverride(environment);
    var resourceManager = new org.enso.distribution.locking.ResourceManager(lockManager);

    packageRepository =
        PackageRepository.initializeRepository(
            OptionConverters.toScala(projectPackage),
            OptionConverters.toScala(languageHome),
            OptionConverters.toScala(editionOverride),
            distributionManager,
            resourceManager,
            this,
            builtins,
            notificationHandler);
    topScope = new TopLevelScope(builtins, packageRepository);
    this.compiler = new Compiler(this, builtins, packageRepository, compilerConfig);

    projectPackage.ifPresent(
        pkg -> packageRepository.registerMainProjectPackage(pkg.libraryName(), pkg));
  }

  /**
   * @param node the location of context access. Pass {@code null} if not in a node.
   * @return the proper context instance for the current {@link
   *     com.oracle.truffle.api.TruffleContext}.
   */
  public static EnsoContext get(Node node) {
    return REFERENCE.get(node);
  }

  public static TruffleLanguage.ContextReference<EnsoContext> getReference() {
    return REFERENCE;
  }

  /** Performs eventual cleanup before the context is disposed of. */
  public void shutdown() {
    threadManager.shutdown();
    resourceManager.shutdown();
    compiler.shutdown(shouldWaitForPendingSerializationJobs);
  }

  /**
   * Creates a truffle file for a given standard file.
   *
   * @param file the file to wrap
   * @return the truffle wrapper for {@code file}
   */
  public TruffleFile getTruffleFile(File file) {
    return getEnvironment().getInternalTruffleFile(file.getAbsolutePath());
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

  /** Returns an {@link Assumption} that Chrome inspector is not attached to this context. */
  public Assumption getChromeInspectorNotAttached() {
    return chromeInspectorNotAttached;
  }

  /**
   * Returns the {@link Env} instance used by this context.
   *
   * @return the {@link Env} instance used by this context
   */
  public Env getEnvironment() {
    return environment;
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

  /** @return the standard input stream of characters. */
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
    return ScalaConversions.asJava(packageRepository.getLoadedPackages()).stream()
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
        .filter(
            module ->
                module.getIr().preorder().exists(ir -> ir.getExternalId().contains(expressionId)))
        .findFirst();
  }

  /**
   * Finds the package the provided module belongs to.
   *
   * @param file the module to find the package of
   * @return {@code module}'s package, if exists
   */
  public Optional<Package<TruffleFile>> getPackageOf(TruffleFile file) {
    if (file == null) {
      return Optional.empty();
    }
    return ScalaConversions.asJava(packageRepository.getLoadedPackages()).stream()
        .filter(pkg -> file.getAbsoluteFile().startsWith(pkg.root().getAbsoluteFile()))
        .findFirst();
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
    return getEnvironment().getOptions().get(RuntimeOptions.STRICT_ERRORS_KEY);
  }

  /**
   * Checks whether the suggestions indexing is enabled for project files.
   *
   * @return true if project-level suggestion indexing is enabled.
   */
  public boolean isProjectSuggestionsEnabled() {
    return getEnvironment().getOptions().get(RuntimeOptions.ENABLE_PROJECT_SUGGESTIONS_KEY);
  }

  /**
   * Checks whether the suggestion indexing is enabled for external libraries.
   *
   * @return true if the suggestions indexing is enabled for external libraries.
   */
  public boolean isGlobalSuggestionsEnabled() {
    return getEnvironment().getOptions().get(RuntimeOptions.ENABLE_GLOBAL_SUGGESTIONS_KEY);
  }

  /** Creates a new thread that has access to the current language context. */
  public Thread createThread(Runnable runnable) {
    return environment.createThread(runnable);
  }

  /** @return the thread manager for this context. */
  public ThreadManager getThreadManager() {
    return threadManager;
  }

  /** @return the resource manager for this context */
  public ResourceManager getResourceManager() {
    return resourceManager;
  }

  /** @return whether inline caches should be disabled for this context. */
  public boolean isInlineCachingDisabled() {
    return isInlineCachingDisabled;
  }

  /** @return whether IR caching should be disabled for this context. */
  public boolean isIrCachingDisabled() {
    return isIrCachingDisabled;
  }

  /** @return the compiler configuration for this language */
  public CompilerConfig getCompilerConfig() {
    return compilerConfig;
  }

  /** @return the distribution manager for this language */
  public DistributionManager getDistributionManager() {
    return distributionManager;
  }

  /** @return The logger for this language */
  public TruffleLogger getLogger() {
    return logger;
  }

  /** @return the package repository */
  public PackageRepository getPackageRepository() {
    return packageRepository;
  }

  /**
   * Gets a logger for the specified class.
   *
   * @param klass the class to name log entries with
   * @return a new logger for the specified {@code path}
   */
  public TruffleLogger getLogger(Class<?> klass) {
    return TruffleLogger.getLogger(LanguageInfo.ID, klass);
  }

  /**
   * Returns the current clock value and atomically increments the counter by one.
   *
   * <p>The counter is used to track the creation time of warnings.
   */
  public long clockTick() {
    return clock.getAndIncrement();
  }

  public IOPermissions getRootIOPermissions() {
    return rootIOPermissions;
  }

  public Shape getRootStateShape() {
    return rootStateShape;
  }

  public State emptyState() {
    return State.create(this);
  }
}
