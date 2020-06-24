package org.enso.interpreter.runtime;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLanguage.Env;
import java.io.BufferedReader;
import java.io.File;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.*;
import java.util.stream.Collectors;
import org.enso.compiler.Compiler;
import org.enso.home.HomeManager;
import org.enso.interpreter.Language;
import org.enso.interpreter.OptionsHelper;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.scope.TopLevelScope;
import org.enso.interpreter.runtime.util.TruffleFileSystem;
import org.enso.interpreter.util.ScalaConversions;
import org.enso.pkg.Package;
import org.enso.pkg.PackageManager;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.RuntimeOptions;

/**
 * The language context is the internal state of the language that is associated with each thread in
 * a running Enso program.
 */
public class Context {

  private final Language language;
  private final Env environment;
  private final Compiler compiler;
  private final PrintStream out;
  private final PrintStream err;
  private final BufferedReader in;
  private final List<Package<TruffleFile>> packages;
  private final TopLevelScope topScope;
  private final ThreadManager threadManager;
  private final boolean isCachingDisabled;

  /**
   * Creates a new Enso context.
   *
   * @param language the language identifier
   * @param environment the execution environment of the {@link TruffleLanguage}
   */
  public Context(Language language, String home, Env environment) {
    this.language = language;
    this.environment = environment;
    this.out = new PrintStream(environment.out());
    this.err = new PrintStream(environment.err());
    this.in = new BufferedReader(new InputStreamReader(environment.in()));
    this.threadManager = new ThreadManager();
    this.isCachingDisabled = environment.getOptions().get(RuntimeOptions.DISABLE_INLINE_CACHES_KEY);
    TruffleFileSystem fs = new TruffleFileSystem();

    packages = new ArrayList<>();

    if (home != null) {
      HomeManager<TruffleFile> homeManager =
          new HomeManager<>(environment.getInternalTruffleFile(home), fs);
      packages.addAll(homeManager.loadStdLib().collect(Collectors.toList()));
    }

    PackageManager<TruffleFile> packageManager = new PackageManager<>(fs);

    List<TruffleFile> packagePaths = OptionsHelper.getPackagesPaths(environment);
    packages.addAll(
        packagePaths.stream()
            .map(packageManager::fromDirectory)
            .map(ScalaConversions::asJava)
            .filter(Optional::isPresent)
            .map(Optional::get)
            .collect(Collectors.toList()));

    packages.forEach(
        pkg -> {
          List<TruffleFile> classPathItems =
              ScalaConversions.asJava(pkg.listPolyglotExtensions("java"));
          classPathItems.forEach(environment::addToHostClassPath);
        });

    Map<String, Module> knownFiles =
        packages.stream()
            .flatMap(p -> ScalaConversions.asJava(p.listSources()).stream())
            .collect(
                Collectors.toMap(
                    srcFile -> srcFile.qualifiedName().toString(),
                    srcFile -> new Module(srcFile.qualifiedName(), srcFile.file())));
    topScope = new TopLevelScope(new Builtins(this), knownFiles);

    this.compiler = new Compiler(this);
  }

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
  public Language getLanguage() {
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
   * @return the standard input stream for this context
   */
  public BufferedReader getIn() {
    return in;
  }

  /**
   * Creates a new module scope that automatically imports all the builtin types and methods.
   *
   * @param module the module related to the newly created scope.
   * @return a new module scope with automatic builtins dependency.
   */
  public ModuleScope createScope(Module module) {
    ModuleScope moduleScope = new ModuleScope(module);
    initializeScope(moduleScope);
    return moduleScope;
  }

  /**
   * Creates a new module with scope that automatically imports all the builtin types and methods.
   *
   * @param name the qualified name of the newly created module.
   * @return a new module containing scope with automatic builtins dependency.
   */
  public Module createModule(QualifiedName name) {
    Module module = Module.empty(name);
    initializeScope(module.parseScope(this));
    return module;
  }

  /**
   * Removes all contents from a given scope.
   *
   * @param scope the scope to reset.
   */
  public void resetScope(ModuleScope scope) {
    scope.reset();
    initializeScope(scope);
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
    return packages.stream()
        .filter(pkg -> p.startsWith(pkg.sourceDir()))
        .map(pkg -> pkg.moduleNameForFile(p))
        .findFirst();
  }

  /**
   * Renames project in packages and modules.
   *
   * @param oldName the old project name
   * @param newName the new project name
   */
  public void renameProject(String oldName, String newName) {
    renamePackages(oldName, newName);
    topScope.renameProjectInModules(oldName, newName);
  }


  private void renamePackages(String oldName, String newName) {
    List<Package<TruffleFile>> toChange =
      packages
        .stream()
        .filter(p -> p.config().name().equals(oldName))
        .collect(Collectors.toList());

    packages.removeAll(toChange);

    List<Package<TruffleFile>> renamed =
     toChange
       .stream()
       .map(p -> p.setPackageName(newName))
       .collect(Collectors.toList());

    packages.addAll(renamed);
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
   * Fetches a module with a given name.
   *
   * @param moduleName the qualified name of the module to lookup.
   * @return the relevant module, if exists.
   */
  public Optional<Module> findModule(String moduleName) {
    return getTopScope().getModule(moduleName);
  }

  /**
   * Registers a new module corresponding to a given file.
   *
   * @param path the file to register.
   * @return the newly created module, if the file is a source file.
   */
  public Optional<Module> createModuleForFile(File path) {
    return getModuleNameForFile(path)
        .map(name -> getTopScope().createModule(name, getTruffleFile(path)));
  }

  private void initializeScope(ModuleScope scope) {
    scope.addImport(getBuiltins().getScope());
  }

  /**
   * Gets the builtin functions from the compiler.
   *
   * @return an object containing the builtin functions
   */
  public Builtins getBuiltins() {
    return getTopScope().getBuiltins();
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
   * Returns the atom constructor corresponding to the {@code Unit} type, for builtin constructs
   * that need to return an atom of this type.
   *
   * @return the builtin {@code Unit} atom constructor
   */
  public AtomConstructor getUnit() {
    return getBuiltins().unit();
  }

  /**
   * Checks whether the strict errors option was set for this context.
   *
   * @return true if the strict errors option is enabled, false otherwise.
   */
  public boolean isStrictErrors() {
    return getEnvironment().getOptions().get(RuntimeOptions.STRICT_ERRORS_KEY);
  }

  /** Creates a new thread that has access to the current language context. */
  public Thread createThread(Runnable runnable) {
    return environment.createThread(runnable);
  }

  /** @return the thread manager for this context. */
  public ThreadManager getThreadManager() {
    return threadManager;
  }

  /** @return whether inline caches should be disabled for this context. */
  public boolean isCachingDisabled() {
    return isCachingDisabled;
  }
}
