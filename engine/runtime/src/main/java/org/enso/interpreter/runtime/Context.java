package org.enso.interpreter.runtime;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLanguage.Env;
import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import org.enso.compiler.Compiler;
import org.enso.interpreter.Language;
import org.enso.interpreter.OptionsHelper;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.scope.TopLevelScope;
import org.enso.interpreter.util.ScalaConversions;
import org.enso.pkg.Package;
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
  private final List<Package> packages;

  /**
   * Creates a new Enso context.
   *
   * @param language the language identifier
   * @param environment the execution environment of the {@link TruffleLanguage}
   */
  public Context(Language language, Env environment) {
    this.language = language;
    this.environment = environment;
    this.out = new PrintStream(environment.out());
    this.err = new PrintStream(environment.err());
    this.in = new BufferedReader(new InputStreamReader(environment.in()));

    List<File> packagePaths = OptionsHelper.getPackagesPaths(environment);

    packages =
        packagePaths.stream()
            .map(Package::fromDirectory)
            .map(ScalaConversions::asJava)
            .filter(Optional::isPresent)
            .map(Optional::get)
            .collect(Collectors.toList());

    packages.forEach(
        pkg -> {
          List<File> classPathItems = ScalaConversions.asJava(pkg.listPolyglotExtensions("java"));
          classPathItems.forEach(
              cp -> {
                TruffleFile f = getTruffleFile(cp);
                getEnvironment().addToHostClassPath(f);
              });
        });

    Map<String, Module> knownFiles =
        packages.stream()
            .flatMap(p -> ScalaConversions.asJava(p.listSources()).stream())
            .collect(
                Collectors.toMap(
                    srcFile -> srcFile.qualifiedName().toString(),
                    srcFile ->
                        new Module(
                            srcFile.qualifiedName(),
                            getEnvironment()
                                .getInternalTruffleFile(srcFile.file().getAbsolutePath()))));
    TopLevelScope topLevelScope = new TopLevelScope(new Builtins(this), knownFiles);

    this.compiler = new Compiler(this.language, topLevelScope, this);
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
    initializeScope(module.getScope(this));
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
    return packages.stream()
        .filter(pkg -> path.getAbsolutePath().startsWith(pkg.sourceDir().getAbsolutePath()))
        .map(pkg -> pkg.moduleNameForFile(path))
        .findFirst();
  }

  /**
   * Fetches a module associated with a given file.
   *
   * @param path the module path to lookup.
   * @return the relevant module, if exists.
   */
  public Optional<Module> getModuleForFile(File path) {
    return getModuleNameForFile(path)
        .flatMap(n -> getCompiler().topScope().getModule(n.toString()));
  }

  /**
   * Fetches a module with a given name.
   *
   * @param moduleName the qualified name of the module to lookup.
   * @return the relevant module, if exists.
   */
  public Optional<Module> findModule(String moduleName) {
    return getCompiler().topScope().getModule(moduleName);
  }

  /**
   * Registers a new module corresponding to a given file.
   *
   * @param path the file to register.
   * @return the newly created module, if the file is a source file.
   */
  public Optional<Module> createModuleForFile(File path) {
    return getModuleNameForFile(path)
        .map(name -> getCompiler().topScope().createModule(name, getTruffleFile(path)));
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
    return this.compiler.topScope().getBuiltins();
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
}
