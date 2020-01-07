package org.enso.interpreter.runtime;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.TruffleLanguage.Env;
import org.enso.compiler.Compiler;
import org.enso.interpreter.Language;
import org.enso.interpreter.runtime.callable.atom.AtomConstructor;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.scope.TopLevelScope;
import org.enso.interpreter.util.ScalaConversions;
import org.enso.pkg.Package;

import java.io.File;
import java.io.PrintStream;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * The language context is the internal state of the language that is associated with each thread in
 * a running Enso program.
 */
public class Context {

  private final Language language;
  private final Env environment;
  private final Compiler compiler;
  private final PrintStream out;

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

    List<File> packagePaths = RuntimeOptions.getPackagesPaths(environment);
    Map<String, Module> knownFiles =
        packagePaths.stream()
            .map(Package::fromDirectory)
            .map(ScalaConversions::asJava)
            .filter(Optional::isPresent)
            .map(Optional::get)
            .flatMap(p -> ScalaConversions.asJava(p.listSources()).stream())
            .collect(
                Collectors.toMap(
                    srcFile -> srcFile.qualifiedName().toString(),
                    srcFile ->
                        new Module(
                            srcFile.qualifiedName(),
                            getEnvironment()
                                .getInternalTruffleFile(srcFile.file().getAbsolutePath()))));
    TopLevelScope topLevelScope = new TopLevelScope(new Builtins(language), knownFiles);

    this.compiler = new Compiler(this.language, topLevelScope, this);
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
  public final Compiler compiler() {
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
   * Creates a new module scope that automatically imports all the builtin types and methods.
   *
   * @param name the name of the newly created scope.
   *
   * @return a new module scope with automatic builtins dependency.
   */
  public ModuleScope createScope(String name) {
    ModuleScope moduleScope = new ModuleScope(name);
    moduleScope.addImport(getBuiltins().getScope());
    return moduleScope;
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
}
