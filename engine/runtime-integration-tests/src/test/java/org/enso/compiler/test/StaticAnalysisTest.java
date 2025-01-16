package org.enso.compiler.test;

import com.oracle.truffle.api.TruffleFile;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.common.RuntimeOptions;
import org.enso.compiler.core.ir.Module;
import org.enso.editions.LibraryName;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.util.TruffleFileSystem;
import org.enso.interpreter.test.InterpreterContext;
import org.enso.pkg.Config;
import org.enso.pkg.Contact;
import org.enso.pkg.Package;
import org.enso.pkg.QualifiedName;
import org.graalvm.polyglot.Source;
import scala.Option;
import scala.jdk.javaapi.CollectionConverters;

public abstract class StaticAnalysisTest {

  /**
   * The interpreter context is needed here as it ensures initialization of everything needed to
   * perform imports resolution, including PackageRepository.
   *
   * <p>Ideally, the tests for the static analysis capabilities of the compiler should _not_ depend
   * on the Graal runtime context, as they should be runnable in other contexts - i.e. in a Visual
   * Studio Code language server.
   */
  private final InterpreterContext interpreterContext =
      new InterpreterContext(
          (builder) ->
              builder
                  .option(RuntimeOptions.ENABLE_STATIC_ANALYSIS, "true")
                  .option(RuntimeOptions.LOG_LEVEL, Level.INFO.getName())
                  .option(RuntimeOptions.LOG_LEVEL, Level.SEVERE.getName())
                  .out(OutputStream.nullOutputStream())
                  .err(OutputStream.nullOutputStream()));

  private final EnsoContext langCtx =
      interpreterContext
          .ctx()
          .getBindings(LanguageInfo.ID)
          .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
          .asHostObject();

  private final Map<LibraryName, Package<TruffleFile>> syntheticTestPackages = new HashMap<>();

  protected Module compile(Source src) {
    String suffix = ".enso";
    String name = src.getName();
    if (!name.endsWith(suffix)) {
      throw new IllegalArgumentException("Source name must end with " + suffix);
    }
    QualifiedName qualifiedName =
        QualifiedName.fromString(name.substring(0, name.length() - suffix.length()));

    var packageRepository = langCtx.getPackageRepository();
    Package<TruffleFile> pkg = null;

    // If the module name is supposed to be put in a project, we register a synthetic project entry
    // for it
    if (qualifiedName.path().length() >= 2) {
      var libraryName =
          new LibraryName(qualifiedName.path().apply(0), qualifiedName.path().apply(1));
      if (!packageRepository.isPackageLoaded(libraryName)) {
        // We are able only to register a synthetic package without associated Package<> object, but
        // perhaps that's fine.
        packageRepository.registerSyntheticPackage(libraryName.namespace(), libraryName.name());
        assert packageRepository.isPackageLoaded(libraryName);
      }

      pkg = makeSyntheticPackageForTestProject(libraryName);
    }

    // This creates the module and also registers it in the scope, so that import resolution will
    // see it.
    var module =
        langCtx.getTopScope().createModule(qualifiedName, pkg, src.getCharacters().toString());
    langCtx.getCompiler().run(module.asCompilerModule());
    return module.getIr();
  }

  private Package<TruffleFile> makeSyntheticPackageForTestProject(LibraryName name) {
    return syntheticTestPackages.computeIfAbsent(
        name,
        (unused) -> {
          try {
            var tmpRoot = Files.createTempDirectory("test-project-" + name);
            TruffleFile root = langCtx.getPublicTruffleFile(tmpRoot.toString());
            List<Contact> contacts = List.of();
            Config initialConfig =
                new Config(
                    name.name(),
                    Option.empty(),
                    name.namespace(),
                    "0.0.0",
                    "",
                    CollectionConverters.asScala(contacts).toList(),
                    CollectionConverters.asScala(contacts).toList(),
                    Option.empty(),
                    true,
                    Option.empty());
            return new Package<>(root, initialConfig, TruffleFileSystem.INSTANCE);
          } catch (IOException e) {
            throw new RuntimeException(e);
          }
        });
  }
}
