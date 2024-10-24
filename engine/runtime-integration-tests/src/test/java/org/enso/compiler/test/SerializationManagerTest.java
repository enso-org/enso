package org.enso.compiler.test;

import com.oracle.truffle.api.TruffleFile;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Supplier;
import java.util.stream.Stream;
import org.apache.commons.io.FileUtils;
import org.enso.common.LanguageInfo;
import org.enso.common.MethodNames;
import org.enso.editions.LibraryName;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.util.TruffleFileSystem;
import org.enso.interpreter.test.InterpreterContext;
import org.enso.interpreter.util.ScalaConversions;
import org.enso.pkg.Package;
import org.enso.pkg.PackageManager;
import org.enso.polyglot.Suggestion;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class SerializationManagerTest {

  private static final long COMPILE_TIMEOUT_SECONDS = 20;

  private PackageManager<TruffleFile> packageManager;
  private InterpreterContext interpreterContext;
  private EnsoContext ensoContext;

  @Before
  public void setup() {
    packageManager = new PackageManager<>(new TruffleFileSystem());
    interpreterContext = new InterpreterContext(x -> x);
    ensoContext =
        interpreterContext
            .ctx()
            .getBindings(LanguageInfo.ID)
            .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
            .asHostObject();
    interpreterContext.ctx().initialize(LanguageInfo.ID);
    interpreterContext.ctx().enter();
  }

  @After
  public void teardown() {
    interpreterContext.close();
    ensoContext.shutdown();
    ensoContext = null;
  }

  private Path getLibraryPath(LibraryName libraryName) {
    return Paths.get(
            interpreterContext.languageHome().toFile().getAbsolutePath(),
            "..",
            "lib",
            libraryName.namespace(),
            libraryName.name(),
            interpreterContext.edition())
        .toAbsolutePath()
        .normalize();
  }

  private Package<TruffleFile> getLibraryPackage(LibraryName libraryName) {
    Path libraryPath = getLibraryPath(libraryName);
    TruffleFile libraryFile = ensoContext.getPublicTruffleFile(libraryPath.toString());
    return packageManager.loadPackage(libraryFile).get();
  }

  private void clearLibraryCache(LibraryName libraryName) {
    Path libraryCachePath = Paths.get(getLibraryPath(libraryName).toString(), ".enso");
    FileUtils.deleteQuietly(libraryCachePath.toFile());
  }

  @Test
  @SuppressWarnings("unchecked")
  public void serializeLibrarySuggestions()
      throws ExecutionException, InterruptedException, TimeoutException {
    LibraryName standardBaseLibrary = new LibraryName("Standard", "Base");
    Package<TruffleFile> standardBasePackage = getLibraryPackage(standardBaseLibrary);
    ensoContext
        .getPackageRepository()
        .registerMainProjectPackage(standardBaseLibrary, standardBasePackage);

    Object result =
        ensoContext
            .getCompiler()
            .compile(false, true, false)
            .get(COMPILE_TIMEOUT_SECONDS, TimeUnit.SECONDS);
    Assert.assertEquals(Boolean.TRUE, result);

    var cachedSuggestions =
        (java.util.List<Suggestion>)
            ensoContext.getCompiler().context().deserializeSuggestions(standardBaseLibrary).get();

    Supplier<Stream<Suggestion.Constructor>> cachedConstructorSuggestions =
        () ->
            cachedSuggestions.stream()
                .flatMap(
                    suggestion -> {
                      if (suggestion instanceof Suggestion.Constructor constructor) {
                        return Stream.of(constructor);
                      }
                      return Stream.empty();
                    });

    Suggestion.Constructor booleanTrueSuggestion =
        cachedConstructorSuggestions
            .get()
            .filter(constructor -> constructor.name().equals("True"))
            .findFirst()
            .get();
    Assert.assertEquals(
        ScalaConversions.set("Standard.Base.Main", "Standard.Base.Data.Boolean").toList(),
        booleanTrueSuggestion.reexports().toList());

    Suggestion.Constructor runtimeContextInputSuggestion =
        cachedConstructorSuggestions
            .get()
            .filter(constructor -> constructor.name().equals("Input"))
            .findFirst()
            .get();
    Assert.assertEquals(
        ScalaConversions.set().toList(), runtimeContextInputSuggestion.reexports().toList());

    clearLibraryCache(standardBaseLibrary);
  }
}
