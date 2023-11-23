package org.enso.compiler;

import com.oracle.truffle.api.TruffleFile;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.function.Supplier;
import java.util.stream.Stream;
import org.apache.commons.io.FileUtils;
import org.enso.editions.LibraryName;
import org.enso.interpreter.caches.SuggestionsCache;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.SerializationManager;
import org.enso.interpreter.runtime.util.TruffleFileSystem;
import org.enso.interpreter.test.InterpreterContext;
import org.enso.pkg.Package;
import org.enso.pkg.PackageManager;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.MethodNames;
import org.enso.polyglot.Suggestion;
import org.junit.*;

public class SerializationManagerTest {

  private static final long COMPILE_TIMEOUT_SECONDS = 20;

  private final PackageManager<TruffleFile> packageManager;
  private final InterpreterContext interpreterContext;
  private final EnsoContext ensoContext;

  public SerializationManagerTest() {
    packageManager = new PackageManager<>(new TruffleFileSystem());
    interpreterContext = new InterpreterContext(x -> x);
    ensoContext =
        interpreterContext
            .ctx()
            .getBindings(LanguageInfo.ID)
            .invokeMember(MethodNames.TopScope.LEAK_CONTEXT)
            .asHostObject();
  }

  @Before
  public void setup() {
    interpreterContext.ctx().initialize(LanguageInfo.ID);
    interpreterContext.ctx().enter();
  }

  @After
  public void teardown() {
    interpreterContext.ctx().close();
  }

  private Path getLibraryPath(LibraryName libraryName) {
    return Paths.get(
            interpreterContext.languageHome(),
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
  public void serializeLibrarySuggestions()
      throws ExecutionException, InterruptedException, TimeoutException {
    SerializationManager serializationManager = new SerializationManager(ensoContext.getCompiler());
    LibraryName standardBaseLibrary = new LibraryName("Standard", "Base");
    Package<TruffleFile> standardBasePackage = getLibraryPackage(standardBaseLibrary);
    ensoContext
        .getPackageRepository()
        .registerMainProjectPackage(standardBaseLibrary, standardBasePackage);

    Object result =
        ensoContext
            .getCompiler()
            .compile(false, false)
            .get(COMPILE_TIMEOUT_SECONDS, TimeUnit.SECONDS);
    Assert.assertEquals(Boolean.TRUE, result);

    SuggestionsCache.CachedSuggestions cachedSuggestions =
        serializationManager.deserializeSuggestions(standardBaseLibrary).get();
    Assert.assertEquals(standardBaseLibrary, cachedSuggestions.getLibraryName());

    Supplier<Stream<Suggestion.Constructor>> cachedConstructorSuggestions =
        () ->
            cachedSuggestions.getSuggestions().stream()
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
    Assert.assertEquals(scala.Some.apply("Standard.Base.Main"), booleanTrueSuggestion.reexport());

    Suggestion.Constructor runtimeContextInputSuggestion =
        cachedConstructorSuggestions
            .get()
            .filter(constructor -> constructor.name().equals("Input"))
            .findFirst()
            .get();
    Assert.assertEquals(scala.None$.MODULE$, runtimeContextInputSuggestion.reexport());

    clearLibraryCache(standardBaseLibrary);
  }
}
