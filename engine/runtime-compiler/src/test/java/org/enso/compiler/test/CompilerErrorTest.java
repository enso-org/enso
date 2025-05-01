package org.enso.compiler.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URI;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.function.Consumer;
import java.util.logging.Level;
import org.enso.common.CompilationStage;
import org.enso.compiler.Compiler;
import org.enso.compiler.PackageRepository;
import org.enso.compiler.Passes;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.context.FreshNameSupply;
import org.enso.compiler.core.ir.Diagnostic;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.expression.errors.Redefined;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.CompilerConfig;
import org.enso.compiler.data.IdMap;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.editions.LibraryName;
import org.enso.filesystem.FileSystem;
import org.enso.pkg.ComponentGroups;
import org.enso.pkg.Package;
import org.enso.pkg.PackageManager;
import org.enso.pkg.QualifiedName;
import org.junit.Test;
import scala.Option;
import scala.collection.concurrent.Map;
import scala.collection.immutable.List;
import scala.collection.immutable.ListSet;
import scala.collection.immutable.Seq;
import scala.runtime.BoxedUnit;
import scala.util.Either;
import scala.util.Right;

public final class CompilerErrorTest {
  @Test
  public void varialesIsRedefinedInIfBranch() {
    var path = "check.enso";
    var qName = QualifiedName.fromString("local.check");
    var code = """
    check x =
        x = 'No'
        x == 'False'
    """;

    var out = new ByteArrayOutputStream();
    var ps = new PrintStream(out);
    var repo = new MockPackageRepository();
    var ctx = new MockCompilerContext(repo, ps);
    var cfg =
        new CompilerConfig(
            true,
            true,
            true,
            true,
            false,
            scala.Option.empty(),
            true,
            true,
            scala.Option.apply(ps));
    var c = new Compiler(ctx, repo, cfg);
    var optPkg = c.getPackageRepository().getMainProjectPackage();
    var m = new MockModule(optPkg.get(), qName, path, code);
    try {
      var res = c.run(m);
      fail("Compilation shall fail, but got: " + res);
    } catch (DiagnosticException t) {
      assertSame(m, t.module);
      assertNotNull(t.diagnostic);
      assertTrue(t.diagnostic instanceof Redefined.Binding);
      var invalid = ((Redefined.Binding) t.diagnostic).invalidBinding();
      assertEquals("x", invalid.name().name());
    }
  }

  private static final class MockModule extends CompilerContext.Module {
    private final QualifiedName qName;
    private final String code;
    private final String path;
    private final Package<? extends Object> pkg;

    private org.enso.compiler.core.ir.Module ir;
    private BindingsMap bm;
    private CompilationStage stage;

    MockModule(Package<?> pkg, QualifiedName qName, String path, String code) {
      this.pkg = pkg;
      this.qName = qName;
      this.path = path;
      this.code = code;
      this.stage = CompilationStage.INITIAL;
    }

    @Override
    public CharSequence getCharacters() throws IOException {
      return code;
    }

    @Override
    public int findLine(IdentifiedLocation loc) {
      throw new UnsupportedOperationException();
    }

    @Override
    public String getPath() {
      return path;
    }

    @Override
    public URI getUri() {
      throw new UnsupportedOperationException();
    }

    @Override
    public Package<? extends Object> getPackage() {
      return pkg;
    }

    @Override
    public QualifiedName getName() {
      return qName;
    }

    @Override
    public BindingsMap getBindingsMap() {
      if (this.getIr() != null) {
        // move to better location than the context
        var meta = this.getIr().passData();
        var pass = meta.get(BindingAnalysis$.MODULE$);
        if (pass.isDefined()) {
          return (BindingsMap) pass.get();
        }
      }
      return bm;
    }

    @Override
    public IdMap getIdMap() {
      throw new UnsupportedOperationException();
    }

    @Override
    public java.util.List<QualifiedName> getDirectModulesRefs() {
      return java.util.List.of();
    }

    @Override
    public CompilationStage getCompilationStage() {
      return stage;
    }

    @Override
    public boolean isSynthetic() {
      return false;
    }

    @Override
    public Module getIr() {
      return ir;
    }

    @Override
    public boolean isPrivate() {
      throw new UnsupportedOperationException();
    }

    @Override
    public CompilerContext.ModuleScopeBuilder getScopeBuilder() {
      throw new UnsupportedOperationException();
    }

    @Override
    public CompilerContext.ModuleScopeBuilder newScopeBuilder() {
      throw new UnsupportedOperationException();
    }
  }

  private static final class DiagnosticException extends RuntimeException {
    final MockModule module;
    final Diagnostic diagnostic;
    final boolean isOutputRedirected;

    DiagnosticException(MockModule module, Diagnostic diagnostic, boolean isOutputRedirected) {
      this.module = module;
      this.diagnostic = diagnostic;
      this.isOutputRedirected = isOutputRedirected;
    }
  }

  private static class MockPackageRepository implements PackageRepository {

    MockPackageRepository() {}

    @Override
    public Either<PackageRepository.Error, BoxedUnit> initialize() {
      return new Right<>(null);
    }

    @Override
    public Either<PackageRepository.Error, BoxedUnit> ensurePackageIsLoaded(
        LibraryName libraryName) {
      throw new UnsupportedOperationException();
    }

    @Override
    public boolean isPackageLoaded(LibraryName libraryName) {
      throw new UnsupportedOperationException();
    }

    @Override
    public Seq<Package<Object>> getLoadedPackages() {
      throw new UnsupportedOperationException();
    }

    @Override
    public Seq<CompilerContext.Module> getLoadedModules() {
      throw new UnsupportedOperationException();
    }

    @Override
    public Map<String, CompilerContext.Module> getModuleMap() {
      throw new UnsupportedOperationException();
    }

    @Override
    public scala.collection.immutable.Map<String, CompilerContext.Module> freezeModuleMap() {
      throw new UnsupportedOperationException();
    }

    @Override
    public scala.collection.immutable.Map<LibraryName, ComponentGroups> getComponents() {
      throw new UnsupportedOperationException();
    }

    @Override
    public ListSet<CompilerContext.Module> getPendingModules() {
      throw new UnsupportedOperationException();
    }

    @Override
    public Option<CompilerContext.Module> getLoadedModule(String qualifiedName) {
      return switch (qualifiedName) {
        case "Standard.Base.Any" -> Option.apply(null);
        default -> throw new UnsupportedOperationException("no module: " + qualifiedName);
      };
    }

    @Override
    public void registerMainProjectPackage(LibraryName libraryName, Package<Object> pkg) {
      throw new UnsupportedOperationException();
    }

    @Override
    @SuppressWarnings("unchecked")
    public Option<Package<Object>> getMainProjectPackage() {
      try {
        var pm = new PackageManager<File>(FileSystem.Default$.MODULE$);
        var tmp = Files.createTempDirectory("mockdir");
        var dir = pm.getOrCreate(tmp.toFile());
        return Option.apply((Package) dir);
      } catch (IOException ex) {
        throw new IllegalStateException(ex);
      }
    }

    @Override
    public void registerModuleCreatedInRuntime(CompilerContext.Module module) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void registerSyntheticPackage(String namespace, String name) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void deregisterModule(String qualifiedName) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void renameProject(String namespace, String oldName, String newName) {
      throw new UnsupportedOperationException();
    }

    @Override
    public boolean isNamespaceRegistered(String namespace) {
      throw new UnsupportedOperationException();
    }

    @Override
    public Option<Package<Object>> getPackageForLibrary(LibraryName lib) {
      throw new UnsupportedOperationException();
    }

    @Override
    public List<CompilerContext.Module> getModulesForLibrary(LibraryName libraryName) {
      throw new UnsupportedOperationException();
    }

    @Override
    public Option<Module> getLibraryBindings(
        LibraryName libraryName, QualifiedName moduleName, CompilerContext context) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void shutdown() {
      throw new UnsupportedOperationException();
    }
  }

  private static final class MockCompilerContext implements CompilerContext {

    private final MockPackageRepository repo;
    private final PrintStream outErr;

    MockCompilerContext(MockPackageRepository repo, PrintStream ps) {
      this.repo = repo;
      this.outErr = ps;
    }

    @Override
    public boolean isIrCachingDisabled() {
      return true;
    }

    @Override
    public boolean isPrivateCheckDisabled() {
      throw new UnsupportedOperationException();
    }

    @Override
    public boolean isUseGlobalCacheLocations() {
      return false;
    }

    @Override
    public boolean isInteractiveMode() {
      return false;
    }

    @Override
    public PackageRepository getPackageRepository() {
      return repo;
    }

    @Override
    public PrintStream getErr() {
      return outErr;
    }

    @Override
    public PrintStream getOut() {
      return outErr;
    }

    @Override
    public void log(Level level, String msg, Object... args) {
      outErr.println(msg + " " + Arrays.toString(args));
    }

    @Override
    public void log(Level level, String msg, Throwable ex) {
      outErr.println("" + msg);
      if (ex != null) {
        ex.printStackTrace(outErr);
      }
    }

    @Override
    public void logSerializationManager(Level level, String msg, Object... args) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void notifySerializeModule(QualifiedName moduleName) {
      throw new UnsupportedOperationException();
    }

    @Override
    public CompilerContext.Module findTopScopeModule(String name) {
      throw new UnsupportedOperationException();
    }

    @Override
    public RuntimeException formatDiagnostic(
        CompilerContext.Module module, Diagnostic diagnostic, boolean isOutputRedirected) {
      return new DiagnosticException((MockModule) module, diagnostic, isOutputRedirected);
    }

    @Override
    public boolean isCreateThreadAllowed() {
      throw new UnsupportedOperationException();
    }

    @Override
    public ExecutorService newParsingPool() {
      throw new UnsupportedOperationException();
    }

    @Override
    public void truffleRunCodegen(
        CompilerContext.Module module,
        CompilerContext.ModuleScopeBuilder scopeBuilder,
        CompilerConfig config)
        throws IOException {
      throw new UnsupportedOperationException();
    }

    @Override
    public void runStubsGenerator(
        CompilerContext.Module module, CompilerContext.ModuleScopeBuilder scopeBuilder) {
      throw new UnsupportedOperationException();
    }

    @Override
    public boolean typeContainsValues(String name) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void initializeBuiltinsIr(
        Compiler compiler,
        boolean irCachingEnabled,
        FreshNameSupply freshNameSupply,
        Passes passes) {}

    @Override
    public IdMap getIdMap(CompilerContext.Module module) {
      return null;
    }

    @Override
    public void updateModule(
        CompilerContext.Module raw, Consumer<CompilerContext.Updater> callback) {
      var module = (MockModule) raw;
      callback.accept(
          new Updater() {
            @Override
            public void bindingsMap(BindingsMap map) {
              module.bm = map;
            }

            @Override
            public void idMap(IdMap idMap) {
              throw new UnsupportedOperationException();
            }

            @Override
            public void ir(org.enso.compiler.core.ir.Module ir) {
              module.ir = ir;
            }

            @Override
            public void compilationStage(CompilationStage stage) {
              module.stage = stage;
            }

            @Override
            public void loadedFromCache(boolean b) {}

            @Override
            public void resetScope() {}

            @Override
            public void invalidateCache() {}
          });
    }

    @Override
    public boolean isInteractive(CompilerContext.Module module) {
      throw new UnsupportedOperationException();
    }

    @Override
    public boolean isModuleInRootPackage(CompilerContext.Module module) {
      throw new UnsupportedOperationException();
    }

    @Override
    public boolean wasLoadedFromCache(CompilerContext.Module module) {
      return false;
    }

    @Override
    public Future<Boolean> serializeLibrary(
        Compiler compiler, LibraryName libraryName, boolean useGlobalCacheLocations) {
      throw new UnsupportedOperationException();
    }

    @Override
    public Option<Object> deserializeSuggestions(LibraryName libraryName)
        throws InterruptedException {
      throw new UnsupportedOperationException();
    }

    @Override
    public Future<Boolean> serializeModule(
        Compiler compiler,
        CompilerContext.Module module,
        boolean useGlobalCacheLocations,
        boolean usePool) {
      throw new UnsupportedOperationException();
    }

    @Override
    public boolean deserializeModule(Compiler compiler, CompilerContext.Module module) {
      throw new UnsupportedOperationException();
    }

    @Override
    public void shutdown(boolean waitForPendingJobCompletion) {
      throw new UnsupportedOperationException();
    }

    @Override
    public RuntimeException throwAbortedException() {
      throw new UnsupportedOperationException();
    }
  }
}
