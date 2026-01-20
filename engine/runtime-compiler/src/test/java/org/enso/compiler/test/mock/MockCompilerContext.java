package org.enso.compiler.test.mock;

import java.io.PrintStream;
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
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.CompilerConfig;
import org.enso.compiler.data.IdMap;
import org.enso.editions.LibraryName;
import org.enso.pkg.QualifiedName;
import scala.Option;

final class MockCompilerContext implements CompilerContext {

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
  public Module findTopScopeModule(String name) {
    var mod = repo.getLoadedModule(name);
    return mod.isEmpty() ? null : mod.get();
  }

  @Override
  public RuntimeException formatDiagnostic(
      Module module, Diagnostic diagnostic, boolean isOutputRedirected) {
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
      Module module, ModuleScopeBuilder scopeBuilder, CompilerConfig config) {
    // nop
  }

  @Override
  public void runStubsGenerator(Module module, ModuleScopeBuilder scopeBuilder) {
    // nop
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
  public IdMap getIdMap(Module module) {
    return null;
  }

  @Override
  public void updateModule(Module raw, Consumer<Updater> callback) {
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
  public boolean isInteractive(Module module) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean isModuleInRootPackage(Module module) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean wasLoadedFromCache(Module module) {
    return false;
  }

  @Override
  public Future<Boolean> serializeLibrary(Compiler compiler, LibraryName libraryName) {
    throw new UnsupportedOperationException();
  }

  @Override
  public Option<Object> deserializeSuggestions(LibraryName libraryName)
      throws InterruptedException {
    throw new UnsupportedOperationException();
  }

  @Override
  public Future<Boolean> serializeModule(Compiler compiler, Module module, boolean usePool) {
    throw new UnsupportedOperationException();
  }

  @Override
  public boolean deserializeModule(Compiler compiler, Module module) {
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
