package org.enso.compiler.context;

import java.io.IOException;
import java.io.PrintStream;
import java.util.List;
import java.util.concurrent.Future;
import java.util.function.Consumer;
import java.util.logging.Level;
import org.enso.common.CompilationStage;
import org.enso.compiler.Compiler;
import org.enso.compiler.PackageRepository;
import org.enso.compiler.Passes;
import org.enso.compiler.core.CompilerStub;
import org.enso.compiler.core.ir.Diagnostic;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.CompilerConfig;
import org.enso.compiler.data.IdMap;
import org.enso.editions.LibraryName;
import org.enso.pkg.Package;
import org.enso.pkg.QualifiedName;

/**
 * Interface that encapsulate all services {@link Compiler} needs from Truffle or other environment.
 * It is implemented by {@code TruffleCompilerContext}. This adds an abstraction between {@link
 * Compiler} and the information it needs from the runtime. The ultimate state is to compile the
 * {@link Compiler} & co. classes separately without any dependency on Truffle API.
 */
public interface CompilerContext extends CompilerStub {

  boolean isIrCachingDisabled();

  boolean isPrivateCheckDisabled();

  boolean isUseGlobalCacheLocations();

  boolean isInteractiveMode();

  PackageRepository getPackageRepository();

  PrintStream getErr();

  PrintStream getOut();

  void log(Level level, String msg, Object... args);

  void log(Level level, String msg, Throwable ex);

  void logSerializationManager(Level level, String msg, Object... args);

  void notifySerializeModule(QualifiedName moduleName);

  Module findTopScopeModule(String name);

  /**
   * Format the given diagnostic into a string. The returned string might have ANSI colors.
   *
   * @param module may be null if inline diagnostics is required.
   * @param diagnostic an IR node representing diagnostic information
   * @param isOutputRedirected true if the output is not system's out. If true, no ANSI color escape
   *     characters will be inside the returned string.
   * @return exception with a message to display or to throw
   */
  RuntimeException formatDiagnostic(
      Module module, Diagnostic diagnostic, boolean isOutputRedirected);

  // threads
  boolean isCreateThreadAllowed();

  Thread createThread(Runnable r);

  Thread createSystemThread(Runnable r);

  // Truffle related

  void truffleRunCodegen(Module module, ModuleScopeBuilder scopeBuilder, CompilerConfig config)
      throws IOException;

  // module related

  void runStubsGenerator(Module module, ModuleScopeBuilder scopeBuilder);

  boolean typeContainsValues(String name);

  void initializeBuiltinsIr(
      Compiler compiler, boolean irCachingEnabled, FreshNameSupply freshNameSupply, Passes passes);

  QualifiedName getModuleName(Module module);

  CharSequence getCharacters(Module module) throws IOException;

  IdMap getIdMap(Module module);

  void updateModule(Module module, Consumer<Updater> callback);

  boolean isSynthetic(Module module);

  boolean isInteractive(Module module);

  boolean isModuleInRootPackage(Module module);

  boolean wasLoadedFromCache(Module module);

  org.enso.compiler.core.ir.Module getIr(Module module);

  CompilationStage getCompilationStage(Module module);

  Future<Boolean> serializeLibrary(
      Compiler compiler, LibraryName libraryName, boolean useGlobalCacheLocations);

  scala.Option<Object> deserializeSuggestions(LibraryName libraryName) throws InterruptedException;

  Future<Boolean> serializeModule(
      Compiler compiler, Module module, boolean useGlobalCacheLocations, boolean usePool);

  boolean deserializeModule(Compiler compiler, Module module);

  void shutdown(boolean waitForPendingJobCompletion);

  RuntimeException throwAbortedException();

  public static interface Updater {
    void bindingsMap(BindingsMap map);

    void idMap(IdMap idMap);

    void ir(org.enso.compiler.core.ir.Module ir);

    void compilationStage(CompilationStage stage);

    void loadedFromCache(boolean b);

    void resetScope();

    void resetScope(boolean resetTypes);

    void invalidateCache();
  }

  public abstract static class Module {

    public abstract CharSequence getCharacters() throws IOException;

    public abstract String getPath();

    public abstract Package<? extends Object> getPackage();

    public abstract QualifiedName getName();

    public abstract BindingsMap getBindingsMap();

    public abstract IdMap getIdMap();

    public abstract List<QualifiedName> getDirectModulesRefs();

    public abstract CompilationStage getCompilationStage();

    public abstract boolean isSynthetic();

    public abstract org.enso.compiler.core.ir.Module getIr();

    public abstract boolean isPrivate();

    public abstract ModuleScopeBuilder getScopeBuilder();

    public abstract ModuleScopeBuilder newScopeBuilder();
  }

  public abstract static class ModuleScopeBuilder {}
}
