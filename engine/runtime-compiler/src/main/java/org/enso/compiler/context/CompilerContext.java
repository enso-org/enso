package org.enso.compiler.context;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.source.Source;
import java.io.IOException;
import java.io.PrintStream;
import java.util.List;
import java.util.concurrent.Future;
import java.util.function.Consumer;
import java.util.logging.Level;
import org.enso.compiler.Compiler;
import org.enso.compiler.PackageRepository;
import org.enso.compiler.Passes;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.CompilerConfig;
import org.enso.editions.LibraryName;
import org.enso.pkg.Package;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.CompilationStage;
import org.enso.polyglot.data.TypeGraph;

/**
 * Interface that encapsulate all services {@link Compiler} needs from Truffle or other environment.
 * It is implemented by {@code TruffleCompilerContext}. This adds an abstraction between {@link
 * Compiler} and the information it needs from the runtime. The ultimate state is to compile the
 * {@link Compiler} & co. classes separately without any dependency on Truffle API.
 */
public interface CompilerContext {
  boolean isIrCachingDisabled();

  boolean isPrivateCheckDisabled();

  boolean isUseGlobalCacheLocations();

  boolean isInteractiveMode();

  PackageRepository getPackageRepository();

  PrintStream getErr();

  PrintStream getOut();

  void log(Level level, String msg, Object... args);

  void logSerializationManager(Level level, String msg, Object... args);

  void notifySerializeModule(QualifiedName moduleName);

  Module findTopScopeModule(String name);

  // threads
  boolean isCreateThreadAllowed();

  Thread createThread(Runnable r);

  Thread createSystemThread(Runnable r);

  // Truffle related

  void truffleRunCodegen(Module module, CompilerConfig config) throws IOException;

  // module related

  void runStubsGenerator(Module module);

  boolean typeContainsValues(String name);

  void initializeBuiltinsIr(
      Compiler compiler, boolean irCachingEnabled, FreshNameSupply freshNameSupply, Passes passes);

  QualifiedName getModuleName(Module module);

  CharSequence getCharacters(Module module) throws IOException;

  void updateModule(Module module, Consumer<Updater> callback);

  boolean isSynthetic(Module module);

  boolean isInteractive(Module module);

  boolean wasLoadedFromCache(Module module);

  boolean hasCrossModuleLinks(Module module);

  org.enso.compiler.core.ir.Module getIr(Module module);

  CompilationStage getCompilationStage(Module module);

  TypeGraph getTypeHierarchy();

  Future<Boolean> serializeLibrary(
      Compiler compiler, LibraryName libraryName, boolean useGlobalCacheLocations);

  Future<Boolean> serializeModule(
      Compiler compiler, Module module, boolean useGlobalCacheLocations);

  boolean deserializeModule(Compiler compiler, Module module);

  void shutdown(boolean waitForPendingJobCompletion);

  public static interface Updater {
    void bindingsMap(BindingsMap map);

    void ir(org.enso.compiler.core.ir.Module ir);

    void compilationStage(CompilationStage stage);

    void loadedFromCache(boolean b);

    void hasCrossModuleLinks(boolean b);

    void resetScope();

    void invalidateCache();
  }

  public abstract static class Module {
    public abstract Source getSource() throws IOException;

    public abstract String getPath();

    public abstract Package<TruffleFile> getPackage();

    public abstract QualifiedName getName();

    public abstract BindingsMap getBindingsMap();

    public abstract TruffleFile getSourceFile();

    public abstract List<QualifiedName> getDirectModulesRefs();

    public abstract CompilationStage getCompilationStage();

    public abstract boolean isSynthetic();

    public abstract boolean hasCrossModuleLinks();

    public abstract org.enso.compiler.core.ir.Module getIr();

    public abstract boolean isPrivate();
  }
}
