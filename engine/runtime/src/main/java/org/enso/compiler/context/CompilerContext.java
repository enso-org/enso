package org.enso.compiler.context;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.source.Source;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.logging.Level;
import org.enso.compiler.Cache;
import org.enso.compiler.Compiler;
import org.enso.compiler.PackageRepository;
import org.enso.compiler.Passes;
import org.enso.compiler.SerializationManager;
import org.enso.compiler.core.IR;
import org.enso.compiler.data.CompilerConfig;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.Module;
import org.enso.interpreter.runtime.scope.LocalScope;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.scope.TopLevelScope;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.CompilationStage;

/**
 * Interface that encapsulate all services {@link Compiler} needs from Truffle or other environment.
 * It is implemented by {@code TruffleCompilerContext}. This adds an abstraction between {@link
 * Compiler} and the information it needs from the runtime. The ultimate state is to compile the
 * {@link Compiler} & co. classes separately without any dependency on Truffle API.
 */
public interface CompilerContext {
  boolean isIrCachingDisabled();

  boolean isUseGlobalCacheLocations();

  boolean isInteractiveMode();

  PackageRepository getPackageRepository();

  PrintStream getErr();

  PrintStream getOut();

  void log(Level level, String msg, Object... args);

  void logSerializationManager(Level level, String msg, Object... args);

  void notifySerializeModule(QualifiedName moduleName);

  TopLevelScope getTopScope();

  // threads
  boolean isCreateThreadAllowed();

  Thread createThread(Runnable r);

  Thread createSystemThread(Runnable r);

  // Truffle related

  void truffleRunCodegen(Module module, CompilerConfig config) throws IOException;

  void truffleRunCodegen(Source source, ModuleScope scope, CompilerConfig config, IR.Module ir);

  ExpressionNode truffleRunInline(
      Source source, LocalScope localScope, Module module, CompilerConfig config, IR.Expression ir);

  // module related

  void runStubsGenerator(Module module);

  boolean typeContainsValues(String name);

  void initializeBuiltinsIr(
      boolean irCachingEnabled,
      SerializationManager serializationManager,
      FreshNameSupply freshNameSupply,
      Passes passes);

  QualifiedName getModuleName(Module module);

  CharSequence getCharacters(Module module) throws IOException;

  void updateModule(Module module, Consumer<Updater> callback);

  boolean isSynthetic(Module module);

  boolean isInteractive(Module module);

  boolean wasLoadedFromCache(Module module);

  boolean hasCrossModuleLinks(Module module);

  IR.Module getIr(Module module);

  CompilationStage getCompilationStage(Module module);

  <T> Optional<T> loadCache(Cache<T, ?> cache);

  <T> Optional<TruffleFile> saveCache(Cache<T, ?> cache, T entry, boolean useGlobalCacheLocations);

  public static interface Updater {
    void ir(IR.Module ir);

    void compilationStage(CompilationStage stage);

    void loadedFromCache(boolean b);

    void hasCrossModuleLinks(boolean b);

    void resetScope();

    void invalidateCache();
  }
}
