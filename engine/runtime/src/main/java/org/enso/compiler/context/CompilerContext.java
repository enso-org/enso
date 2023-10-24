package org.enso.compiler.context;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.source.Source;
import java.io.IOException;
import java.io.PrintStream;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.logging.Level;
import org.enso.compiler.Cache;
import org.enso.compiler.Compiler;
import org.enso.compiler.ModuleCache;
import org.enso.compiler.PackageRepository;
import org.enso.compiler.Passes;
import org.enso.compiler.SerializationManager;
import org.enso.compiler.core.ir.Expression;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.CompilerConfig;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.scope.LocalScope;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.scope.TopLevelScope;
import org.enso.pkg.Package;
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

  void truffleRunCodegen(
      Source source, ModuleScope scope, CompilerConfig config, org.enso.compiler.core.ir.Module ir);

  ExpressionNode truffleRunInline(
      Source source, LocalScope localScope, Module module, CompilerConfig config, Expression ir);

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

  org.enso.compiler.core.ir.Module getIr(Module module);

  CompilationStage getCompilationStage(Module module);

  <T> Optional<T> loadCache(Cache<T, ?> cache);

  <T> Optional<TruffleFile> saveCache(Cache<T, ?> cache, T entry, boolean useGlobalCacheLocations);

  public static interface Updater {
    void ir(org.enso.compiler.core.ir.Module ir);

    void compilationStage(CompilationStage stage);

    void loadedFromCache(boolean b);

    void hasCrossModuleLinks(boolean b);

    void resetScope();

    void invalidateCache();
  }

  public static final class Module {
    private final org.enso.interpreter.runtime.Module module;

    public Module(org.enso.interpreter.runtime.Module module) {
      this.module = module;
    }

    public Source getSource() throws IOException {
      return module.getSource();
    }

    public String getPath() {
      return module.getPath();
    }

    public Package<TruffleFile> getPackage() {
      return module.getPackage();
    }

    // XXX
    public org.enso.interpreter.runtime.Module unsafeModule() {
      return module;
    }

    public boolean isSameAs(org.enso.interpreter.runtime.Module m) {
      return module == m;
    }

    // XXX
    public org.enso.interpreter.runtime.scope.ModuleScope getScope() {
      return module.getScope();
    }

    public QualifiedName getName() {
      return module.getName();
    }

    public Type findType(String name) {
      return module.getScope().getTypes().get(name);
    }

    // XXX
    public BindingsMap getBindingsMap() {
      var meta = module.getIr().passData();
      var pass = meta.get(BindingAnalysis$.MODULE$);
      return (BindingsMap) pass.get();
    }

    public TruffleFile getSourceFile() {
      return module.getSourceFile();
    }

    public boolean isInteractive() {
      return module.isInteractive();
    }

    public List<QualifiedName> getDirectModulesRefs() {
      return module.getDirectModulesRefs();
    }

    public ModuleCache getCache() {
      return module.getCache();
    }

    public CompilationStage getCompilationStage() {
      return module.getCompilationStage();
    }

    public boolean isSynthetic() {
      return module.isSynthetic();
    }

    public boolean hasCrossModuleLinks() {
      return module.hasCrossModuleLinks();
    }

    public org.enso.compiler.core.ir.Module getIr() {
      return module.getIr();
    }

    public boolean isPrivate() {
      return module.isPrivate();
    }

    @Override
    public int hashCode() {
      int hash = 7;
      hash = 67 * hash + Objects.hashCode(this.module);
      return hash;
    }

    @Override
    public boolean equals(Object obj) {
      if (this == obj) {
        return true;
      }
      if (obj == null) {
        return false;
      }
      if (getClass() != obj.getClass()) {
        return false;
      }
      final Module other = (Module) obj;
      return Objects.equals(this.module, other.module);
    }
  }
}
