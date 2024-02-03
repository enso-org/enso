package org.enso.interpreter.runtime;

import static org.enso.interpreter.util.ScalaConversions.cons;
import static org.enso.interpreter.util.ScalaConversions.nil;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.source.Source;
import java.io.IOException;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import java.util.function.Consumer;
import java.util.logging.Level;
import org.enso.compiler.Compiler;
import org.enso.compiler.PackageRepository;
import org.enso.compiler.Passes;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.context.FreshNameSupply;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.CompilerConfig;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.editions.LibraryName;
import org.enso.interpreter.caches.Cache;
import org.enso.interpreter.caches.ImportExportCache.MapToBindings;
import org.enso.interpreter.caches.ModuleCache;
import org.enso.interpreter.runtime.type.Types;
import org.enso.pkg.Package;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.CompilationStage;
import org.enso.polyglot.LanguageInfo;
import org.enso.polyglot.data.TypeGraph;

final class TruffleCompilerContext implements CompilerContext {

  private final EnsoContext context;
  private final TruffleLogger loggerCompiler;
  private final TruffleLogger loggerSerializationManager;
  private final RuntimeStubsGenerator stubsGenerator;
  private final SerializationManager serializationManager;

  TruffleCompilerContext(EnsoContext context) {
    this.context = context;
    this.loggerCompiler = context.getLogger(Compiler.class);
    this.loggerSerializationManager = context.getLogger(SerializationManager.class);
    this.serializationManager = new SerializationManager(this);
    this.stubsGenerator = new RuntimeStubsGenerator(context.getBuiltins());
  }

  @Override
  public boolean isIrCachingDisabled() {
    return context.isIrCachingDisabled();
  }

  @Override
  public boolean isPrivateCheckDisabled() {
    return context.isPrivateCheckDisabled();
  }

  @Override
  public boolean isUseGlobalCacheLocations() {
    return context.isUseGlobalCache();
  }

  @Override
  public boolean isInteractiveMode() {
    return context.isInteractiveMode();
  }

  @Override
  public PackageRepository getPackageRepository() {
    return context.getPackageRepository();
  }

  final SerializationManager getSerializationManager() {
    return serializationManager;
  }

  @Override
  public PrintStream getErr() {
    return context.getErr();
  }

  @Override
  public PrintStream getOut() {
    return context.getOut();
  }

  @Override
  public void log(Level level, String msg, Object... args) {
    loggerCompiler.log(level, msg, args);
  }

  @Override
  public void log(Level level, String msg, Throwable ex) {
    loggerCompiler.log(level, msg, ex);
  }

  @Override
  public void logSerializationManager(Level level, String msg, Object... args) {
    loggerSerializationManager.log(level, msg, args);
  }

  @Override
  public void notifySerializeModule(QualifiedName moduleName) {
    context.getNotificationHandler().serializeModule(moduleName);
  }

  @Override
  public boolean isCreateThreadAllowed() {
    return context.isCreateThreadAllowed();
  }

  @Override
  public Thread createThread(Runnable r) {
    return context.createThread(false, r);
  }

  @Override
  public Thread createSystemThread(Runnable r) {
    return context.createThread(true, r);
  }

  @Override
  public void truffleRunCodegen(CompilerContext.Module module, CompilerConfig config)
      throws IOException {
    var m = org.enso.interpreter.runtime.Module.fromCompilerModule(module);
    new IrToTruffle(context, module.getSource(), m.getScope(), config).run(module.getIr());
  }

  // module related
  @Override
  public QualifiedName getModuleName(CompilerContext.Module module) {
    return module.getName();
  }

  @Override
  public CharSequence getCharacters(CompilerContext.Module module) throws IOException {
    return module.getSource().getCharacters();
  }

  @Override
  public boolean isSynthetic(CompilerContext.Module module) {
    return module.isSynthetic();
  }

  @Override
  public boolean isInteractive(CompilerContext.Module module) {
    return ((Module) module).unsafeModule().isInteractive();
  }

  @Override
  public boolean wasLoadedFromCache(CompilerContext.Module module) {
    return ((Module) module).unsafeModule().wasLoadedFromCache();
  }

  @Override
  public org.enso.compiler.core.ir.Module getIr(CompilerContext.Module module) {
    return module.getIr();
  }

  @Override
  public CompilationStage getCompilationStage(CompilerContext.Module module) {
    return module.getCompilationStage();
  }

  @Override
  public TypeGraph getTypeHierarchy() {
    return Types.getTypeHierarchy();
  }

  @Override
  public void updateModule(CompilerContext.Module module, Consumer<Updater> callback) {
    try (var u = new ModuleUpdater((Module) module)) {
      callback.accept(u);
    }
  }

  public <T> Optional<T> loadCache(Cache<T, ?> cache) {
    return cache.load(context);
  }

  public <T> Optional<TruffleFile> saveCache(
      Cache<T, ?> cache, T entry, boolean useGlobalCacheLocations) {
    return cache.save(entry, context, useGlobalCacheLocations);
  }

  @Override
  public boolean typeContainsValues(String name) {
    var type = context.getBuiltins().getBuiltinType(name);
    return type != null && type.containsValues();
  }

  /** Lazy-initializes the IR for the builtins module. */
  @Override
  public void initializeBuiltinsIr(
      Compiler compiler, boolean irCachingEnabled, FreshNameSupply freshNameSupply, Passes passes) {
    var builtins = context.getBuiltins();
    var builtinsModule = builtins.getModule().asCompilerModule();
    if (!builtins.isIrInitialized()) {
      log(Level.FINE, "Initialising IR for [{0}].", builtinsModule.getName());

      builtins.initializeBuiltinsSource();

      if (irCachingEnabled) {
        if (deserializeModule(compiler, builtinsModule)) {
          // Ensure that builtins doesn't try and have codegen run on it.
          updateModule(builtinsModule, u -> u.compilationStage(CompilationStage.AFTER_CODEGEN));
        } else {
          builtins.initializeBuiltinsIr(this, freshNameSupply, passes);
        }
      } else {
        builtins.initializeBuiltinsIr(this, freshNameSupply, passes);
      }

      if (irCachingEnabled && !wasLoadedFromCache(builtinsModule)) {
        serializeModule(compiler, builtinsModule, true, true);
      }
    }
  }

  @Override
  public void runStubsGenerator(CompilerContext.Module module) {
    stubsGenerator.run(((Module) module).unsafeModule());
  }

  @Override
  public CompilerContext.Module findTopScopeModule(String name) {
    var option = context.getTopScope().getModule(name);
    return option.isEmpty() ? null : option.get().asCompilerModule();
  }

  @SuppressWarnings("unchecked")
  @Override
  public Future<Boolean> serializeLibrary(
      Compiler compiler, LibraryName libraryName, boolean useGlobalCacheLocations) {
    logSerializationManager(Level.INFO, "Requesting serialization for library [{0}].", libraryName);

    var task =
        (Callable)
            serializationManager.doSerializeLibrary(compiler, libraryName, useGlobalCacheLocations);

    return (Future<Boolean>)
        serializationManager
            .getPool()
            .submitTask(task, isCreateThreadAllowed(), toQualifiedName(libraryName));
  }

  /**
   * Requests that `module` be serialized.
   *
   * <p>This method will attempt to schedule the provided module and IR for serialization regardless
   * of whether or not it is appropriate to do so. If there are preconditions needed for
   * serialization, these should be checked before calling this method.
   *
   * <p>In addition, this method handles breaking links between modules contained in the IR to
   * ensure safe serialization.
   *
   * <p>It is responsible for taking a "snapshot" of the relevant module state at the point at which
   * serialization is requested. This is due to the fact that serialization happens in a separate
   * thread and the module may be mutated beneath it.
   *
   * @param module the module to serialize
   * @param useGlobalCacheLocations if true, will use global caches location, local one otherwise
   * @param useThreadPool if true, will perform serialization asynchronously
   * @return Future referencing the serialization task. On completion Future will return `true` if
   *     `module` has been successfully serialized, `false` otherwise
   */
  @SuppressWarnings("unchecked")
  @Override
  public Future<Boolean> serializeModule(
      Compiler compiler,
      CompilerContext.Module module,
      boolean useGlobalCacheLocations,
      boolean useThreadPool) {
    if (module.isSynthetic()) {
      throw new IllegalStateException(
          "Cannot serialize synthetic module [" + module.getName() + "]");
    }
    logSerializationManager(
        Level.FINE, "Requesting serialization for module [{0}].", module.getName());
    var ir = module.getIr();
    var dupl =
        ir.duplicate(
            ir.duplicate$default$1(), ir.duplicate$default$2(), ir.duplicate$default$3(), true);
    var duplicatedIr = compiler.updateMetadata(ir, dupl);
    Source src;
    try {
      src = module.getSource();
    } catch (IOException ex) {
      logSerializationManager(Level.WARNING, "Cannot get source for " + module.getName(), ex);
      return CompletableFuture.failedFuture(ex);
    }
    var task =
        doSerializeModule(
            ((Module) module).getCache(),
            duplicatedIr,
            module.getCompilationStage(),
            module.getName(),
            src,
            useGlobalCacheLocations);
    return serializationManager.getPool().submitTask(task, useThreadPool, module.getName());
  }

  /**
   * Create the task that serializes the provided module IR when it is run.
   *
   * @param cache the cache manager for the module being serialized
   * @param ir the IR for the module being serialized
   * @param stage the compilation stage of the module
   * @param name the name of the module being serialized
   * @param source the source of the module being serialized
   * @param useGlobalCacheLocations if true, will use global caches location, local one otherwise
   * @return the task that serialies the provided `ir`
   */
  private Callable<Boolean> doSerializeModule(
      Cache<ModuleCache.CachedModule, ModuleCache.Metadata> cache,
      org.enso.compiler.core.ir.Module ir,
      CompilationStage stage,
      QualifiedName name,
      Source source,
      boolean useGlobalCacheLocations) {
    return () -> {
      var pool = serializationManager.getPool();
      pool.waitWhileSerializing(name);

      logSerializationManager(Level.FINE, "Running serialization for module [{0}].", name);
      pool.startSerializing(name);
      try {
        var fixedStage =
            stage.isAtLeast(CompilationStage.AFTER_STATIC_PASSES)
                ? CompilationStage.AFTER_STATIC_PASSES
                : stage;
        var optionallySaved =
            saveCache(
                cache,
                new ModuleCache.CachedModule(ir, fixedStage, source),
                useGlobalCacheLocations);
        return optionallySaved.isPresent();
      } catch (Throwable e) {
        logSerializationManager(
            Level.SEVERE,
            "Serialization of module `" + name + "` failed: " + e.getMessage() + "`",
            e);
        throw e;
      } finally {
        pool.finishSerializing(name);
      }
    };
  }

  private final Map<LibraryName, MapToBindings> known = new HashMap<>();

  @Override
  public boolean deserializeModule(Compiler compiler, CompilerContext.Module module) {
    var level = Level.FINE;
    if (module.getPackage() != null) {
      var library = module.getPackage().libraryName();
      var bindings = known.get(library);
      if (bindings == null) {
        var cached = serializationManager.deserializeLibraryBindings(library);
        if (cached.isDefined()) {
          bindings = cached.get().bindings();
          known.put(library, bindings);
        }
      }
      if (bindings != null) {
        var ir = bindings.findForModule(module.getName());
        loggerSerializationManager.log(
            Level.FINE,
            "Deserializing module " + module.getName() + " from library: " + (ir != null));
        if (ir != null) {
          compiler
              .context()
              .updateModule(
                  module,
                  (u) -> {
                    u.ir(ir);
                    u.compilationStage(CompilationStage.AFTER_STATIC_PASSES);
                    u.loadedFromCache(true);
                  });
          return true;
        }
      }
      level = "Standard".equals(library.namespace()) ? Level.WARNING : Level.FINE;
    }
    var result = serializationManager.deserialize(compiler, module);
    loggerSerializationManager.log(
        level, "Deserializing module " + module.getName() + " from IR file: " + result.nonEmpty());
    return result.nonEmpty();
  }

  @Override
  public void shutdown(boolean waitForPendingJobCompletion) {
    serializationManager.shutdown(waitForPendingJobCompletion);
  }

  private final class ModuleUpdater implements Updater, AutoCloseable {
    private final Module module;
    private BindingsMap[] map;
    private org.enso.compiler.core.ir.Module[] ir;
    private CompilationStage stage;
    private Boolean loadedFromCache;
    private boolean resetScope;
    private boolean invalidateCache;

    private ModuleUpdater(Module module) {
      this.module = module;
    }

    @Override
    public void bindingsMap(BindingsMap map) {
      this.map = new BindingsMap[] {map};
    }

    @Override
    public void ir(org.enso.compiler.core.ir.Module ir) {
      this.ir = new org.enso.compiler.core.ir.Module[] {ir};
    }

    @Override
    public void compilationStage(CompilationStage stage) {
      this.stage = stage;
    }

    @Override
    public void loadedFromCache(boolean b) {
      this.loadedFromCache = b;
    }

    @Override
    public void resetScope() {
      this.resetScope = true;
    }

    @Override
    public void invalidateCache() {
      this.invalidateCache = true;
    }

    @Override
    public void close() {
      if (map != null) {
        if (module.bindings != null && map[0] != null) {
          loggerCompiler.log(Level.FINEST, "Reassigining bindings to {0}", module);
        }
        module.bindings = map[0];
      }
      if (ir != null) {
        module.module.unsafeSetIr(ir[0]);
      }
      if (stage != null) {
        module.module.unsafeSetCompilationStage(stage);
      }
      if (loadedFromCache != null) {
        module.module.setLoadedFromCache(loadedFromCache);
      }
      if (resetScope) {
        module.module.ensureScopeExists();
        module.module.getScope().reset();
      }
      if (invalidateCache) {
        module.module.getCache().invalidate(context);
      }
    }
  }

  public static final class Module extends CompilerContext.Module {
    private final org.enso.interpreter.runtime.Module module;
    private BindingsMap bindings;

    public Module(org.enso.interpreter.runtime.Module module) {
      this.module = module;
    }

    @Override
    public Source getSource() throws IOException {
      return module.getSource();
    }

    @Override
    public String getPath() {
      return module.getPath();
    }

    @Override
    public Package<TruffleFile> getPackage() {
      return module.getPackage();
    }

    /** Intentionally not public. */
    final org.enso.interpreter.runtime.Module unsafeModule() {
      return module;
    }

    @Override
    public QualifiedName getName() {
      return module.getName();
    }

    @Override
    public BindingsMap getBindingsMap() {
      if (module.getIr() != null) {
        try {
          var meta = module.getIr().passData();
          var pass = meta.get(BindingAnalysis$.MODULE$);
          emitIOException();
          return (BindingsMap) pass.get();
        } catch (IOException ex) {
          var logger =
              TruffleLogger.getLogger(LanguageInfo.ID, org.enso.interpreter.runtime.Module.class);
          var msg = "Cannot read BindingsMap for " + getName() + ": " + ex.getMessage();
          logger.log(Level.SEVERE, msg);
          logger.log(Level.FINE, msg, ex);
        }
      }
      return bindings;
    }

    @Override
    public TruffleFile getSourceFile() {
      return module.getSourceFile();
    }

    @Override
    public List<QualifiedName> getDirectModulesRefs() {
      return module.getDirectModulesRefs();
    }

    public Cache<ModuleCache.CachedModule, ModuleCache.Metadata> getCache() {
      return module.getCache();
    }

    @Override
    public CompilationStage getCompilationStage() {
      return module.getCompilationStage();
    }

    @Override
    public boolean isSynthetic() {
      return module.isSynthetic();
    }

    @Override
    public org.enso.compiler.core.ir.Module getIr() {
      return module.getIr();
    }

    @Override
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

    @Override
    public String toString() {
      StringBuilder sb = new StringBuilder();
      sb.append("CompilerContext.Module{");
      sb.append("module=").append(module);
      sb.append('}');
      return sb.toString();
    }
  }

  private static void emitIOException() throws IOException {}

  private static QualifiedName toQualifiedName(LibraryName libraryName) {
    var namespace = cons(libraryName.namespace(), nil());
    return new QualifiedName(namespace, libraryName.name());
  }
}
