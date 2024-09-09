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
import java.util.logging.Logger;
import org.enso.common.CompilationStage;
import org.enso.common.LanguageInfo;
import org.enso.compiler.Compiler;
import org.enso.compiler.Passes;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.context.FreshNameSupply;
import org.enso.compiler.core.ir.Diagnostic;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.CompilerConfig;
import org.enso.compiler.data.IdMap;
import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.compiler.suggestions.ExportsBuilder;
import org.enso.compiler.suggestions.ExportsMap;
import org.enso.compiler.suggestions.SuggestionBuilder;
import org.enso.editions.LibraryName;
import org.enso.interpreter.CompilationAbortedException;
import org.enso.interpreter.caches.Cache;
import org.enso.interpreter.caches.ImportExportCache;
import org.enso.interpreter.caches.ImportExportCache.MapToBindings;
import org.enso.interpreter.caches.ModuleCache;
import org.enso.interpreter.caches.SuggestionsCache;
import org.enso.interpreter.runtime.type.Types;
import org.enso.interpreter.runtime.util.DiagnosticFormatter;
import org.enso.pkg.Package;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.Suggestion;
import org.enso.polyglot.data.TypeGraph;
import scala.collection.immutable.ListSet;
import scala.collection.immutable.SetOps;

final class TruffleCompilerContext implements CompilerContext {
  private static final Logger LOG = Logger.getLogger(TruffleCompilerContext.class.getName());

  private final EnsoContext context;
  private final TruffleLogger loggerCompiler;
  private final TruffleLogger loggerSerializationManager;
  private final RuntimeStubsGenerator stubsGenerator;
  private final SerializationPool serializationPool;

  TruffleCompilerContext(EnsoContext context) {
    this.context = context;
    this.loggerCompiler = context.getLogger(Compiler.class);
    this.loggerSerializationManager = context.getLogger(SerializationPool.class);
    this.serializationPool = new SerializationPool(this);
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
  public DefaultPackageRepository getPackageRepository() {
    return context.getPackageRepository();
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
  public void truffleRunCodegen(
      CompilerContext.Module module,
      CompilerContext.ModuleScopeBuilder scopeBuilder,
      CompilerConfig config)
      throws IOException {
    var m = org.enso.interpreter.runtime.Module.fromCompilerModule(module);
    var s =
        org.enso.interpreter.runtime.scope.ModuleScope.Builder.fromCompilerModuleScopeBuilder(
            scopeBuilder);
    new IrToTruffle(context, m.getSource(), s, config).run(module.getIr());
  }

  // module related
  @Override
  public QualifiedName getModuleName(CompilerContext.Module module) {
    return module.getName();
  }

  @Override
  public CharSequence getCharacters(CompilerContext.Module module) throws IOException {
    return module.getCharacters();
  }

  @Override
  public IdMap getIdMap(CompilerContext.Module module) {
    return module.getIdMap();
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
  public boolean isModuleInRootPackage(CompilerContext.Module module) {
    var file = ((Module) module).getSourceFile();
    var pkg = getPackageOf(getPackageRepository(), file);
    return pkg.isPresent() && pkg.get() == getPackageRepository().getMainProjectPackage().get();
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

  final TypeGraph getTypeHierarchy() {
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

  final <T> TruffleFile saveCache(Cache<T, ?> cache, T entry, boolean useGlobalCacheLocations)
      throws IOException {
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
  public void runStubsGenerator(
      CompilerContext.Module module, CompilerContext.ModuleScopeBuilder scopeBuilder) {
    var m = ((Module) module).unsafeModule();
    var s =
        ((org.enso.interpreter.runtime.scope.TruffleCompilerModuleScopeBuilder) scopeBuilder)
            .unsafeScopeBuilder();
    stubsGenerator.run(m.getIr(), s);
  }

  @Override
  public CompilerContext.Module findTopScopeModule(String name) {
    var option = context.getTopScope().getModule(name);
    return option.isEmpty() ? null : option.get().asCompilerModule();
  }

  /**
   * Return true if the given location is inside module. More specifically, if the location's bounds
   * point inside the character bounds of the module.
   *
   * <p>Note that it is possible that a {@link Diagnostic}'s location has a bigger size than the
   * size of the module.
   */
  private static boolean isLocationInsideModule(
      org.enso.interpreter.runtime.Module module, IdentifiedLocation location) {
    try {
      return location.end() <= module.getSource().getLength();
    } catch (IOException e) {
      throw new AssertionError("Unreachable", e);
    }
  }

  @Override
  public CompilationAbortedException formatDiagnostic(
      CompilerContext.Module module, Diagnostic diagnostic, boolean isOutputRedirected) {
    DiagnosticFormatter diagnosticFormatter;
    var m = org.enso.interpreter.runtime.Module.fromCompilerModule(module);
    if (module != null && diagnostic.location().isDefined()) {
      var location = diagnostic.location().get();
      if (isLocationInsideModule(m, location)) {
        Source source;
        try {
          source = m.getSource();
        } catch (IOException e) {
          throw new AssertionError(e);
        }
        assert source != null;
        diagnosticFormatter = new DiagnosticFormatter(diagnostic, source, isOutputRedirected);
        return new CompilationAbortedException(
            diagnosticFormatter.format(), diagnosticFormatter.where());
      }
    }
    var emptySource = Source.newBuilder(LanguageInfo.ID, "", null).build();
    diagnosticFormatter = new DiagnosticFormatter(diagnostic, emptySource, isOutputRedirected);
    return new CompilationAbortedException(diagnosticFormatter.format(), null);
  }

  @SuppressWarnings("unchecked")
  @Override
  public Future<Boolean> serializeLibrary(
      Compiler compiler, LibraryName libraryName, boolean useGlobalCacheLocations) {
    logSerializationManager(Level.INFO, "Requesting serialization for library [{0}].", libraryName);

    var task = doSerializeLibrary(compiler, libraryName, useGlobalCacheLocations);

    return serializationPool.submitTask(
        task, isCreateThreadAllowed(), toQualifiedName(libraryName));
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
      var m = org.enso.interpreter.runtime.Module.fromCompilerModule(module);
      src = m.getSource();
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
    return serializationPool.submitTask(task, useThreadPool, module.getName());
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
      var pool = serializationPool;
      pool.waitWhileSerializing(name);

      logSerializationManager(Level.FINE, "Running serialization for module [{0}].", name);
      pool.startSerializing(name);
      try {
        var fixedStage =
            stage.isAtLeast(CompilationStage.AFTER_STATIC_PASSES)
                ? CompilationStage.AFTER_STATIC_PASSES
                : stage;
        var saved =
            saveCache(
                cache,
                new ModuleCache.CachedModule(ir, fixedStage, source),
                useGlobalCacheLocations);
        return saved != null;
      } catch (Throwable e) {
        logSerializationManager(
            e instanceof IOException ? Level.FINE : Level.SEVERE,
            "Serialization of module `" + name + "` failed: " + e.getMessage(),
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
    if (module.getPackage() != null) {
      var library = module.getPackage().libraryName();
      var bindings = known.get(library);
      if (bindings == null) {
        try {
          var cached = deserializeLibraryBindings(library);
          if (cached.isDefined()) {
            bindings = cached.get().bindings();
            known.put(library, bindings);
          }
        } catch (InterruptedException ex) {
          // proceed
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
    }
    try {
      var result = deserializeModuleDirect(module);
      loggerSerializationManager.log(
          Level.FINE, "Deserializing module " + module.getName() + " from IR file: " + result);
      return result;
    } catch (InterruptedException e) {
      loggerSerializationManager.log(
          Level.WARNING, "Deserializing module " + module.getName() + " from IR file", e);
      return false;
    }
  }

  /**
   * Deserializes the requested module from the cache if possible.
   *
   * <p>If the requested module is currently being serialized it will wait for completion before
   * loading. If the module is queued for serialization it will evict it and not load from the cache
   * (this is usually indicative of a programming bug).
   *
   * @param module the module to deserialize from the cache.
   * @return {@code true} if the deserialization succeeded
   */
  private boolean deserializeModuleDirect(CompilerContext.Module module)
      throws InterruptedException {
    var pool = serializationPool;
    if (pool.isWaitingForSerialization(module.getName())) {
      pool.abort(module.getName());
      return false;
    } else {
      pool.waitWhileSerializing(module.getName());

      var loaded = loadCache(((Module) module).getCache());
      if (loaded.isPresent()) {
        updateModule(
            module,
            (u) -> {
              u.ir(loaded.get().moduleIR());
              u.compilationStage(loaded.get().compilationStage());
              u.loadedFromCache(true);
            });
        logSerializationManager(
            Level.FINE,
            "Restored IR from cache for module [{0}] at stage [{1}].",
            module.getName(),
            loaded.get().compilationStage());
        return true;
      } else {
        logSerializationManager(
            Level.FINE, "Unable to load a cache for module [{0}].", module.getName());
        return false;
      }
    }
  }

  @SuppressWarnings("unchecked")
  Callable<Boolean> doSerializeLibrary(
      Compiler compiler, LibraryName libraryName, boolean useGlobalCacheLocations) {
    return () -> {
      var pool = serializationPool;
      pool.waitWhileSerializing(toQualifiedName(libraryName));

      logSerializationManager(Level.FINE, "Running serialization for bindings [{0}].", libraryName);
      pool.startSerializing(toQualifiedName(libraryName));
      var map = new HashMap<QualifiedName, org.enso.compiler.core.ir.Module>();
      var it = context.getPackageRepository().getModulesForLibrary(libraryName);
      while (it.nonEmpty()) {
        var module = it.head();
        map.put(module.getName(), module.getIr());
        it =
            (scala.collection.immutable.List<org.enso.compiler.context.CompilerContext.Module>)
                it.tail();
      }
      var snd =
          context
              .getPackageRepository()
              .getPackageForLibraryJava(libraryName)
              .map(x -> x.listSourcesJava());

      var bindingsCache =
          new ImportExportCache.CachedBindings(
              libraryName, new ImportExportCache.MapToBindings(map), snd);
      try {
        boolean result =
            doSerializeLibrarySuggestions(compiler, libraryName, useGlobalCacheLocations);
        try {
          var cache = ImportExportCache.create(libraryName);
          var file = saveCache(cache, bindingsCache, useGlobalCacheLocations);
          result &= file != null;
        } catch (Throwable e) {
          logSerializationManager(
              e instanceof IOException ? Level.WARNING : Level.SEVERE,
              "Serialization of bindings `" + libraryName + "` failed: " + e.getMessage() + "`",
              e);
          throw e;
        }
        return result;
      } finally {
        pool.finishSerializing(toQualifiedName(libraryName));
      }
    };
  }

  private boolean doSerializeLibrarySuggestions(
      Compiler compiler, LibraryName libraryName, boolean useGlobalCacheLocations)
      throws IOException {
    var exportsBuilder = new ExportsBuilder();
    var exportsMap = new ExportsMap();
    var suggestions = new java.util.ArrayList<Suggestion>();

    try {
      var libraryModules = context.getPackageRepository().getModulesForLibrary(libraryName);
      libraryModules
          .flatMap(
              module -> {
                var sug =
                    SuggestionBuilder.apply(module, getTypeHierarchy(), compiler)
                        .build(module.getName(), module.getIr())
                        .toVector()
                        .filter(Suggestion::isGlobal);
                var exports = exportsBuilder.build(module.getName(), module.getIr());
                exportsMap.addAll(module.getName(), exports);
                return sug;
              })
          .map(
              suggestion -> {
                scala.collection.immutable.Set<String> identity = new ListSet<>();
                var reexports =
                    exportsMap.get(suggestion).stream()
                        .map(QualifiedName::toString)
                        .reduce(identity, SetOps::incl, SetOps::union);
                return suggestion.withReexports(reexports);
              })
          .foreach(suggestions::add);

      var cachedSuggestions = new SuggestionsCache.CachedSuggestions(libraryName, suggestions);
      var cache = SuggestionsCache.create(libraryName);
      var file = saveCache(cache, cachedSuggestions, useGlobalCacheLocations);
      return file != null;
    } catch (Throwable e) {
      logSerializationManager(
          e instanceof IOException ? Level.WARNING : Level.SEVERE,
          "Serialization of suggestions `" + libraryName + "` failed: " + e.getMessage() + "`",
          e);
      throw e;
    }
  }

  @Override
  public scala.Option<Object> deserializeSuggestions(LibraryName libraryName)
      throws InterruptedException {
    var option = deserializeSuggestionsImpl(libraryName);
    return option.map(s -> s.suggestions());
  }

  private scala.Option<SuggestionsCache.CachedSuggestions> deserializeSuggestionsImpl(
      LibraryName libraryName) throws InterruptedException {
    var pool = serializationPool;
    if (pool.isWaitingForSerialization(toQualifiedName(libraryName))) {
      pool.abort(toQualifiedName(libraryName));
      return scala.Option.empty();
    } else {
      pool.waitWhileSerializing(toQualifiedName(libraryName));
      var cache = SuggestionsCache.create(libraryName);
      var loaded = loadCache(cache);
      if (loaded.isPresent()) {
        logSerializationManager(Level.FINE, "Restored suggestions for library [{0}].", libraryName);
        return scala.Option.apply(loaded.get());
      } else {
        logSerializationManager(
            Level.WARNING, "Unable to load suggestions for library [{0}].", libraryName);
        return scala.Option.empty();
      }
    }
  }

  scala.Option<ImportExportCache.CachedBindings> deserializeLibraryBindings(LibraryName libraryName)
      throws InterruptedException {
    var pool = serializationPool;
    if (pool.isWaitingForSerialization(toQualifiedName(libraryName))) {
      pool.abort(toQualifiedName(libraryName));
      return scala.Option.empty();
    } else {
      pool.waitWhileSerializing(toQualifiedName(libraryName));
      var cache = ImportExportCache.create(libraryName);
      var loaded = loadCache(cache);
      if (loaded.isPresent()) {
        logSerializationManager(Level.FINE, "Restored bindings for library [{0}].", libraryName);
        return scala.Option.apply(loaded.get());
      } else {
        logSerializationManager(
            Level.FINEST, "Unable to load bindings for library [{0}].", libraryName);
        return scala.Option.empty();
      }
    }
  }

  @Override
  public void shutdown(boolean waitForPendingJobCompletion) {
    try {
      serializationPool.shutdown(waitForPendingJobCompletion);
    } catch (InterruptedException ex) {
      logSerializationManager(Level.WARNING, ex.getMessage(), ex);
    }
  }

  @Override
  public RuntimeException throwAbortedException() {
    throw new CompilationAbortedException();
  }

  private final class ModuleUpdater implements Updater, AutoCloseable {
    private final Module module;
    private BindingsMap[] map;
    private IdMap idMap;
    private org.enso.compiler.core.ir.Module[] ir;
    private CompilationStage stage;
    private Boolean loadedFromCache;
    private boolean resetScope;
    private boolean resetTypes;
    private boolean invalidateCache;

    private ModuleUpdater(Module module) {
      this.module = module;
    }

    @Override
    public void bindingsMap(BindingsMap map) {
      this.map = new BindingsMap[] {map};
    }

    @Override
    public void idMap(IdMap idMap) {
      this.idMap = idMap;
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
      this.resetTypes = false;
    }

    @Override
    public void resetScope(boolean resetTypes) {
      this.resetScope = true;
      this.resetTypes = true;
    }

    @Override
    public void invalidateCache() {
      this.invalidateCache = true;
    }

    @Override
    public void close() {
      if (map != null) {
        if (module.bindings != null && map[0] != null) {
          loggerCompiler.log(Level.FINEST, "Reassigning bindings to {0}", module);
        }
        module.bindings = map[0];
      }
      if (ir != null) {
        module.module.unsafeSetIr(ir[0]);
      }
      if (idMap != null) {
        module.module.unsafeSetIdMap(idMap);
      }
      if (stage != null) {
        module.module.unsafeSetCompilationStage(stage);
      }
      if (loadedFromCache != null) {
        module.module.setLoadedFromCache(loadedFromCache);
      }
      if (resetScope) {
        var inheritTypes = !resetTypes;
        module.module.newScopeBuilder(inheritTypes);
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
    public CharSequence getCharacters() throws IOException {
      return module.getSource().getCharacters();
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
    org.enso.interpreter.runtime.Module unsafeModule() {
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
    public IdMap getIdMap() {
      return module.getIdMap();
    }

    TruffleFile getSourceFile() {
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
    public CompilerContext.ModuleScopeBuilder getScopeBuilder() {
      return new org.enso.interpreter.runtime.scope.TruffleCompilerModuleScopeBuilder(
          module.getScopeBuilder());
    }

    @Override
    public ModuleScopeBuilder newScopeBuilder() {
      return new org.enso.interpreter.runtime.scope.TruffleCompilerModuleScopeBuilder(
          module.newScopeBuilder(false));
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

  /**
   * Finds the package the provided module belongs to.
   *
   * @param packageRepository repository to work on
   * @param file the module to find the package of
   * @return {@code module}'s package, if exists
   */
  static Optional<Package<TruffleFile>> getPackageOf(
      DefaultPackageRepository packageRepository, TruffleFile file) {
    try {
      if (file != null) {
        file = file.getCanonicalFile();
        for (var pkg : packageRepository.getLoadedPackagesJava()) {
          if (file.startsWith(pkg.root().getCanonicalFile())) {
            return Optional.of(pkg);
          }
        }
      }
    } catch (IOException e) {
      LOG.log(Level.WARNING, null, e);
    }
    return Optional.empty();
  }
}
