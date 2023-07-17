package org.enso.interpreter.runtime;

import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.source.Source;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.logging.Level;
import org.enso.compiler.Cache;
import org.enso.compiler.Compiler;
import org.enso.compiler.PackageRepository;
import org.enso.compiler.SerializationManager;
import org.enso.compiler.codegen.IrToTruffle;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.context.InlineContext;
import org.enso.compiler.core.IR;
import org.enso.compiler.data.CompilerConfig;
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.scope.LocalScope;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.interpreter.runtime.scope.TopLevelScope;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.CompilationStage;
import org.enso.polyglot.RuntimeOptions;

final class TruffleCompilerContext implements CompilerContext {
  private final EnsoContext context;
  private final TruffleLogger compiler;
  private final TruffleLogger serializationManager;

  TruffleCompilerContext(EnsoContext context) {
    this.context = context;
    this.compiler = context.getLogger(Compiler.class);
    this.serializationManager = context.getLogger(SerializationManager.class);
  }

  @Override
  public boolean isIrCachingDisabled() {
    return context.isIrCachingDisabled();
  }

  @Override
  public boolean isUseGlobalCacheLocations() {
    return context
        .getEnvironment()
        .getOptions()
        .get(RuntimeOptions.USE_GLOBAL_IR_CACHE_LOCATION_KEY);
  }

  @Override
  public boolean isInteractiveMode() {
    return context.getEnvironment().getOptions().get(RuntimeOptions.INTERACTIVE_MODE_KEY);
  }

  @Override
  public PackageRepository getPackageRepository() {
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
    compiler.log(level, msg, args);
  }

  @Override
  public void logSerializationManager(Level level, String msg, Object... args) {
    serializationManager.log(level, msg, args);
  }

  @Override
  public void notifySerializeModule(QualifiedName moduleName) {
    context.getNotificationHandler().serializeModule(moduleName);
  }

  @Override
  public TopLevelScope getTopScope() {
    return context.getTopScope();
  }

  @Override
  public boolean isCreateThreadAllowed() {
    return context.getEnvironment().isCreateThreadAllowed();
  }

  @Override
  public Thread createThread(Runnable r) {
    return context.getEnvironment().createThread(r);
  }

  @Override
  public Thread createSystemThread(Runnable r) {
    return context.getEnvironment().createSystemThread(r);
  }

  @Override
  public void truffleRunCodegen(Module module, CompilerConfig config) throws IOException {
    truffleRunCodegen(module.getSource(), module.getScope(), config, module.getIr());
  }

  @Override
  public void truffleRunCodegen(
      Source source, ModuleScope scope, CompilerConfig config, IR.Module ir) {
    new IrToTruffle(context, source, scope, config).run(ir);
  }

  @Override
  public ExpressionNode truffleRunInline(
      Source source, InlineContext inlineContext, CompilerConfig config, IR.Expression ir) {
    var localScope =
        inlineContext.localScope().isDefined()
            ? inlineContext.localScope().get()
            : LocalScope.root();
    return new IrToTruffle(context, source, inlineContext.module().getScope(), config)
        .runInline(ir, localScope, "<inline_source>");
  }

  // module related
  @Override
  public QualifiedName getModuleName(Module module) {
    return module.getName();
  }

  @Override
  public CharSequence getCharacters(Module module) throws IOException {
    return module.getSource().getCharacters();
  }

  @Override
  public boolean isSynthetic(Module module) {
    return module.isSynthetic();
  }

  @Override
  public boolean isInteractive(Module module) {
    return module.isInteractive();
  }

  @Override
  public boolean wasLoadedFromCache(Module module) {
    return module.wasLoadedFromCache();
  }

  @Override
  public boolean hasCrossModuleLinks(Module module) {
    return module.hasCrossModuleLinks();
  }

  @Override
  public IR.Module getIr(Module module) {
    return module.getIr();
  }

  @Override
  public CompilationStage getCompilationStage(Module module) {
    return module.getCompilationStage();
  }

  @Override
  public void updateModule(Module module, Consumer<Updater> callback) {
    try (var u = new ModuleUpdater(module)) {
      callback.accept(u);
    }
  }

  @Override
  public <T> Optional<T> loadCache(Cache<T, ?> cache) {
    return cache.load(context);
  }

  @Override
  public <T> Optional<TruffleFile> saveCache(
      Cache<T, ?> cache, T entry, boolean useGlobalCacheLocations) {
    return cache.save(entry, context, useGlobalCacheLocations);
  }

  private final class ModuleUpdater implements Updater, AutoCloseable {
    private final Module module;
    private IR.Module ir;
    private CompilationStage stage;
    private Boolean loadedFromCache;
    private Boolean hasCrossModuleLinks;
    private boolean resetScope;
    private boolean invalidateCache;

    private ModuleUpdater(org.enso.interpreter.runtime.Module module) {
      this.module = module;
    }

    @Override
    public void ir(IR.Module ir) {
      this.ir = ir;
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
    public void hasCrossModuleLinks(boolean b) {
      this.hasCrossModuleLinks = b;
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
      if (ir != null) module.unsafeSetIr(ir);
      if (stage != null) module.unsafeSetCompilationStage(stage);
      if (loadedFromCache != null) module.setLoadedFromCache(loadedFromCache);
      if (hasCrossModuleLinks != null) module.setHasCrossModuleLinks(hasCrossModuleLinks);
      if (resetScope) {
        module.ensureScopeExists();
        module.getScope().reset();
      }
      if (invalidateCache) {
        module.getCache().invalidate(context);
      }
    }
  }
}
