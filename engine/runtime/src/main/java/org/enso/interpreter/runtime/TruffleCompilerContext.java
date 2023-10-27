package org.enso.interpreter.runtime;

import org.enso.compiler.pass.analyse.BindingAnalysis$;
import org.enso.compiler.codegen.IrToTruffle;
import org.enso.compiler.codegen.RuntimeStubsGenerator;
import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.context.FreshNameSupply;

import com.oracle.truffle.api.TruffleLogger;
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
import org.enso.interpreter.node.ExpressionNode;
import org.enso.interpreter.runtime.data.Type;
import org.enso.interpreter.runtime.scope.LocalScope;
import org.enso.interpreter.runtime.scope.ModuleScope;
import org.enso.pkg.Package;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.CompilationStage;

import scala.Option;

final class TruffleCompilerContext implements CompilerContext {

  private final EnsoContext context;
  private final TruffleLogger compiler;
  private final TruffleLogger serializationManager;
  private final RuntimeStubsGenerator stubsGenerator;

  TruffleCompilerContext(EnsoContext context) {
    this.context = context;
    this.compiler = context.getLogger(Compiler.class);
    this.serializationManager = context.getLogger(SerializationManager.class);
    this.stubsGenerator = new RuntimeStubsGenerator(context.getBuiltins());
  }

  @Override
  public boolean isIrCachingDisabled() {
    return context.isIrCachingDisabled();
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
  public void truffleRunCodegen(CompilerContext.Module module, CompilerConfig config) throws IOException {
    truffleRunCodegen(module.getSource(), module.getScope(), config, module.getIr());
  }

  @Override
  public void truffleRunCodegen(Source source, ModuleScope scope, CompilerConfig config, org.enso.compiler.core.ir.Module ir) {
    new IrToTruffle(context, source, scope, config).run(ir);
  }

  @Override
  public ExpressionNode truffleRunInline(Source source, LocalScope localScope, CompilerContext.Module module, CompilerConfig config, Expression ir) {
    return new IrToTruffle(context, source, module.getScope(), config)
            .runInline(ir, localScope, "<inline_source>");
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
    return ((Module)module).unsafeModule().isInteractive();
  }

  @Override
  public boolean wasLoadedFromCache(CompilerContext.Module module) {
    return ((Module)module).unsafeModule().wasLoadedFromCache();
  }

  @Override
  public boolean hasCrossModuleLinks(CompilerContext.Module module) {
    return ((Module)module).unsafeModule().hasCrossModuleLinks();
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
  public void updateModule(CompilerContext.Module module, Consumer<Updater> callback) {
    try (var u = new ModuleUpdater((Module)module)) {
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

  @Override
  public boolean typeContainsValues(String name) {
    var type = context.getBuiltins().getBuiltinType(name);
    return type != null && type.containsValues();
  }

  /**
   * Lazy-initializes the IR for the builtins module.
   */
  @Override
  public void initializeBuiltinsIr(
          boolean irCachingEnabled, SerializationManager serializationManager,
          FreshNameSupply freshNameSupply, Passes passes
  ) {
    var builtins = context.getBuiltins();
    var builtinsModule = builtins.getModule().asCompilerModule();
    if (!builtins.isIrInitialized()) {
      log(
              Level.FINE,
              "Initialising IR for [{0}].",
              builtinsModule.getName()
      );

      builtins.initializeBuiltinsSource();

      if (irCachingEnabled) {
        if (
          serializationManager.deserialize(builtinsModule) instanceof Option<?> op &&
          op.isDefined() &&
          op.get() instanceof Boolean b && b
        ) {
          // Ensure that builtins doesn't try and have codegen run on it.
          updateModule(
            builtinsModule,
            u -> u.compilationStage(CompilationStage.AFTER_CODEGEN)
          );
        } else {
          builtins.initializeBuiltinsIr(this, freshNameSupply, passes);
          updateModule(
            builtinsModule,
            u -> u.hasCrossModuleLinks(true)
          );
        }
      } else {
        builtins.initializeBuiltinsIr(this, freshNameSupply, passes);
        updateModule(
                builtinsModule,
                u -> u.hasCrossModuleLinks(true)
        );
      }

      if (irCachingEnabled && !wasLoadedFromCache(builtinsModule)) {
        serializationManager.serializeModule(
                builtinsModule, true, true
        );
      }
    }
  }

  @Override
  public void runStubsGenerator(CompilerContext.Module module) {
    stubsGenerator.run(((Module)module).unsafeModule());
  }

  @Override
  public CompilerContext.Module findTopScopeModule(String name) {
    var option = context.getTopScope().getModule(name);
    return option.isEmpty() ? null : option.get().asCompilerModule();
  }


  private final class ModuleUpdater implements Updater, AutoCloseable {
    private final Module module;
    private BindingsMap map;
    private org.enso.compiler.core.ir.Module ir;
    private CompilationStage stage;
    private Boolean loadedFromCache;
    private Boolean hasCrossModuleLinks;
    private boolean resetScope;
    private boolean invalidateCache;

    private ModuleUpdater(Module module) {
      this.module = module;
    }

    @Override
    public void bindingsMap(BindingsMap map) {
      this.map = map;
    }

    @Override
    public void ir(org.enso.compiler.core.ir.Module ir) {
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
      if (map != null) {
        if (module.bindings != null) {
          throw new IllegalStateException("Reassigining bindings to " + module);
        }
        module.bindings = map;
      }
      if (ir != null) {
        module.module.unsafeSetIr(ir);
      }
      if (stage != null) {
        module.module.unsafeSetCompilationStage(stage);
      }
      if (loadedFromCache != null) {
        module.module.setLoadedFromCache(loadedFromCache);
      }
      if (hasCrossModuleLinks != null) {
        module.module.setHasCrossModuleLinks(hasCrossModuleLinks);
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
    public boolean isSameAs(org.enso.interpreter.runtime.Module m) {
      return module == m;
    }

    // XXX
    public org.enso.interpreter.runtime.scope.ModuleScope getScope() {
      return module.getScope();
    }

    @Override
    public QualifiedName getName() {
      return module.getName();
    }

    @Override
    public Type findType(String name) {
      return module.getScope().getTypes().get(name);
    }

    @Override
    public BindingsMap getBindingsMap() {
      if (module.getIr() != null) {
        var meta = module.getIr().passData();
        var pass = meta.get(BindingAnalysis$.MODULE$);
        return (BindingsMap) pass.get();
      } else {
        return bindings;
      }
    }

    @Override
    public TruffleFile getSourceFile() {
      return module.getSourceFile();
    }

    @Override
    public List<QualifiedName> getDirectModulesRefs() {
      return module.getDirectModulesRefs();
    }

    @Override
    public ModuleCache getCache() {
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
    public boolean hasCrossModuleLinks() {
      return module.hasCrossModuleLinks();
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
}
