package org.enso.interpreter.instrument.job;

import org.enso.compiler.context.CompilerContext;
import org.enso.compiler.suggestions.ExportsBuilder;
import org.enso.compiler.suggestions.ModuleExportsDiff;
import org.enso.compiler.suggestions.SuggestionBuilder;
import org.enso.compiler.suggestions.SuggestionDiff;
import org.enso.editions.LibraryName;
import org.enso.interpreter.instrument.execution.RuntimeContext;
import org.enso.interpreter.runtime.Module;
import org.enso.polyglot.ModuleExports;
import org.enso.polyglot.data.Tree;
import org.enso.polyglot.runtime.Runtime$Api$Response$;
import org.enso.polyglot.runtime.Runtime$Api$SuggestionsDatabaseAction;
import org.enso.polyglot.runtime.Runtime$Api$SuggestionsDatabaseAction$Clean;
import org.enso.polyglot.runtime.Runtime$Api$SuggestionsDatabaseModuleUpdateNotification;
import org.enso.scala.wrapper.ScalaConversions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import scala.collection.immutable.Set$;

/**
 * A job that generates suggestions for all modules of a loaded library when no pre-generated
 * suggestion cache is available.
 */
public final class AnalyzeLibraryModuleJob extends BackgroundJob<Void>
    implements UniqueJob<Void>, SkipSchedulingUniqueJob {

  private static final Logger logger = LoggerFactory.getLogger(AnalyzeLibraryModuleJob.class);
  private static final int PRIORITY = 100;
  private static final ExportsBuilder exportsBuilder = new ExportsBuilder();

  private final LibraryName libraryName;

  public AnalyzeLibraryModuleJob(LibraryName libraryName) {
    super(PRIORITY);
    this.libraryName = libraryName;
  }

  public LibraryName libraryName() {
    return libraryName;
  }

  @Override
  public boolean equalsTo(UniqueJob<?> that) {
    if (that instanceof AnalyzeLibraryModuleJob other) {
      return this.libraryName.equals(other.libraryName);
    }
    return false;
  }

  @Override
  public Void runImpl(RuntimeContext ctx) {
    logger.debug("Generating suggestions for library [{}].", libraryName);
    analyzeLibrary(libraryName, ctx);
    return null;
  }

  @Override
  public String toString() {
    return "AnalyzeLibraryModuleJob(" + libraryName + ")";
  }

  @SuppressWarnings("unchecked")
  static void analyzeLibrary(LibraryName libraryName, RuntimeContext ctx) {
    if (!ctx.executionService().getContext().isGlobalSuggestionsEnabled()) {
      return;
    }
    var packageRepository = ctx.executionService().getContext().getCompiler().packageRepository();
    var it = packageRepository.getModulesForLibrary(libraryName);

    while (it.nonEmpty()) {
      var compilerModule = it.head();
      var module = Module.fromCompilerModule(compilerModule);
      var ir = module.getIr();
      if (ir != null) {
        analyzeModule(module, ctx);
      } else {
        logger.debug(
            "Skipping module [{}] without IR in library [{}].", module.getName(), libraryName);
      }
      it = (scala.collection.immutable.List<CompilerContext.Module>) it.tail();
    }
  }

  @SuppressWarnings("unchecked")
  private static void analyzeModule(Module module, RuntimeContext ctx) {
    var moduleName = module.getName();
    var compiler = ctx.executionService().getContext().getCompiler();
    var state = ctx.state().suggestions().getOrCreateFresh(module, module.getIr());

    if (!state.isIndexed()) {
      logger.debug("Analyzing library module [{}].", moduleName);
      var types = Module.findTypeHierarchy(compiler.context());
      var newSuggestions =
          SuggestionBuilder.apply(module.asCompilerModule(), types, compiler)
              .build(moduleName, state.ir());
      var prevExports =
          new ModuleExports(
              moduleName.toString(), (scala.collection.immutable.Set) Set$.MODULE$.empty());
      var newExports = exportsBuilder.build(moduleName, state.ir());
      Runtime$Api$SuggestionsDatabaseAction cleanAction =
          new Runtime$Api$SuggestionsDatabaseAction$Clean(moduleName.toString());
      var actions = ScalaConversions.seq(java.util.List.of(cleanAction));
      var notification =
          new Runtime$Api$SuggestionsDatabaseModuleUpdateNotification(
              moduleName.toString(),
              actions.toVector(),
              ModuleExportsDiff.compute(prevExports, newExports),
              SuggestionDiff.compute(Tree.empty(), newSuggestions));
      if (ctx.state().suggestions().markAsIndexed(module, state)) {
        sendModuleUpdate(notification, ctx);
      } else {
        logger.debug(
            "Calculated index for library module [{}] is not up-to-date. Discarding.", moduleName);
      }
    } else {
      logger.debug("Library module [{}] is already indexed. Skipping.", moduleName);
    }
  }

  private static void sendModuleUpdate(
      Runtime$Api$SuggestionsDatabaseModuleUpdateNotification payload, RuntimeContext ctx) {
    if (payload.actions().nonEmpty()
        || payload.exports().nonEmpty()
        || !payload.updates().isEmpty()) {
      ctx.endpoint().sendToClient(Runtime$Api$Response$.MODULE$.apply(payload));
    }
  }
}
