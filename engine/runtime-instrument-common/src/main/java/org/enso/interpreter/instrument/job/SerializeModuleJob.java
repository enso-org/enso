package org.enso.interpreter.instrument.job;

import org.enso.compiler.SerializationManager;
import org.enso.interpreter.instrument.execution.RuntimeContext;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.Module;
import org.enso.pkg.QualifiedName;
import org.enso.polyglot.RuntimeOptions;

import java.util.logging.Level;

/** The job that serializes module. */
public final class SerializeModuleJob extends BackgroundJob<Void> {

  private final QualifiedName moduleName;

  private static final int SERIALIZE_MODULE_JOB_PRIORITY = 1000;

  public SerializeModuleJob(QualifiedName moduleName) {
    super(SERIALIZE_MODULE_JOB_PRIORITY);
    this.moduleName = moduleName;
  }

  @Override
  public Void run(RuntimeContext ctx) {
    EnsoContext ensoContext = ctx.executionService().getContext();
    SerializationManager serializationManager = ensoContext.getCompiler().getSerializationManager();
    boolean useGlobalCacheLocations =
        ensoContext
            .getEnvironment()
            .getOptions()
            .get(RuntimeOptions.USE_GLOBAL_IR_CACHE_LOCATION_KEY);
    ctx.locking().acquireWriteCompilationLock();
    try {
      ctx.executionService()
          .getContext()
          .findModule(moduleName.toString())
          .ifPresent(
              module -> {
                if (module.getCompilationStage().isBefore(Module.CompilationStage.AFTER_CODEGEN)) {
                  ctx.executionService()
                      .getLogger()
                      .log(
                          Level.WARNING,
                          "Attempt to serialize the module [{}] at stage [{}].",
                          new Object[] {module.getName(), module.getCompilationStage()});
                  return;
                }

                serializationManager.serializeModule(module, useGlobalCacheLocations, false);
              });
    } finally {
      ctx.locking().releaseWriteCompilationLock();
    }
    return null;
  }
}
