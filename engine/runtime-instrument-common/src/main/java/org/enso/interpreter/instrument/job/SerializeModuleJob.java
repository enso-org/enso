package org.enso.interpreter.instrument.job;

import org.enso.common.CompilationStage;
import org.enso.interpreter.instrument.execution.RuntimeContext;
import org.enso.pkg.QualifiedName;
import org.slf4j.LoggerFactory;

/** The job that serializes module. */
public final class SerializeModuleJob extends BackgroundJob<Void> {

  private final QualifiedName moduleName;

  private static final int SERIALIZE_MODULE_JOB_PRIORITY = 1000;

  public SerializeModuleJob(QualifiedName moduleName) {
    super(SERIALIZE_MODULE_JOB_PRIORITY);
    this.moduleName = moduleName;
  }

  @Override
  public Void runImpl(RuntimeContext ctx) {
    var ensoContext = ctx.executionService().getContext();
    var compiler = ensoContext.getCompiler();
    boolean useGlobalCacheLocations = ensoContext.isUseGlobalCache();
    ctx.locking()
        .withWriteCompilationLock(
            this.getClass(),
            () -> {
              ctx.executionService()
                  .getContext()
                  .findModule(moduleName.toString())
                  .ifPresent(
                      module -> {
                        if (module.getCompilationStage().isBefore(CompilationStage.AFTER_CODEGEN)) {
                          LoggerFactory.getLogger(SerializeModuleJob.class)
                              .warn(
                                  "Attempt to serialize the module [{}] at stage [{}].",
                                  module.getName(),
                                  module.getCompilationStage());
                          return;
                        }
                        compiler
                            .context()
                            .serializeModule(
                                compiler,
                                module.asCompilerModule(),
                                useGlobalCacheLocations,
                                false);
                      });
              return null;
            });
    return null;
  }

  @Override
  public String toString() {
    return "SerializeModuleJob(" + moduleName.toString() + ")";
  }
}
