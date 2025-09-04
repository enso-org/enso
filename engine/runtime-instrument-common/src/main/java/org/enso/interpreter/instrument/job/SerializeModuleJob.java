package org.enso.interpreter.instrument.job;

import org.enso.interpreter.instrument.execution.RuntimeContext;
import org.enso.pkg.QualifiedName;

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
    ctx.locking()
        .withWriteCompilationLock(
            this.getClass(),
            () -> {
              ctx.executionService()
                  .getContext()
                  .findModule(moduleName.toString())
                  .ifPresent(
                      module -> {
                        assert !module.needsCompilation()
                            : "Attempt to serialize the module that needs compilation: " + module;
                        compiler
                            .context()
                            .serializeModule(compiler, module.asCompilerModule(), false);
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
