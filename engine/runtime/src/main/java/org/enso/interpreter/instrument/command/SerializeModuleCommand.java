package org.enso.interpreter.instrument.command;

import org.enso.interpreter.instrument.execution.RuntimeContext;
import org.enso.interpreter.instrument.job.SerializeModuleJob;
import org.enso.pkg.QualifiedName;
import scala.Option;
import scala.concurrent.ExecutionContext;
import scala.concurrent.Future;
import scala.runtime.BoxedUnit;

import java.util.UUID;

/** The command to start the module serialization. */
public final class SerializeModuleCommand extends Command {

  private final QualifiedName moduleName;

  public SerializeModuleCommand(Option<UUID> maybeRequestId, QualifiedName moduleName) {
    super(maybeRequestId);
    this.moduleName = moduleName;
  }

  @Override
  public Future<BoxedUnit> execute(RuntimeContext ctx, ExecutionContext ec) {
    ctx.jobProcessor().runBackground(new SerializeModuleJob(moduleName));
    return Future.successful(BoxedUnit.UNIT);
  }
}
