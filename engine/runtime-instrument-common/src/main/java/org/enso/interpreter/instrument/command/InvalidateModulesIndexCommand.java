package org.enso.interpreter.instrument.command;

import java.util.UUID;
import org.enso.interpreter.instrument.execution.RuntimeContext;
import org.enso.interpreter.instrument.job.AnalyzeModuleJob;
import org.enso.interpreter.instrument.job.DeserializeLibrarySuggestionsJob;
import org.enso.interpreter.instrument.job.EnsureCompiledJob;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.polyglot.runtime.Runtime$Api$InvalidateModulesIndexResponse;
import org.slf4j.LoggerFactory;
import scala.Option;
import scala.concurrent.ExecutionContext;
import scala.concurrent.Future;
import scala.runtime.BoxedUnit;

/** A command that invalidates the modules index. */
public final class InvalidateModulesIndexCommand extends AsynchronousCommand {

  private final Option<UUID> maybeRequestId;

  /**
   * Create a command that invalidates the modules index.
   *
   * @param maybeRequestId an option with request id
   */
  public InvalidateModulesIndexCommand(Option<UUID> maybeRequestId) {
    super(maybeRequestId);
    this.maybeRequestId = maybeRequestId;
  }

  @Override
  @SuppressWarnings("unchecked")
  public Future<BoxedUnit> executeAsynchronously(RuntimeContext ctx, ExecutionContext ec) {
    return Future.apply(
        () -> {
          var logger = LoggerFactory.getLogger(InvalidateModulesIndexCommand.class);
          try {
            logger.debug("Invalidating modules, cancelling background jobs");
            ctx.jobControlPlane().stopBackgroundJobs();
            ctx.jobControlPlane()
                .abortBackgroundJobs(
                    "invalidate modules index",
                    DeserializeLibrarySuggestionsJob.class,
                    AnalyzeModuleJob.class);

            EnsoContext context = ctx.executionService().getContext();
            context
                .getTopScope()
                .getModules()
                .forEach(module -> ctx.state().suggestions().markIndexAsDirty(module));

            maybeRequestId.foreach(
                uuid -> {
                  var stack = ctx.contextManager().getStack(uuid);
                  ctx.jobProcessor().run(EnsureCompiledJob.apply(stack, ctx));
                  return BoxedUnit.UNIT;
                });

            context
                .getPackageRepository()
                .getLoadedPackages()
                .foreach(
                    pkg -> {
                      ctx.jobProcessor()
                          .runBackground(new DeserializeLibrarySuggestionsJob(pkg.libraryName()));
                      return BoxedUnit.UNIT;
                    });
          } finally {
            reply(new Runtime$Api$InvalidateModulesIndexResponse(), ctx);
          }
          return BoxedUnit.UNIT;
        },
        ec);
  }
}
