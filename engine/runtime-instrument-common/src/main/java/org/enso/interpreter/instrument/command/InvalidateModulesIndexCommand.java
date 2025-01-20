package org.enso.interpreter.instrument.command;

import com.oracle.truffle.api.TruffleLogger;
import java.util.UUID;
import java.util.logging.Level;
import org.enso.interpreter.instrument.execution.RuntimeContext;
import org.enso.interpreter.instrument.job.DeserializeLibrarySuggestionsJob;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.polyglot.runtime.Runtime$Api$InvalidateModulesIndexResponse;
import scala.Option;
import scala.concurrent.ExecutionContext;
import scala.concurrent.Future;
import scala.runtime.BoxedUnit;

/** A command that invalidates the modules index. */
public final class InvalidateModulesIndexCommand extends AsynchronousCommand {

  /**
   * Create a command that invalidates the modules index.
   *
   * @param maybeRequestId an option with request id
   */
  public InvalidateModulesIndexCommand(Option<UUID> maybeRequestId) {
    super(maybeRequestId);
  }

  @Override
  @SuppressWarnings("unchecked")
  public Future<BoxedUnit> executeAsynchronously(RuntimeContext ctx, ExecutionContext ec) {
    return Future.apply(
        () -> {
          TruffleLogger logger = ctx.executionService().getLogger();
          try {
            logger.log(Level.FINE, "Invalidating modules, cancelling background jobs");
            ctx.jobControlPlane().stopBackgroundJobs();
            ctx.jobControlPlane()
                .abortBackgroundJobs(
                    "invalidate modules index", DeserializeLibrarySuggestionsJob.class);

            EnsoContext context = ctx.executionService().getContext();
            context
                .getTopScope()
                .getModules()
                .forEach(module -> ctx.state().suggestions().markIndexAsDirty(module));

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
