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
  public Future<BoxedUnit> executeAsynchronously(RuntimeContext ctx, ExecutionContext ec) {
    return Future.apply(
        () -> {
          TruffleLogger logger = ctx.executionService().getLogger();
          long writeCompilationLockTimestamp = ctx.locking().acquireWriteCompilationLock();
          try {
            ctx.jobControlPlane().abortAllJobs();

            EnsoContext context = ctx.executionService().getContext();
            context.getTopScope().getModules().forEach(module -> module.setIndexed(false));
            ctx.jobControlPlane().stopBackgroundJobs();

            context
                .getPackageRepository()
                .getLoadedPackages()
                .foreach(
                    pkg -> {
                      ctx.jobProcessor()
                          .runBackground(new DeserializeLibrarySuggestionsJob(pkg.libraryName()));
                      return BoxedUnit.UNIT;
                    });

            reply(new Runtime$Api$InvalidateModulesIndexResponse(), ctx);
          } finally {
            ctx.locking().releaseWriteCompilationLock();
            logger.log(
                Level.FINEST,
                "Kept write compilation lock [{0}] for {1} milliseconds.",
                new Object[] {
                  this.getClass().getSimpleName(),
                  System.currentTimeMillis() - writeCompilationLockTimestamp
                });
          }

          return BoxedUnit.UNIT;
        },
        ec);
  }
}
