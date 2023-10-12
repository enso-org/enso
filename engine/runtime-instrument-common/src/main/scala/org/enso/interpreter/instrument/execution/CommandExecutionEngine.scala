package org.enso.interpreter.instrument.execution

import org.enso.interpreter.instrument.InterpreterContext
import org.enso.interpreter.instrument.command.{
  AsynchronousCommand,
  Command,
  SynchronousCommand
}
import org.enso.text.Sha3_224VersionCalculator

import java.util.concurrent.{
  BlockingQueue,
  CompletableFuture,
  LinkedBlockingQueue
}
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}

/** This component schedules the execution of commands. It keep a queue of
  * pending commands and activates command execution in FIFO order.
  *
  * @param interpreterContext suppliers of services that provide interpreter
  * specific functionality
  */
class CommandExecutionEngine(interpreterContext: InterpreterContext)
    extends CommandProcessor {

  private val isSequential =
    interpreterContext.executionService.getContext
      .isInterpreterSequentialCommandExection()

  private val locking = new ReentrantLocking(
    interpreterContext.executionService.getLogger
  )

  private val executionState = new ExecutionState()

  private val jobExecutionEngine =
    new JobExecutionEngine(interpreterContext, executionState, locking)

  private val commandExecutor =
    if (isSequential) {
      interpreterContext.executionService.getLogger.fine(
        "Executing commands sequentially"
      )
      jobExecutionEngine.jobExecutor
    } else {
      interpreterContext.executionService.getLogger.fine(
        "Executing commands in a separate command pool"
      )
      interpreterContext.executionService.getContext
        .newCachedThreadPool("command-pool", false)
    }

  private val sequentialExecutionService =
    interpreterContext.executionService.getContext.newFixedThreadPool(
      1,
      "sequential-command-pool",
      false
    )
  private val sequentialExecutionContext =
    ExecutionContext.fromExecutor(sequentialExecutionService)

  private val blockingQueue: BlockingQueue[SynchronousCommand] =
    new LinkedBlockingQueue[SynchronousCommand]()

  implicit private val commandExecutionContext: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(commandExecutor)

  private val runtimeContext =
    RuntimeContext(
      executionService  = interpreterContext.executionService,
      contextManager    = interpreterContext.contextManager,
      endpoint          = interpreterContext.endpoint,
      truffleContext    = interpreterContext.truffleContext,
      jobProcessor      = jobExecutionEngine,
      jobControlPlane   = jobExecutionEngine,
      locking           = locking,
      state             = executionState,
      versionCalculator = Sha3_224VersionCalculator
    )

  /** @inheritdoc */
  def invoke(cmd: Command): Future[Completion] = {
    cmd match {
      case c: SynchronousCommand =>
        import scala.jdk.FutureConverters._
        blockingQueue.add(c)
        val javaFuture: CompletableFuture[Completion] = {
          CompletableFuture.supplyAsync(
            () => {
              val orderedCommand = blockingQueue.take()
              orderedCommand.execute(runtimeContext, sequentialExecutionContext)
            },
            sequentialExecutionContext
          )
        }
        javaFuture.asScala
      case c: AsynchronousCommand =>
        c.execute(runtimeContext, commandExecutionContext)
    }
  }

  /** @inheritdoc */
  override def stop(): Unit = {
    jobExecutionEngine.stop()
    commandExecutor.shutdownNow()
  }

}
