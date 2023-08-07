package org.enso.interpreter.instrument.execution

import org.enso.interpreter.instrument.InterpreterContext
import org.enso.interpreter.instrument.command.Command
import org.enso.polyglot.RuntimeOptions
import org.enso.text.Sha3_224VersionCalculator

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

/** This component schedules the execution of commands. It keep a queue of
  * pending commands and activates command execution in FIFO order.
  *
  * @param interpreterContext suppliers of services that provide interpreter
  * specific functionality
  */
class CommandExecutionEngine(interpreterContext: InterpreterContext)
    extends CommandProcessor {

  private val isSequential =
    interpreterContext.executionService.getContext.getEnvironment.getOptions
      .get(RuntimeOptions.INTERPRETER_SEQUENTIAL_COMMAND_EXECUTION_KEY)
      .booleanValue()

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
  def invoke(cmd: Command): cmd.Result[Completion] = {
    cmd.execute(runtimeContext, commandExecutionContext)
  }

  /** @inheritdoc */
  override def stop(): Unit = {
    jobExecutionEngine.stop()
    commandExecutor.shutdownNow()
  }

}
