package org.enso.interpreter.instrument.job

import org.enso.compiler.context.Changeset
import org.enso.interpreter.instrument.execution.RuntimeContext
import org.enso.interpreter.instrument.execution.model.PendingEdit
import org.enso.interpreter.runtime.Module
import org.enso.text.buffer.Rope

import java.io.File

final class ApplyPendingEditsJob extends Job[Unit](List.empty, true, false) {

  import ApplyPendingEditsJob._

  override def run(implicit ctx: RuntimeContext): Unit = {
    ctx.locking.acquirePendingEditsLock()
    try {
      val pendingEdits = ctx.state.pendingEdits.dequeueAll()
      val resolvedFiles = pendingEdits. { case (file )}
      ensureCompiled(pendingEdits.keys)
      val changesets = pendingEdits.map { case (file, edits) =>
        file -> applyEdits(file, edits)
      }
      changesets.foreach(Function.tupled(invalidateCaches))
      changesets.foreach(Function.tupled(ensureCompiled))
    } finally {
      ctx.locking.releasePendingEditsLock()
    }
  }

}
object ApplyPendingEditsJob {

  type Result[+A] = Either[Throwable, A]

  sealed trait State
  case object State {
    case class Error(throwable: Throwable) extends State
    case class PendingEdits(pendingEdits: Map[File, Seq[PendingEdit]])
        extends State
    case class FilesResolved(pendingEdits: Map[Module, Seq[PendingEdit]])
        extends State
  }

  sealed trait Transition {
    def run(state: State): State
  }
  case object Transition {
    case object ResolveFiles extends Transition {
      override def run(state: State): State = ???
    }
  }

  private def run(init: State)(cmds: Seq[Transition]): Unit =
    cmds.foldLeft(Right(init)) {
      case (Right(state), transition) => transition.run(state) }

  case class ApplyEditsResult(
    changeset: Changeset[Rope],
    shouldRecompile: Boolean
  )

  private def resolveFile(file: File, edits: Seq[PendingEdit])(implicit
    ctx: RuntimeContext
  ): Result[State.FilesResolved] = ???

  private def applyEdits(file: File, edits: Seq[PendingEdit])(implicit
    ctx: RuntimeContext
  ): ApplyEditsResult =
    ???

  private def invalidateCaches(file: File, result: ApplyEditsResult)(implicit
    ctx: RuntimeContext
  ): Unit = ???

  private def ensureCompiled(file: File, result: ApplyEditsResult)(implicit
    ctx: RuntimeContext
  ): Unit = ???

  private def ensureCompiled(files: Iterable[File])(implicit
    ctx: RuntimeContext
  ): Unit = ???
}
