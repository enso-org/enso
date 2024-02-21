package src.main.scala.licenses.report

sealed trait Diagnostic {
    def message: String
}

object Diagnostic {

  /** A notice indicating some unexpected event that should be noted but does not stop the report from being accepted. */
  case class Notice(message: String) extends Diagnostic

  /** A problem with the license review that has to be resolved before the report can be accepted. */
  case class Problem(message: String) extends Diagnostic

  def partition(diagnostics: Seq[Diagnostic]): (Seq[Notice], Seq[Problem]) = {
    val notices = diagnostics.collect { case n: Notice => n }
    val problems = diagnostics.collect { case p: Problem => p }
    (notices, problems)
  }
}
