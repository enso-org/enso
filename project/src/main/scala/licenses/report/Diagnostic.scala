package src.main.scala.licenses.report

sealed trait Diagnostic {
  def message: String
}

object Diagnostic {

  /** A warning indicating some unexpected event that should be noted
    * but does not stop the report from being accepted.
    */
  case class Warning(message: String) extends Diagnostic

  /** An error with the license review that has to be resolved
    * before the report can be accepted.
    */
  case class Error(message: String, metadata: Map[String, String] = Map.empty)
      extends Diagnostic

  def partition(diagnostics: Seq[Diagnostic]): (Seq[Warning], Seq[Error]) = {
    val warnings = diagnostics.collect { case n: Warning => n }
    val errors   = diagnostics.collect { case p: Error => p }
    (warnings, errors)
  }
}
