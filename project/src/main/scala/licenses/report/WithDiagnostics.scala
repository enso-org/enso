package src.main.scala.licenses.report

/** A simple monad for storing diagnostics related to a result.
  */
case class WithDiagnostics[+A](value: A, diagnostics: Seq[Diagnostic] = Seq()) {

  /** Returns a result with a mapped value and the same diagnostics.
    */
  def map[B](f: A => B): WithDiagnostics[B] =
    WithDiagnostics(f(value), diagnostics)

  /** Combines two computations returning diagnostics, preserving diagnostics from
    * both of them.
    */
  def flatMap[B](f: A => WithDiagnostics[B]): WithDiagnostics[B] = {
    val result = f(value)
    WithDiagnostics(result.value, diagnostics ++ result.diagnostics)
  }
}

object WithDiagnostics {
  implicit class SeqWithDiagnosticsSyntax[+A](seq: Seq[WithDiagnostics[A]]) {

    /** Turns a sequence of [[WithDiagnostics]] instances into a single
      * [[WithDiagnostics]] that contains the sequence of values and combined
      * diagnostics.
      */
    def flip: WithDiagnostics[Seq[A]] =
      WithDiagnostics(seq.map(_.value), seq.flatMap(_.diagnostics))
  }

  implicit class SeqLikeSyntax[A](seq: WithDiagnostics[Seq[A]]) {

    /** A shorthand syntax to concatenate two sequences wrapped with diagnostics,
      * combining their diagnostics.
      */
    def ++(other: WithDiagnostics[Seq[A]]): WithDiagnostics[Seq[A]] =
      for {
        lhs <- seq
        rhs <- other
      } yield lhs ++ rhs

    /** A shorthand syntax to concatenate an ordinary sequence to a sequence
      * with diagnostics.
      */
    def ++(other: Seq[A]): WithDiagnostics[Seq[A]] =
      for {
        lhs <- seq
      } yield lhs ++ other
  }

  /** Creates a [[WithDiagnostics]] containing Unit and a provided sequence of
    * diagnostics.
    */
  def justDiagnostics(diagnostics: Seq[Diagnostic]): WithDiagnostics[Unit] =
    WithDiagnostics((), diagnostics)
}
