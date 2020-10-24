package src.main.scala.licenses.report

/** A simple monad for storing warnings related to a result.
  */
case class WithWarnings[+A](value: A, warnings: Seq[String] = Seq()) {

  /** Returns a result with a mapped value and the same warnings.
    */
  def map[B](f: A => B): WithWarnings[B] = WithWarnings(f(value), warnings)

  /** Combines two computations returning warnings, preserving warnings from
    * both of them.
    */
  def flatMap[B](f: A => WithWarnings[B]): WithWarnings[B] = {
    val result = f(value)
    WithWarnings(result.value, warnings ++ result.warnings)
  }
}

object WithWarnings {
  implicit class SeqWithWarningsSyntax[+A](seq: Seq[WithWarnings[A]]) {

    /** Turns a sequence of [[WithWarnings]] instances into a single
      * [[WithWarnings]] that contains the sequence of values and combined
      * warnings.
      */
    def flip: WithWarnings[Seq[A]] =
      WithWarnings(seq.map(_.value), seq.flatMap(_.warnings))
  }

  implicit class SeqLikeSyntax[A](seq: WithWarnings[Seq[A]]) {

    /** A shorthand syntax to concatenate two sequences wrapped with warnings,
      * combining their warnings.
      */
    def ++(other: WithWarnings[Seq[A]]): WithWarnings[Seq[A]] =
      for {
        lhs <- seq
        rhs <- other
      } yield lhs ++ rhs

    /** A shorthand syntax to concatenate an ordinary sequence to a sequence
      * with warnings.
      */
    def ++(other: Seq[A]): WithWarnings[Seq[A]] =
      for {
        lhs <- seq
      } yield lhs ++ other
  }

  /** Creates a [[WithWarnings]] containing Unit and a provided sequence of
    * warnings.
    */
  def justWarnings(warnings: Seq[String]): WithWarnings[Unit] =
    WithWarnings((), warnings)
}
