package org.enso.launcher.cli.impl

class Flag(name: String, short: Option[Char], helpComment: String)
    extends BaseOpts[Boolean] {
  override private[cli] val flags = {
    val shortParser = for {
      opt <- short
    } yield opt.toString -> (update _)

    Map(name -> (update _)) ++ shortParser
  }

  val empty                                = Right(false)
  var value: Either[List[String], Boolean] = empty

  override private[cli] def reset(): Unit = {
    value = empty
  }

  private def update(): Unit = {
    value = value.flatMap(
      if (_) Left(List(s"Flag $name is set more than once"))
      else Right(true)
    )
  }

  override private[cli] def result() = value

  override def helpExplanations(): Seq[String] = {
    val orShort = short.map(c => s" or -$c").getOrElse("")
    Seq(s"--$name$orShort\t$helpComment")
  }
}
