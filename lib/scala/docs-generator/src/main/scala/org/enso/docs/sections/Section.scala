package org.enso.docs.sections

sealed trait Section[A]
object Section {

  case class Tag[A](name: String, text: Option[A]) extends Section[A]
  case class Paragraph[A](body: List[A])           extends Section[A]
  case class Keyed[A](key: String, body: List[A])  extends Section[A]
  case class Marked[A](mark: Mark, header: Option[String], body: List[A])
      extends Section[A]

  sealed trait Mark
  object Mark {
    case object Important extends Mark
    case object Info      extends Mark
    case object Example   extends Mark
  }
}
