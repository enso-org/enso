package org.enso.loggingservice.internal

sealed class Level(val level: Int)
object Level {
  case object None    extends Level(-1)
  case object Error   extends Level(0)
  case object Warning extends Level(1)
  case object Info    extends Level(2)
  case object Debug   extends Level(3)
  case object Trace   extends Level(4)

  implicit val ord: Ordering[Level] = (x, y) => x.level - y.level
}
