package src.main.scala.licenses

import java.nio.file.Path

sealed trait Attachment
case class Notice(path: Path, content: String) extends Attachment
case class CopyrightMention(content: String, context: Option[String])
    extends Attachment
