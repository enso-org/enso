package src.main.scala.licenses

import java.nio.file.Path

import sbt.IO

/** Represents a licensing information related to a dependency.
  */
sealed trait Attachment

/** Represents a file attached to the dependency's sources.
  *
  * This may be a license file, a copyright notice, a credits file etc.
  */
case class AttachedFile(
  path: PortablePath,
  content: String,
  origin: Option[String] = None
) extends Attachment {

  /** @inheritdoc
    */
  override def toString: String = {
    val originRepr = origin.map(o => s", origin = $o")
    s"AttachedFile($path, ...$originRepr)"
  }

  /** Name of the file.
    */
  def fileName: String = path.getFileName
}

/** Represents a copyright mention extracted from a file.
  *
  * The copyright mention may come from comments in the source code, for
  * example.
  *
  * Equal copyright mentions are merged, so a single mention may contain
  * multiple contexts and origins if it was a result of merging from different
  * mentions.
  *
  * @param content cleaned content of the line that was extracted
  * @param contexts contexts (surrounding lines) that are associated with the mention
  * @param origins paths to the files that the mention comes from
  */
case class CopyrightMention(
  content: String,
  contexts: Seq[String],
  origins: Seq[PortablePath]
) extends Attachment {

  /** @inheritdoc
    */
  override def toString: String = s"CopyrightMention('$content')"
}

object CopyrightMention {

  /** Creates a [[CopyrightMention]] converting `origins` to [[PortablePath]].
    */
  def from(
    content: String,
    contexts: Seq[String],
    origins: Seq[Path]
  ): CopyrightMention =
    CopyrightMention(content, contexts, origins.map(PortablePath(_)))

  /** Creates a [[CopyrightMention]] with no origins.
    */
  def apply(
    content: String,
    contexts: Seq[String]
  ): CopyrightMention =
    CopyrightMention(content, contexts, Seq.empty[PortablePath])

  /** Transforms the sequence of copyright mentions by merging ones that have
    * equal content.
    */
  def mergeByContent(copyrights: Seq[CopyrightMention]): Seq[CopyrightMention] =
    copyrights
      .groupBy(c => c.content)
      .map({ case (_, equal) => mergeEqual(equal) })
      .toSeq

  /** Transforms the sequence of copyright mentions by merging ones that have
    * equal context.
    *
    * This may be useful as a single comment block may contain multiple mentions
    * of the word copyright, so multiple mentions coming from the same context
    * would be redundant.
    *
    * It only allows to merge mentions that have only one context, as a
    * heuristic to avoid too aggressive merging.
    */
  def mergeByContext(
    copyrights: Seq[CopyrightMention]
  ): Seq[CopyrightMention] = {
    val (eligible, rest) = copyrights.partition(_.contexts.size == 1)
    val merged = eligible.groupBy(c => c.contexts.head).map { case (_, equal) =>
      val ref = findBestRepresentative(equal)
      ref.copy(origins = equal.flatMap(_.origins).distinct)
    }
    (merged ++ rest).toSeq
  }

  /** Returns the best representative to use as content of the merged set of
    * copyrights that share a context.
    *
    * Usually, mentions that start with the word 'copyright' are most
    * interesting, if the word appears somewhere in the middle it is usually
    * just a continuation of a sentence. So we choose the instance starting with
    * the word 'copyright' if such instance exists. Otherwise an arbitrary
    * instance is selected.
    */
  private def findBestRepresentative(
    copyrights: Seq[CopyrightMention]
  ): CopyrightMention = {
    copyrights
      .find(_.content.stripLeading.toLowerCase.startsWith("copyright"))
      .getOrElse(copyrights.head)
  }

  /** Merges multiple copyright mentions that have equal content.
    *
    * Their contexts and origins are concatenated (ignoring duplicates).
    */
  def mergeEqual(copyrights: Seq[CopyrightMention]): CopyrightMention = {
    val ref = copyrights.headOption.getOrElse(
      throw new IllegalArgumentException("Copyrights must not be empty")
    )
    val ok = copyrights.forall(_.content == ref.content)
    if (!ok) {
      throw new IllegalArgumentException("Copyrights must be equal to merge")
    }
    CopyrightMention(
      ref.content,
      copyrights.flatMap(_.contexts).distinct,
      copyrights.flatMap(_.origins).distinct
    )
  }

  /** Strips comment-related characters from the prefix and whitespace from
    * suffix of the copyright line.
    */
  def cleanup(string: String): String = {
    val charsToIgnore = Seq('*', '-', '#', '/')
    string
      .dropWhile(char => char.isWhitespace || charsToIgnore.contains(char))
      .strip
  }
}

object AttachedFile {

  /** Reads a file at the given path into an [[AttachedFile]].
    *
    * If `relativizeTo` is provided, the path included in the resulting
    * [[AttachedFile]] is relative to the one provided.
    */
  def read(path: Path, relativizeTo: Option[Path]): AttachedFile = {
    val content = IO.read(path.toFile)
    val actualPath = relativizeTo match {
      case Some(root) => root.relativize(path)
      case None       => path
    }
    AttachedFile(PortablePath(actualPath), content)
  }
}
