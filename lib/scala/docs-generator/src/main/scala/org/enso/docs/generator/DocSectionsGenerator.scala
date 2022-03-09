package org.enso.docs.generator

import org.enso.syntax.text.ast.Doc
import org.enso.syntax.text.ast.Doc.HTML
import scalatags.Text.all._
import scalatags.Text.{all => HTML}

final class DocSectionsGenerator {

  import DocSectionsGenerator._

  def generate(doc: Doc): DocSections = {
    println("=======")
    println(doc)
    DocSections(
      tags     = doc.tags.map(tags => tags.elems.toList.map(TagRender.render)),
      synopsis = doc.synopsis.map(SynopsisRender.render)
    )
  }
}
object DocSectionsGenerator {

  trait Render[A] {
    def render(o: A): String
  }
  object Render {
    def apply[A: Render]: Render[A] = implicitly[Render[A]]
  }

  implicit object HtmlRender extends Render[Doc.HTML] {
    final val DivOpenLength  = 5
    final val DivCloseLength = 6

    override def render(o: HTML): String =
      HTML
        .div(o)
        .toString
        .drop(DivOpenLength)
        .dropRight(DivCloseLength)
  }

  implicit object TagRender extends Render[Doc.Tags.Tag] {
    override def render(o: Doc.Tags.Tag): String =
      o.repr.build().strip
  }

  implicit object SynopsisRender extends Render[Doc.Synopsis] {
    override def render(o: Doc.Synopsis): String = {
      val builder = o.elems.foldLeft(Seq.newBuilder[Modifier]) { (b, a) =>
        b ++= a.html
      }
      HtmlRender.render(builder.result())
    }
  }
}

case class DocSections(tags: Option[Seq[String]], synopsis: Option[String])
