package org.enso.docs.generator

import org.enso.syntax.text.ast.Doc
import org.enso.syntax.text.ast.Doc.HTML
import scalatags.Text.all._
import scalatags.Text.{all => HTML}

final class DocSectionsGenerator {

  import DocSectionsGenerator._

  def generate(doc: Doc): DocSections = {
    println(doc.body)
    DocSections(
      tags        = doc.tags.map(tags => tags.elems.toList.map(TagRender.render)),
      description = doc.synopsis.map(SynopsisRender.render),
      arguments   = doc.body.flatMap(buildArguments),
      examples    = None,
      icon        = None,
      aliases     = None
    )
  }

  private def buildArguments(body: Doc.Body): Option[DocArguments] = {
    val argumentsSection = body.elems.toList.find { section =>
      section.elems.head.repr.build().contains("Arguments")
    }
    val argumentsOpt = argumentsSection.map { section =>
      section.elems.tail
        .flatMap {
          case e: Doc.Elem.List => e.elems.toList
          case _                => Seq()
        }
        .flatMap(buildArgument)
    }
    argumentsOpt.map(DocArguments)
  }

  private def buildArgument(elem: Doc.Elem): Option[DocArgument] = {
    val line         = HtmlRender.render(elem.html)
    val (name, text) = line.span(_ != ':')
    Option.when(text.nonEmpty) {
      DocArgument(name.trim, text.tail.trim)
    }
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

case class DocSections(
  tags: Option[Seq[String]],
  description: Option[String],
  arguments: Option[DocArguments],
  examples: Option[DocExamples],
  icon: Option[String],
  aliases: Option[DocAliases]
)

case class DocArguments(arguments: Seq[DocArgument])

case class DocArgument(name: String, text: String)

case class DocExamples(examples: Seq[DocExample])

case class DocExample(text: String, code: String)

case class DocAliases(aliases: Seq[String])
