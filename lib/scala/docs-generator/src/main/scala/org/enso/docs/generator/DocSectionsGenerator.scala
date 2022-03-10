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
      tags        = doc.tags.map(buildTags),
      description = doc.synopsis.map(SynopsisRender.render),
      arguments   = doc.body.flatMap(buildArguments),
      examples    = doc.body.flatMap(buildExamples),
      icon        = doc.body.flatMap(buildIcon),
      aliases     = doc.body.flatMap(buildAliases)
    )
  }

  private def buildTags(tags: Doc.Tags): DocTags = {
    val docTags = tags.elems.toList.map { tag =>
      DocTag(tag.name, tag.details.map(_.trim))
    }
    DocTags(docTags)
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

  private def buildExamples(body: Doc.Body): Option[DocExamples] = {
    val examplesSections = body.elems.toList.collect {
      case m @ Doc.Section.Marked(_, _, Doc.Section.Marked.Example, _) =>
        m
    }
    val docExamples = examplesSections.map { section =>
      val text = section.elems
        .collect { case Doc.Elem.Text(text) =>
          text
        }
        .mkString(" ")
      val code = section.elems.collect { case block: Doc.Elem.CodeBlock =>
        normalizeCodeBlock(block).repr.build()
      }.mkString
      DocExample(text, code)
    }
    Option.unless(docExamples.isEmpty)(DocExamples(docExamples))
  }

  private def normalizeCodeBlock(
    block: Doc.Elem.CodeBlock
  ): Doc.Elem.CodeBlock = {
    val minIndent = block.elems.map(_.indent).toList.min
    val normalized =
      block.elems.map(elem => elem.copy(indent = elem.indent - minIndent))
    block.copy(elems = normalized)
  }

  private def buildIcon(body: Doc.Body): Option[String] = {
    val iconSection = body.elems.toList.find { section =>
      section.elems.head.repr.build().startsWith("Icon:")
    }
    iconSection.map { section =>
      HtmlRender.render(section.html).stripPrefix("Icon:").trim
    }
  }

  private def buildAliases(body: Doc.Body): Option[DocAliases] = {
    val aliasesSection = body.elems.toList.find { section =>
      section.elems.head.repr.build().startsWith("Aliases:")
    }
    aliasesSection
      .map { section =>
        val list = HtmlRender
          .render(section.html)
          .stripPrefix("Aliases:")
          .trim
          .split(", ")
        DocAliases(list.toSeq)
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
    final val POpenTag       = "<p>"
    final val PCloseTag      = "</p>"

    override def render(o: HTML): String =
      HTML
        .div(o)
        .toString
        .drop(DivOpenLength)
        .dropRight(DivCloseLength)
        .stripPrefix(POpenTag)
        .stripSuffix(PCloseTag)
        .trim
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
  tags: Option[DocTags],
  description: Option[String],
  arguments: Option[DocArguments],
  examples: Option[DocExamples],
  icon: Option[String],
  aliases: Option[DocAliases]
)

case class DocTags(tags: Seq[DocTag])

case class DocTag(name: String, description: Option[String])

case class DocArguments(arguments: Seq[DocArgument])

case class DocArgument(name: String, text: String)

case class DocExamples(examples: Seq[DocExample])

case class DocExample(text: String, code: String)

case class DocAliases(aliases: Seq[String])
