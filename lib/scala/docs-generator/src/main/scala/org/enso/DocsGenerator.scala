package org.enso

import java.io._
import org.enso.syntax.text.{DocParser, DocParserMain, Parser}

object DocsGenerator {
  def generate(program: String): String = {
    val parser   = new Parser()
    val module   = parser.run(program)
    val dropMeta = parser.dropMacroMeta(module)
    val doc      = DocParser.DocParserRunner.createDocs(dropMeta)
    val code =
      DocParser.DocParserHTMLGenerator.generateHTMLForEveryDocumented(doc)
    code
  }

  def generatePure(comment: String): String = {
    val doc  = DocParserMain.runMatched(comment)
    val html = DocParser.DocParserHTMLGenerator.generateHTMLPureDoc(doc)
    html
  }

  def mapIfEmpty(doc: String): String = {
    var tmp = doc
    if (doc.replace("<div>", "").replace("</div>", "").length == 0) {
      tmp =
        "\n\n*Enso Reference Viewer.*\n\nNo documentation available for chosen source file."
      tmp = generatePure(tmp)
    }
    tmp
  }

  def removeUnnecessaryDivs(doc: String): String = {
    var tmp = doc
    while (tmp.contains("<div></div>"))
      tmp = tmp.replace("<div></div>", "")
    tmp
  }

  def traverse(root: File): LazyList[File] =
    if (!root.exists) LazyList.empty
    else
      LazyList.apply(root) ++ (root.listFiles match {
        case null  => LazyList.empty
        case files => files.view.flatMap(traverse)
      })

  case class StringList(value: List[String])

  def flattenTailRec(ls: List[Any]): List[Any] = {
    def flattenR(res: List[Any], rem: List[Any]): List[Any] = rem match {
      case Nil                     => res
      case (head: List[_]) :: tail => flattenR(res, head ::: tail)
      case element :: tail         => flattenR(res ::: List(element), tail)
    }
    flattenR(List(), ls)
  }

  def groupByPrefix(
    strings: List[String]
  ): scala.collection.mutable.Map[String, Any] = {
    var stringsByPrefix = scala.collection.mutable.Map[String, Any]()
    for (string <- strings) {
      if (string.split('/').length <= 1) {
        stringsByPrefix = stringsByPrefix ++ Map(string -> List())
      } else {
        val arr = string.split('/')
        if (stringsByPrefix.contains(arr.head)) {
          stringsByPrefix(arr.head) =
            stringsByPrefix(arr.head) +: List(arr.tail.mkString("/"))
        } else {
          stringsByPrefix += (arr.head -> List(arr.tail.mkString("/")))
        }
      }
    }
    for (elem <- stringsByPrefix) {
      if (elem._2.isInstanceOf[List[Any]]) {
        stringsByPrefix(elem._1) =
          elem._2 :: flattenTailRec(elem._2.asInstanceOf[List[Any]])
      }
      elem._2 match {
        case StringList(xs) => stringsByPrefix(elem._1) = groupByPrefix(xs)
        case _              =>
      }
    }
    stringsByPrefix
  }
}

object DocsGeneratorMain extends App {
  import DocsGenerator._

  val path = "./distribution/std-lib/Standard/src"
  val allFiles = traverse(new File(path))
    .filter(f => f.isFile && f.getName.endsWith(".enso"))
  val allFileNames =
    allFiles.map(_.getPath.replace(path + "/", "").replace(".enso", ""))
  val treeNames = groupByPrefix(allFileNames.toList)
  print(treeNames)

  val allPrograms =
    allFiles.map(scala.io.Source.fromFile(_, "UTF-8").getLines().mkString("\n"))
  val allDocs =
    allPrograms.map(generate).map(mapIfEmpty).map(removeUnnecessaryDivs)
  val allDocFiles = allFiles.map(
    _.getPath.replace(".enso", ".html").replace("Standard/src", "docs")
  )
  val zipped = allDocFiles.zip(allDocs)
  zipped.foreach(x => {
    val file = new File(x._1)
    val dir  = new File(x._1.replaceAll("\\/[a-zA-Z_]*\\.[a-zA-Z]*", ""))
    dir.mkdirs()
    file.createNewFile();
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(x._2)
    bw.close()
  })

  val jsTemplate = new File(
    "./lib/scala/docs-generator/src/main/scala/org/enso/docsgenerator/template.js"
  )
  val templateCode =
    scala.io.Source.fromFile(jsTemplate, "UTF-8").getLines().mkString("\n")
  val allDocJSFiles = allFiles.map(
    _.getPath.replace(".enso", ".js").replace("Standard/src", "docs-js")
  )
  val zippedJS = allDocJSFiles.zip(allDocs)
  zippedJS.foreach(x => {
    val file = new File(x._1)
    val dir  = new File(x._1.replaceAll("\\/[a-zA-Z_]*\\.[a-zA-Z]*", ""))
    dir.mkdirs()
    file.createNewFile();
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(
      templateCode.replace(
        "{/*PAGE*/}",
        x._2
          .replace("display: flex", "display: none")
          .replace("{", "&#123;")
          .replace("}", "&#125;")
      )
    )
    bw.close()
  })
}
