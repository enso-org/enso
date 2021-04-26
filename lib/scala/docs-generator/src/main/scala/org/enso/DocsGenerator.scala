package org.enso

import java.io.File

import org.enso.syntax.text.{DocParserHTMLGenerator, DocParserRunner, Parser}

object DocsGenerator {
  def generate(program: String): String = {
    val parser   = new Parser()
    val module   = parser.run(program)
    val dropMeta = parser.dropMacroMeta(module)
    val doc      = DocParserRunner.createDocs(dropMeta)
    val code     = DocParserHTMLGenerator.generateHTMLForEveryDocumented(doc)
    code
  }

  def traverse(root: File, skipHidden: Boolean = false): LazyList[File] =
    if (!root.exists || (skipHidden && root.isHidden)) LazyList.empty
    else
      LazyList.apply(root) ++ (root.listFiles match {
        case null  => LazyList.empty
        case files => files.view.flatMap(traverse(_, skipHidden))
      })
}

object DocsGeneratorMain extends App {
  import DocsGenerator._
//  println("Enso Docs Generator")
  val path = "./distribution/std-lib/Standard/src"
  val allFiles = traverse(new File(path))
    .filter(f => f.isFile && f.getName.endsWith(".enso"))
//  allFiles.foreach(println)
  val allPrograms =
    allFiles.map(f =>
      scala.io.Source.fromFile(f, "UTF-8").getLines().mkString("\n")
    )
//  allPrograms.foreach(println)
  val allDocs = allPrograms.map(generate)
//  allDocs.foreach(println)
  val zipped = allFiles.zip(allDocs)
//  save R to L.replace("src","docs").replace(".enso",".html")
}
