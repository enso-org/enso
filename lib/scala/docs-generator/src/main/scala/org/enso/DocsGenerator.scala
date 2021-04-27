package org.enso

import java.io._

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
  val allDocs = allPrograms.map(generate)
  val allDocFiles = allFiles.map(x =>
    x.getPath.replace(".enso", ".html").replace("Standard/src", "docs")
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
}
