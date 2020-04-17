import sbt.Keys._
import sbt._

import scala.sys.process._

object GenerateFlatbuffers {

  lazy val task = Def.task {
    val flatcCmd = "flatc"
    val root     = baseDirectory.value
    val schemas =
      (file(s"$root/src/main/schema") ** "*.fbs").get
        .map(_.getAbsolutePath)
        .toList

    val out = (sourceManaged in Compile).value

    schemas foreach { schema =>
      val cmd = s"$flatcCmd --java -o ${out.getAbsolutePath} $schema"
      println(s"*** Generating Java classes for schema: $schema, cmd: $cmd")
      val result = cmd.!!
      println(
        s"*** Generated Java classes from FlatBuffer schema $schema. Results: $result"
      )
    }
    (out ** "*.java").get
  }

}
