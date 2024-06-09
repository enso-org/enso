package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterContext, InterpreterTest}

class EnsoProjectTest extends InterpreterTest {

  override def subject: String = "Enso_Project.enso_project"

  override def specify(implicit
    interpreterContext: InterpreterContext
  ): Unit = {
    "enso_project should be in micro-distribution" in {
      val code =
        """
          |from Standard.Base.Meta import enso_project
          |
          |main = enso_project.name
          |""".stripMargin
      eval(
        code
      ).toString shouldEqual "(Error: Module_Not_In_Package_Error.Error)"
    }

    "enso_project for Standard.Base" in {
      val code =
        """
          |import Standard.Base
          |from Standard.Base.Meta import Project_Description
          |
          |main = (Project_Description.new Standard.Base).name
          |""".stripMargin
      eval(code) shouldEqual "Base"
    }
  }
}
