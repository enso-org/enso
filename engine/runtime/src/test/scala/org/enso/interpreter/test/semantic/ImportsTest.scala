package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterException, PackageTest}

class ImportsTest extends PackageTest {
  "Atoms and methods" should "be available for import" in {
    evalTestProject("TestSimpleImports") shouldEqual 20
  }

  "Methods defined together with atoms" should "be visible even if not imported" in {
    evalTestProject("TestNonImportedOwnMethods") shouldEqual 10
  }

  "Overloaded methods" should "not be visible when not imported" in {
    the[InterpreterException] thrownBy evalTestProject(
      "TestNonImportedOverloads"
    ) should have message "Method `method` of Mk_X could not be found."
  }

  "Import statements" should "report errors when they cannot be resolved" in {
    the[InterpreterException] thrownBy evalTestProject(
      "Test_Bad_Imports"
    ) should have message "Compilation aborted due to errors."
    val outLines = consumeOut
    outLines(2) should include(
      "Package containing the module Surely_This.Does_Not_Exist.My_Module " +
      "could not be loaded: The package could not be resolved: The library " +
      "`Surely_This.Does_Not_Exist` is not defined within the edition."
    )
    outLines(3) should include(
      "The module Enso_Test.Test_Bad_Imports.Oopsie does not exist."
    )
  }

  "Symbols from imported modules" should "not be visible when imported qualified" in {
    the[InterpreterException] thrownBy evalTestProject(
      "Test_Qualified_Error"
    ) should have message "Compilation aborted due to errors."
    consumeOut
      .filterNot(_.contains("Compiler encountered"))
      .filterNot(_.contains("In module"))
      .head should include("The name `Mk_X` could not be found.")
  }

  "Symbols from imported modules" should "not be visible when hidden" in {
    the[InterpreterException] thrownBy evalTestProject(
      "Test_Hiding_Error"
    ) should have message "Compilation aborted due to errors."
    consumeOut
      .filterNot(_.contains("Compiler encountered"))
      .filterNot(_.contains("In module"))
      .head should include("The name `X` could not be found.")
  }

  "Symbols from imported modules" should "be visible even when others are hidden" in {
    evalTestProject("Test_Hiding_Success") shouldEqual 20
  }

  "Imported modules" should "be renamed with renaming imports" in {
    evalTestProject("Test_Rename") shouldEqual 20
  }

  "Imported modules" should "not be visible under original name when renamed" in {
    the[InterpreterException] thrownBy evalTestProject(
      "Test_Rename_Error"
    ) should have message "Compilation aborted due to errors."
    consumeOut
      .filterNot(_.contains("Compiler encountered"))
      .filterNot(_.contains("In module"))
      .head should include("The name `Atom` could not be found.")

  }

  "Exports system" should "detect cycles" in {
    the[InterpreterException] thrownBy (evalTestProject(
      "Cycle_Test"
    )) should have message "Compilation aborted due to errors."
    consumeOut should contain("Export statements form a cycle:")
  }

  "Import statements" should "allow for importing submodules" in {
    evalTestProject("TestSubmodules") shouldEqual 42
    val outLines = consumeOut
    outLines(0) shouldEqual "(Foo 10)"
    outLines(1) shouldEqual "(Mk_C 52)"
    outLines(2) shouldEqual "20"
    outLines(3) shouldEqual "(Mk_C 10)"
  }

  "Compiler" should "detect name conflicts preventing users from importing submodules" in {
    the[InterpreterException] thrownBy evalTestProject(
      "TestSubmodulesNameConflict"
    ) should have message "Method `c_mod_method` of C could not be found."
    val outLines = consumeOut
    outLines(2) should include
    "Declaration of type C shadows module local.TestSubmodulesNameConflict.A.B.C making it inaccessible via a qualified name."
  }

  "Compiler" should "accept exports of the same module" in {
    evalTestProject("Test_Multiple_Exports") shouldEqual 0
    val outLines = consumeOut
    outLines(0) shouldEqual "z"
    outLines(1) shouldEqual "42"
  }

  "Compiler" should "reject qualified exports of the same module with conflicting hidden names" in {
    the[InterpreterException] thrownBy evalTestProject(
      "Test_Multiple_Conflicting_Exports_1"
    ) should have message "Compilation aborted due to errors."
    val outLines = consumeOut
    outLines(
      1
    ) shouldEqual "Hidden 'foo' name of the exported module local.Test_Multiple_Conflicting_Exports_1.F1 conflicts with the qualified export"
  }

  "Compiler" should "reject unqualified exports of the same module with conflicting hidden names" in {
    the[InterpreterException] thrownBy evalTestProject(
      "Test_Multiple_Conflicting_Exports_2"
    ) should have message "Compilation aborted due to errors."
    val outLines = consumeOut
    outLines(
      1
    ) shouldEqual "Hidden 'bar' name of the export module local.Test_Multiple_Conflicting_Exports_2.F1 conflicts with the unqualified export"
  }

  "Polyglot symbols" should "not be exported" in {
    the[InterpreterException] thrownBy evalTestProject(
      "Test_Polyglot_Exports"
    ) should have message "Compilation aborted due to errors."
    val outLines = consumeOut
    outLines should have length 3
    outLines(
      2
    ) shouldEqual "Main.enso[5:16-5:19]: The name `Long` could not be found."
  }

  "Constructors" should "be importable" in {
    evalTestProject("Test_Type_Imports").toString shouldEqual "(Some 10)"
  }

  "Constructors" should "be exportable" in {
    evalTestProject("Test_Type_Exports").toString shouldEqual "(Some 10)"
  }
}
