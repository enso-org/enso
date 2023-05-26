package org.enso.interpreter.test.semantic

import org.enso.interpreter.test.{InterpreterException, PackageTest}

class ImportsTest extends PackageTest {
  implicit def messagingNatureOInterpreterException
    : org.scalatest.enablers.Messaging[InterpreterException] =
    new org.scalatest.enablers.Messaging[InterpreterException] {
      def messageOf(exception: InterpreterException): String =
        exception.getLocalizedMessage
    }

  "Atoms and methods" should "be available for import" in {
    evalTestProject("TestSimpleImports") shouldEqual 20
  }

  "Methods defined together with atoms" should "be visible even if not imported" in {
    evalTestProject("TestNonImportedOwnMethods") shouldEqual 10
  }

  "Overloaded methods" should "not be visible when not imported" in {
    the[InterpreterException] thrownBy evalTestProject(
      "TestNonImportedOverloads"
    ) should have message "Method `method` of type X could not be found."
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

  "Importing everything from the module" should "should not bring module into the scope when resolving names" in {
    evalTestProject("Test_Import_Case") shouldEqual 0
  }

  "Exports system" should "detect cycles" in {
    the[InterpreterException] thrownBy (evalTestProject(
      "Cycle_Test"
    )) should have message "Compilation aborted due to errors."
    consumeOut should contain("Export statements form a cycle:")
  }

  "Exports system" should "honor logical export" in {
    val compilationResult = evalTestProject(
      "Logical_Import_Violated_Test"
    )
    compilationResult shouldEqual "Element with Internal"
    consumeOut shouldEqual List()
  }

  "Import statements" should "allow for importing submodules" in {
    evalTestProject("TestSubmodules") shouldEqual 42
    val outLines = consumeOut
    outLines(0) shouldEqual "(Foo 10)"
    outLines(1) shouldEqual "(Mk_C 52)"
    outLines(2) shouldEqual "20"
    outLines(3) shouldEqual "(Mk_C 10)"
  }

  "Importing module" should "bring extension methods into the scope " in {
    evalTestProject("Test_Extension_Methods_Success_1") shouldEqual 42
  }

  "The unqualified import of a module" should "bring extension methods into the scope " in {
    evalTestProject("Test_Extension_Methods_Success_2") shouldEqual 42
  }

  "Importing module's types" should "not bring extension methods into the scope " in {
    the[InterpreterException] thrownBy evalTestProject(
      "Test_Extension_Methods_Failure"
    ) should have message "Method `foo` of type Integer could not be found."
  }

  "Compiler" should "detect name conflicts preventing users from importing submodules" in {
    the[InterpreterException] thrownBy evalTestProject(
      "TestSubmodulesNameConflict"
    ) should have message "Method `c_mod_method` of type C.type could not be found."
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

  "Fully qualified names" should "not be resolved when lacking imports" in {
    the[InterpreterException] thrownBy evalTestProject(
      "Test_Fully_Qualified_Name_Failure"
    ) should have message "Compilation aborted due to errors."

    val outLines = consumeOut
    outLines should have length 3
    outLines(
      2
    ) shouldEqual "Main.enso[2:14-2:17]: Fully qualified name references a library Standard.Base but an import statement for it is missing."
  }

  "Fully qualified names" should "be resolved when library has already been loaded" in {
    evalTestProject(
      "Test_Fully_Qualified_Name_Success"
    ).toString shouldEqual "0"
    val outLines = consumeOut
    outLines should have length 1
    outLines(0) shouldEqual "Hello world!"
  }

  "Fully qualified names" should "detect conflicts with the exported types sharing the namespace" in {
    the[InterpreterException] thrownBy evalTestProject(
      "Test_Fully_Qualified_Name_Conflict"
    ) should have message "Method `Foo` of type Atom.type could not be found."
    val outLines = consumeOut
    outLines should have length 3
    outLines(
      2
    ) shouldEqual "Main.enso[2:1-2:57]: The exported type `Atom` in `local.Test_Fully_Qualified_Name_Conflict.Atom` module will cause name conflict when attempting to use a fully qualified name of the `local.Test_Fully_Qualified_Name_Conflict.Atom.Foo` module."
  }

  "Deeply nested modules" should "infer correct synthetic modules" in {
    evalTestProject(
      "Test_Deeply_Nested_Modules"
    ).toString shouldEqual "0"
    val outLines = consumeOut
    outLines should have length 3
    outLines(0) shouldEqual "(A_Mod.Value 1)"
    outLines(1) shouldEqual "(C_Mod.Value 1)"
    outLines(2) shouldEqual "(D_Mod.Value 1)"
  }

}
