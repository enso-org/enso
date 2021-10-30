package org.enso.compiler.test.core.ir

import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.DiagnosticStorage
import org.enso.compiler.test.CompilerTest
import org.enso.syntax.text.AST

class DiagnosticStorageTest extends CompilerTest {

  // === Test Configuration ===================================================

  /** Makes a basic diagnostic for testing purposes.
    *
    * @param name the name to give the internal diagnostic
    * @return a new diagnostic
    */
  def mkDiagnostic(name: String): IR.Diagnostic = {
    IR.Warning.Shadowed.FunctionParam(name, IR.Empty(None), None)
  }

  // === The Tests ============================================================

  "The IR diagnostics storage" should {
    "allow adding diagnostic results" in {
      val diagnostics = new DiagnosticStorage

      diagnostics.add(mkDiagnostic("a"))
      diagnostics.toList should contain(mkDiagnostic("a"))
    }

    "allow adding lists of diagnostic results" in {
      val diagnostics = new DiagnosticStorage

      diagnostics.add(
        List(
          mkDiagnostic("a"),
          mkDiagnostic("b"),
          mkDiagnostic("c")
        )
      )
      diagnostics.toList should contain(mkDiagnostic("a"))
      diagnostics.toList should contain(mkDiagnostic("b"))
      diagnostics.toList should contain(mkDiagnostic("c"))
    }

    "mapping across the diagnostics to produce a new sequence" in {
      val diagnostics = new DiagnosticStorage(
        List(
          mkDiagnostic("a"),
          mkDiagnostic("b"),
          mkDiagnostic("c")
        )
      )

      diagnostics.map(d => d.location) shouldEqual Seq(None, None, None)
    }

    "mapping across the diagnostics in place" in {
      val diagnostics = new DiagnosticStorage(
        List(
          mkDiagnostic("a"),
          mkDiagnostic("b"),
          mkDiagnostic("c")
        )
      )

      val expectedResult = List(
        mkDiagnostic("aaa"),
        mkDiagnostic("aaa"),
        mkDiagnostic("aaa")
      )

      diagnostics.mapInPlace {
        case s: IR.Warning.Shadowed.FunctionParam =>
          s.copy(shadowedName = "aaa")
        case x => x
      }

      diagnostics.toList shouldEqual expectedResult
    }

    "collecting across the diagnostics to produce a new sequence" in {
      val err =
        IR.Error.Syntax(AST.Blank(), IR.Error.Syntax.UnsupportedSyntax("aa"))

      val diagnostics = new DiagnosticStorage(
        List(
          mkDiagnostic("a"),
          mkDiagnostic("b"),
          mkDiagnostic("c"),
          err
        )
      )

      diagnostics.collect { case e: IR.Error.Syntax =>
        e
      } shouldEqual Seq(err)
    }

    "filtering the diagnostics" in {
      val diagnostics = new DiagnosticStorage(
        List(
          mkDiagnostic("aa"),
          mkDiagnostic("ba"),
          mkDiagnostic("cd")
        )
      )

      val result = new DiagnosticStorage(
        List(mkDiagnostic("aa"), mkDiagnostic("ba"))
      )

      diagnostics.filter {
        case s: IR.Warning.Shadowed.FunctionParam =>
          s.shadowedName.contains("a")
        case _ => false
      } shouldEqual result
    }

    "filtering the diagnostics in place" in {
      val diagnostics = new DiagnosticStorage(
        List(
          mkDiagnostic("aa"),
          mkDiagnostic("ba"),
          mkDiagnostic("cd")
        )
      )

      val result = List(mkDiagnostic("aa"), mkDiagnostic("ba"))

      diagnostics.filterInPlace {
        case s: IR.Warning.Shadowed.FunctionParam =>
          s.shadowedName.contains("a")
        case _ => false
      }

      diagnostics.toList shouldEqual result
    }

    "folding over the diagnostics" in {
      val diagnostics = new DiagnosticStorage(
        List(
          mkDiagnostic("a"),
          mkDiagnostic("b"),
          mkDiagnostic("cd")
        )
      )

      diagnostics.foldLeft("")((str, d) =>
        d match {
          case f: IR.Warning.Shadowed.FunctionParam => str + f.shadowedName
          case _                                    => str
        }
      ) shouldEqual "abcd"
    }
  }
}
