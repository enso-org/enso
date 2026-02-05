package org.enso.compiler.test.core.ir

import org.enso.compiler.core.ir.{Diagnostic, DiagnosticStorage, Empty}
import org.enso.compiler.core.ir.expression.warnings
import org.enso.compiler.test.CompilerTest

class DiagnosticStorageTest extends CompilerTest {

  // === Test Configuration ===================================================

  /** Makes a basic diagnostic for testing purposes.
    *
    * @param name the name to give the internal diagnostic
    * @return a new diagnostic
    */
  def mkDiagnostic(name: String): Diagnostic = {
    new warnings.Shadowed.FunctionParam(
      name,
      new Empty(null),
      null
    )
  }

  // === The Tests ============================================================

  "The IR diagnostics storage" should {
    "allow adding diagnostic results" in {
      val diagnostics = DiagnosticStorage.createEmpty

      diagnostics.add(mkDiagnostic("a"))
      shouldContain(diagnostics.toList, mkDiagnostic("a"))
    }

    "allow adding lists of diagnostic results" in {
      val diagnostics = DiagnosticStorage.createEmpty

      diagnostics.add(
        List(
          mkDiagnostic("a"),
          mkDiagnostic("b"),
          mkDiagnostic("c")
        )
      )
      shouldContain(diagnostics.toList, mkDiagnostic("a"))
      shouldContain(diagnostics.toList, mkDiagnostic("b"))
      shouldContain(diagnostics.toList, mkDiagnostic("c"))
    }
  }

  private def shouldContain(
    diagnostics: List[Diagnostic],
    diagnostic: Diagnostic
  ): Unit = {
    diagnostics.contains(diagnostic) shouldBe true
  }
}
