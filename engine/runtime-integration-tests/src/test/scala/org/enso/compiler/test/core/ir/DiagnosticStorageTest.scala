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
    warnings.Shadowed.FunctionParam(
      name,
      Empty(identifiedLocation = null),
      identifiedLocation = null
    )
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
  }
}
