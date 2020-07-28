package org.enso.compiler.test.pass

import org.enso.compiler.Passes
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.{
  IRPass,
  PassConfiguration,
  PassGroup,
  PassManager
}
import org.enso.compiler.pass.analyse.{
  AliasAnalysis,
  DataflowAnalysis,
  DemandAnalysis,
  TailCall
}
import org.enso.compiler.pass.desugar._
import org.enso.compiler.pass.lint.UnusedBindings
import org.enso.compiler.pass.optimise.{
  ApplicationSaturation,
  LambdaConsolidate
}
import org.enso.compiler.pass.resolve.{IgnoredBindings, OverloadsResolution}
import org.enso.compiler.test.CompilerTest

class PassManagerTest extends CompilerTest {

  // === Test Setup ===========================================================

  val invalidOrdering: List[IRPass] = List(
    ComplexType,
    FunctionBinding,
    GenerateMethodBodies,
    SectionsToBinOp,
    OperatorToFunction,
    LambdaShorthandToLambda,
    IgnoredBindings,
    AliasAnalysis,
    LambdaConsolidate,
    OverloadsResolution,
    DemandAnalysis,
    ApplicationSaturation,
    TailCall,
    AliasAnalysis,
    DataflowAnalysis,
    UnusedBindings
  )

  val validOrdering: List[PassGroup] = (new Passes).passOrdering

  val passConfiguration: PassConfiguration = new PassConfiguration()

  // === The Tests ============================================================

  "The pass manager" should {
    "raise an error due to invalidations" in {
      a[CompilerError] shouldBe thrownBy(
        new PassManager(List(new PassGroup(invalidOrdering)), passConfiguration)
      )
    }

    "allow a valid pass ordering" in {
      noException shouldBe thrownBy(
        new PassManager(validOrdering, passConfiguration)
      )
    }
  }
}
