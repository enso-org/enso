package org.enso.compiler

import org.enso.compiler.pass.PassConfiguration._
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
import org.enso.compiler.pass.{IRPass, PassConfiguration, PassManager}

class Passes(passes: Option[List[IRPass]] = None) {

  /** A list of the compiler phases, in the order they should be run.
    *
    * Please note that these passes _must_ be run in this order. While we
    * currently can't account for the dependencies between passes in the types,
    * they nevertheless exist.
    */
  private val passOrdering: List[IRPass] = passes.getOrElse(
    List(
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
      AliasAnalysis,
      DemandAnalysis,
      ApplicationSaturation,
      TailCall,
      AliasAnalysis,
      DataflowAnalysis,
      UnusedBindings
    )
  )

  /** Configuration for the passes. */
  private val passConfig: PassConfiguration = PassConfiguration(
    ApplicationSaturation -->> ApplicationSaturation.Configuration(),
    AliasAnalysis         -->> AliasAnalysis.Configuration()
  )

  /** The pass manager for running compiler passes. */
  val passManager: PassManager =
    new PassManager(passOrdering, passConfig)

  /** Slices the compiler's pass ordering to provide the list of all passes that
    * run _before_ [[pass]].
    *
    * This will compute it for the _first_ instance of [[pass]].
    *
    * @param pass the pass to get the precursors for
    * @return the precursors to the first instance of `pass`
    */
  def getPrecursors(pass: IRPass): Option[List[IRPass]] = {
    val result = passOrdering.takeWhile(_ != pass)

    if (result.length != passOrdering.length) {
      Some(result)
    } else {
      None
    }
  }
}
