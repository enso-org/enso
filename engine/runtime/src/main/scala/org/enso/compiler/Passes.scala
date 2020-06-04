package org.enso.compiler

import org.enso.compiler.pass.PassConfiguration._
import org.enso.compiler.pass.analyse._
import org.enso.compiler.pass.desugar._
import org.enso.compiler.pass.lint.{ShadowedPatternFields, UnusedBindings}
import org.enso.compiler.pass.optimise.{
  ApplicationSaturation,
  LambdaConsolidate,
  UnreachableMatchBranches
}
import org.enso.compiler.pass.resolve.{
  DocumentationComments,
  IgnoredBindings,
  OverloadsResolution
}
import org.enso.compiler.pass.{IRPass, PassConfiguration, PassManager}

class Passes(passes: Option[List[IRPass]] = None) {

  /** A list of the compiler phases, in the order they should be run.
    *
    * The pass manager checks at runtime whether the provided order respects the
    * dependencies between passes, and so this pass ordering must adhere to
    * these dependencies.
    */
  val passOrdering: List[IRPass] = passes.getOrElse(
    List(
      DocumentationComments,
      ComplexType,
      FunctionBinding,
      GenerateMethodBodies,
      SectionsToBinOp,
      OperatorToFunction,
      LambdaShorthandToLambda,
      ShadowedPatternFields,
      UnreachableMatchBranches,
      NestedPatternMatch,
      IgnoredBindings,
      AliasAnalysis,
      LambdaConsolidate,
      OverloadsResolution,
      AliasAnalysis,
      DemandAnalysis,
      AliasAnalysis,
      ApplicationSaturation,
      TailCall,
      AliasAnalysis,
      DataflowAnalysis,
      CachePreferenceAnalysis,
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
