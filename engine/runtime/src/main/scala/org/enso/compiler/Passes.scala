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
import org.enso.compiler.pass.resolve._
import org.enso.compiler.pass.{
  IRPass,
  PassConfiguration,
  PassGroup,
  PassManager
}

class Passes(passes: Option[List[PassGroup]] = None) {

  val moduleDiscoveryPasses = new PassGroup(
    List(
      DocumentationComments,
      ComplexType,
      FunctionBinding,
      GenerateMethodBodies,
      BindingAnalysis
    )
  )

  val functionBodyPasses = new PassGroup(
    List(
      MethodDefinitions,
      SectionsToBinOp,
      OperatorToFunction,
      LambdaShorthandToLambda,
      ShadowedPatternFields,
      UnreachableMatchBranches,
      NestedPatternMatch,
      IgnoredBindings,
      TypeFunctions,
      TypeSignatures,
      AliasAnalysis,
      LambdaConsolidate,
      AliasAnalysis,
      SuspendedArguments,
      OverloadsResolution,
      AliasAnalysis,
      DemandAnalysis,
      AliasAnalysis,
      ApplicationSaturation,
      TailCall,
      Patterns,
      AliasAnalysis,
      DataflowAnalysis,
      CachePreferenceAnalysis,
      UnusedBindings
    )
  )

  /** A list of the compiler phases, in the order they should be run.
    *
    * The pass manager checks at runtime whether the provided order respects the
    * dependencies between passes, and so this pass ordering must adhere to
    * these dependencies.
    */
  val passOrdering: List[PassGroup] = passes.getOrElse(
    List(moduleDiscoveryPasses, functionBodyPasses)
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
  def getPrecursors(pass: IRPass): Option[PassGroup] = {
    val allPasses = passOrdering.flatMap(_.passes)
    val result    = allPasses.takeWhile(_ != pass)
    if (result.length != allPasses.length) {
      Some(new PassGroup(result))
    } else {
      None
    }
  }
}
