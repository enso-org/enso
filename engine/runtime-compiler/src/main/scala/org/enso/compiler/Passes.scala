package org.enso.compiler

import org.enso.compiler.data.CompilerConfig
import org.enso.compiler.dump.IRDumperPass
import org.enso.compiler.pass.PassConfiguration._
import org.enso.compiler.pass.analyse._
import org.enso.compiler.pass.analyse.types.TypeInference
import org.enso.compiler.pass.desugar._
import org.enso.compiler.pass.lint.{
  ModuleNameConflicts,
  NoSelfInStatic,
  ShadowedPatternFields,
  UnusedBindings
}
import org.enso.compiler.pass.optimise.{
  LambdaConsolidate,
  UnreachableMatchBranches
}
import org.enso.compiler.pass.resolve._
import org.enso.compiler.pass.{
  IRProcessingPass,
  PassConfiguration,
  PassGroup,
  PassManager
}

class Passes(config: CompilerConfig) {

  val moduleDiscoveryPasses = new PassGroup(
    List(
      ModuleAnnotations,
      DocumentationComments,
      Imports,
      ComplexType,
      FunctionBinding,
      GenerateMethodBodies,
      BindingAnalysis
    ) ++ (if (config.isLintingDisabled) Nil else List(ModuleNameConflicts))
  )

  val globalTypingPasses = new PassGroup(
    List(
      MethodDefinitions,
      SectionsToBinOp.INSTANCE,
      OperatorToFunction,
      LambdaShorthandToLambda,
      ImportSymbolAnalysis,
      AmbiguousImportsAnalysis
    ) ++ (if (config.privateCheckEnabled) {
            List(
              PrivateModuleAnalysis.INSTANCE,
              PrivateConstructorAnalysis.INSTANCE
            )
          } else List())
    ++ List(
      ShadowedPatternFields,
      UnreachableMatchBranches,
      NestedPatternMatch,
      IgnoredBindings,
      TypeFunctions,
      TypeSignatures
    )
  )

  val functionBodyPasses = new PassGroup(
    List(
      ExpressionAnnotations,
      AliasAnalysis,
      FullyQualifiedNames,
      GlobalNames,
      TypeNames,
      MethodCalls,
      FullyAppliedFunctionUses,
      AliasAnalysis
    ) ++
    (if (config.autoParallelismEnabled) {
       List(
         AutomaticParallelism,
         AliasAnalysis
       )
     } else List()) ++ List(
      LambdaConsolidate,
      AliasAnalysis,
      SuspendedArguments,
      OverloadsResolution,
      AliasAnalysis,
      DemandAnalysis,
      AliasAnalysis,
      TailCall.INSTANCE,
      Patterns
    ) ++ (if (config.privateCheckEnabled) {
            List(PrivateSymbolsAnalysis.INSTANCE)
          } else List()) ++ List(
      AliasAnalysis,
      FramePointerAnalysis,
      DataflowAnalysis,
      CachePreferenceAnalysis,
      GenericAnnotations
    ) ++ (if (config.isLintingDisabled) {
            Nil
          } else {
            List(UnusedBindings, NoSelfInStatic)
          }) ++ (if (config.staticTypeInferenceEnabled) {
                   List(
                     TypeInference.INSTANCE
                   )
                 } else Nil) ++ (if (config.dumpIrs) {
                                   List(IRDumperPass.INSTANCE)
                                 } else Nil)
  )

  /** A list of the compiler phases, in the order they should be run.
    *
    * The pass manager checks at runtime whether the provided order respects the
    * dependencies between passes, and so this pass ordering must adhere to
    * these dependencies.
    */
  val passOrdering: List[PassGroup] =
    List(
      moduleDiscoveryPasses,
      globalTypingPasses,
      functionBodyPasses
    )

  /** The ordered representation of all passes run by the compiler. */
  val allPassOrdering: List[IRProcessingPass] = passOrdering.flatMap(_.passes)

  /** Configuration for the passes. */
  private val passConfig: PassConfiguration = PassConfiguration(
    AliasAnalysis -->> AliasAnalysis.Configuration()
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
  def getPrecursors(pass: IRProcessingPass): Option[PassGroup] = {
    val allPasses = passOrdering.flatMap(_.passes)
    val result    = allPasses.takeWhile(_ != pass)
    if (result.length != allPasses.length) {
      Some(new PassGroup(result))
    } else {
      None
    }
  }
}
