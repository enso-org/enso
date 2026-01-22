package org.enso.compiler.test.pass.desugar

import org.enso.compiler.context.{FreshNameSupply, InlineContext, ModuleContext}
import org.enso.compiler.core.ir.{
  CallArgument,
  DefinitionArgument,
  Expression,
  Function,
  Module,
  Name
}
import org.enso.compiler.core.CompilerError
import org.enso.compiler.core.ir.expression.{Application, Section}
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.IRProcessingPass
import org.enso.compiler.pass.analyse._
import org.enso.compiler.pass.lint.UnusedBindings
import org.enso.persist.Persistance.Reference

/** This pass converts operator sections to applications of binary operators.
  *
  * This pass has no configuration.
  *
  * This pass requires the context to provide:
  *
  * - A [[FreshNameSupply]].
  */
case object SectionsToBinOpMegaPass extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override lazy val precursorPasses: Seq[IRProcessingPass] = List(
    org.enso.compiler.pass.desugar.GenerateMethodBodies
  )
  override lazy val invalidatedPasses: Seq[IRProcessingPass] = List(
    AliasAnalysis,
    CachePreferenceAnalysis,
    DataflowAnalysis,
    DemandAnalysis,
    TailCall.INSTANCE,
    UnusedBindings
  )

  /** Performs section to binary operator conversion on an IR module.
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: Module,
    moduleContext: ModuleContext
  ): Module =
    ir.mapExpressions(
      runExpression(
        _,
        new InlineContext(
          moduleContext,
          freshNameSupply = moduleContext.freshNameSupply,
          compilerConfig  = moduleContext.compilerConfig
        )
      )
    )

  /** Performs section to binary operator conversion on an IR expression.
    *
    * @param ir the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: Expression,
    inlineContext: InlineContext
  ): Expression = {
    val freshNameSupply = inlineContext.freshNameSupply.getOrElse(
      throw new CompilerError(
        "A fresh name supply is required for sections desugaring."
      )
    )

    ir.transformExpressions { case sec: Section =>
      desugarSections(sec, freshNameSupply, inlineContext)
    }
  }

  /** Desugars operator sections to fully-saturated applications of operators.
    *
    * For a left sections it will generate a partially-applied function. For
    * right sections it will generate a lambda. For sides sections it is forced
    * to generate a lambda returning a partially applied function as we do not
    * currently support partial application without the this argument.
    *
    * @param section the section to desugar
    * @return the result of desugaring `section`
    */
  private def desugarSections(
    section: Section,
    freshNameSupply: FreshNameSupply,
    inlineContext: InlineContext
  ): Expression = {
    section match {
      case sectionLeft: Section.Left =>
        val arg          = sectionLeft.arg()
        val op           = sectionLeft.operator()
        val passData     = sectionLeft.passData()
        val loc          = sectionLeft.identifiedLocation()
        val rightArgName = freshNameSupply.newName()
        val rightCallArg = CallArgument.Specified
          .builder()
          .name(None)
          .value(rightArgName)
          .isSynthetic(true)
          .build()
        val rightDefArg = DefinitionArgument.Specified
          .builder()
          .name(rightArgName.duplicate())
          .suspended(false)
          .build()

        if (arg.value.isInstanceOf[Name.Blank]) {
          val leftArgName = freshNameSupply.newName()
          val leftCallArg = CallArgument.Specified
            .builder()
            .name(None)
            .value(leftArgName)
            .isSynthetic(true)
            .build()
          val leftDefArg = DefinitionArgument.Specified
            .builder()
            .name(leftArgName.duplicate())
            .suspended(false)
            .build()
          val opCall = Application.Prefix
            .builder()
            .function(op)
            .arguments(List(leftCallArg, rightCallArg))
            .hasDefaultsSuspended(false)
            .passData(passData)
            .diagnostics(sectionLeft.diagnostics)
            .build()
          val rightLam = Function.Lambda
            .builder()
            .arguments(List(rightDefArg))
            .bodyReference(Reference.of(opCall))
            .canBeTCO(true)
            .build()
          Function.Lambda
            .builder()
            .arguments(List(leftDefArg))
            .bodyReference(Reference.of(rightLam))
            .location(loc)
            .canBeTCO(true)
            .build()
        } else {
          val newArg = arg.mapExpressions(runExpression(_, inlineContext))

          Application.Prefix
            .builder()
            .function(op)
            .arguments(List(newArg))
            .hasDefaultsSuspended(false)
            .location(loc)
            .passData(passData)
            .diagnostics(sectionLeft.diagnostics)
            .build()
        }

      case sectionSides: Section.Sides =>
        val leftArgName = freshNameSupply.newName()
        val leftCallArg = CallArgument.Specified
          .builder()
          .name(None)
          .value(leftArgName)
          .isSynthetic(true)
          .build()
        val leftDefArg = DefinitionArgument.Specified
          .builder()
          .name(leftArgName.duplicate())
          .suspended(false)
          .build()

        val rightArgName = freshNameSupply.newName()
        val rightCallArg = CallArgument.Specified
          .builder()
          .name(None)
          .value(rightArgName)
          .isSynthetic(true)
          .build()
        val rightDefArg = DefinitionArgument.Specified
          .builder()
          .name(rightArgName.duplicate())
          .suspended(false)
          .build()

        val opCall = Application.Prefix
          .builder()
          .function(sectionSides.operator())
          .arguments(List(leftCallArg, rightCallArg))
          .hasDefaultsSuspended(false)
          .passData(sectionSides.passData())
          .diagnostics(sectionSides.diagnostics)
          .build()

        val rightLambda = Function.Lambda
          .builder()
          .arguments(List(rightDefArg))
          .bodyReference(Reference.of(opCall))
          .canBeTCO(true)
          .build()

        Function.Lambda
          .builder()
          .arguments(List(leftDefArg))
          .bodyReference(Reference.of(rightLambda))
          .location(sectionSides.identifiedLocation())
          .canBeTCO(true)
          .build()

      /* Note [Blanks in Sections]
       * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       * While the naiive compositional translation of `(- _)` first translates
       * the section into a function applying `-` to two arguments, one of which
       * is a blank, the compositional nature of the blanks translation actually
       * works against us here.
       *
       * As the `LambdaShorthandToLambda` pass can only operate on the
       * application with the blanks, it can't know to push the blank outside
       * that application chain. To that end, we have to handle this case
       * specially here instead. What we want it to translate to is as follows:
       *
       * `(- _)` == `x -> (- x)` == `x -> y -> y - x`
       *
       * We implement this special case here.
       *
       * The same is true of left sections.
       */

      case sectionRight: Section.Right =>
        val arg         = sectionRight.arg()
        val op          = sectionRight.operator()
        val passData    = sectionRight.passData()
        val loc         = sectionRight.identifiedLocation()
        val leftArgName = freshNameSupply.newName()
        val leftCallArg = CallArgument.Specified
          .builder()
          .name(None)
          .value(leftArgName)
          .isSynthetic(true)
          .build()
        val leftDefArg = DefinitionArgument.Specified
          .builder()
          .name(leftArgName.duplicate())
          .suspended(false)
          .build()

        if (arg.value.isInstanceOf[Name.Blank]) {
          // Note [Blanks in Sections]
          val rightArgName = freshNameSupply.newName()
          val rightCallArg = CallArgument.Specified
            .builder()
            .name(None)
            .value(rightArgName)
            .isSynthetic(true)
            .build()
          val rightDefArg = DefinitionArgument.Specified
            .builder()
            .name(rightArgName.duplicate())
            .suspended(false)
            .build()

          val opCall = Application.Prefix
            .builder()
            .function(op)
            .arguments(List(leftCallArg, rightCallArg))
            .hasDefaultsSuspended(false)
            .passData(passData)
            .diagnostics(sectionRight.diagnostics)
            .build()

          val leftLam = Function.Lambda
            .builder()
            .arguments(List(leftDefArg))
            .bodyReference(Reference.of(opCall))
            .canBeTCO(true)
            .build()

          Function.Lambda
            .builder()
            .arguments(List(rightDefArg))
            .bodyReference(Reference.of(leftLam))
            .location(loc)
            .canBeTCO(true)
            .build()
        } else {
          val newArg = arg.mapExpressions(runExpression(_, inlineContext))

          val opCall = Application.Prefix
            .builder()
            .function(op)
            .arguments(List(leftCallArg, newArg))
            .hasDefaultsSuspended(false)
            .passData(passData)
            .diagnostics(sectionRight.diagnostics)
            .build()

          Function.Lambda
            .builder()
            .arguments(List(leftDefArg))
            .bodyReference(Reference.of(opCall))
            .location(loc)
            .canBeTCO(true)
            .build()
        }
    }
  }
}
