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
      case sectionLeft @ Section.Left(arg, op, loc, passData) =>
        val rightArgName = freshNameSupply.newName()
        val rightCallArg =
          CallArgument.Specified(None, rightArgName, identifiedLocation = null)
        val rightDefArg = DefinitionArgument.Specified(
          rightArgName.duplicate(),
          None,
          None,
          suspended = false,
          null
        )

        if (arg.value.isInstanceOf[Name.Blank]) {
          val leftArgName = freshNameSupply.newName()
          val leftCallArg =
            CallArgument.Specified(None, leftArgName, identifiedLocation = null)
          val leftDefArg = DefinitionArgument.Specified(
            leftArgName.duplicate(),
            None,
            None,
            suspended = false,
            null
          )
          val opCall = new Application.Prefix(
            function             = op,
            arguments            = List(leftCallArg, rightCallArg),
            hasDefaultsSuspended = false,
            identifiedLocation   = null,
            passData             = passData,
            diagnostics          = sectionLeft.diagnostics
          )

          val rightLam = new Function.Lambda(
            List(rightDefArg),
            opCall,
            identifiedLocation = null
          )

          new Function.Lambda(
            List(leftDefArg),
            rightLam,
            loc
          )
        } else {
          val newArg = arg.mapExpressions(runExpression(_, inlineContext))

          new Application.Prefix(
            function             = op,
            arguments            = List(newArg),
            hasDefaultsSuspended = false,
            identifiedLocation   = loc,
            passData             = passData,
            diagnostics          = sectionLeft.diagnostics
          )
        }

      case sectionSides @ Section.Sides(op, loc, passData) =>
        val leftArgName = freshNameSupply.newName()
        val leftCallArg =
          CallArgument.Specified(None, leftArgName, identifiedLocation = null)
        val leftDefArg = DefinitionArgument.Specified(
          leftArgName.duplicate(),
          None,
          None,
          suspended = false,
          null
        )

        val rightArgName = freshNameSupply.newName()
        val rightCallArg =
          CallArgument.Specified(None, rightArgName, identifiedLocation = null)
        val rightDefArg = DefinitionArgument.Specified(
          rightArgName.duplicate(),
          None,
          None,
          suspended = false,
          null
        )

        val opCall = new Application.Prefix(
          function             = op,
          arguments            = List(leftCallArg, rightCallArg),
          hasDefaultsSuspended = false,
          identifiedLocation   = null,
          passData             = passData,
          diagnostics          = sectionSides.diagnostics
        )

        val rightLambda = new Function.Lambda(
          List(rightDefArg),
          opCall,
          identifiedLocation = null
        )

        new Function.Lambda(
          List(leftDefArg),
          rightLambda,
          loc
        )

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

      case sectionRight @ Section.Right(op, arg, loc, passData) =>
        val leftArgName = freshNameSupply.newName()
        val leftCallArg =
          CallArgument.Specified(None, leftArgName, identifiedLocation = null)
        val leftDefArg =
          DefinitionArgument.Specified(
            leftArgName.duplicate(),
            None,
            None,
            suspended = false,
            null
          )

        if (arg.value.isInstanceOf[Name.Blank]) {
          // Note [Blanks in Sections]
          val rightArgName = freshNameSupply.newName()
          val rightCallArg =
            CallArgument.Specified(
              None,
              rightArgName,
              identifiedLocation = null
            )
          val rightDefArg = DefinitionArgument.Specified(
            rightArgName.duplicate(),
            None,
            None,
            suspended = false,
            null
          )

          val opCall = new Application.Prefix(
            function             = op,
            arguments            = List(leftCallArg, rightCallArg),
            hasDefaultsSuspended = false,
            identifiedLocation   = null,
            passData             = passData,
            diagnostics          = sectionRight.diagnostics
          )

          val leftLam = new Function.Lambda(
            List(leftDefArg),
            opCall,
            identifiedLocation = null
          )

          new Function.Lambda(
            List(rightDefArg),
            leftLam,
            loc
          )
        } else {
          val newArg = arg.mapExpressions(runExpression(_, inlineContext))

          val opCall = new Application.Prefix(
            function             = op,
            arguments            = List(leftCallArg, newArg),
            hasDefaultsSuspended = false,
            identifiedLocation   = null,
            passData             = passData,
            diagnostics          = sectionRight.diagnostics
          )

          new Function.Lambda(
            List(leftDefArg),
            opCall,
            loc
          )
        }
    }
  }
}
