package org.enso.compiler.pass.desugar

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
case object SectionsToBinOp extends IRPass {
  override type Metadata = IRPass.Metadata.Empty
  override type Config   = IRPass.Configuration.Default

  override val precursorPasses: Seq[IRPass] = List(
    GenerateMethodBodies
  )
  override val invalidatedPasses: Seq[IRPass] = List(
    AliasAnalysis,
    CachePreferenceAnalysis,
    DataflowAnalysis,
    DemandAnalysis,
    TailCall,
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
      desugarSections(sec, freshNameSupply)
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
    freshNameSupply: FreshNameSupply
  ): Expression = {
    section match {
      case Section.Left(arg, op, loc, passData, diagnostics) =>
        val rightArgName = freshNameSupply.newName()
        val rightCallArg =
          CallArgument.Specified(None, rightArgName, None)
        val rightDefArg = DefinitionArgument.Specified(
          rightArgName.duplicate(),
          None,
          None,
          suspended = false,
          None
        )

        if (arg.value.isInstanceOf[Name.Blank]) {
          val leftArgName = freshNameSupply.newName()
          val leftCallArg =
            CallArgument.Specified(None, leftArgName, None)
          val leftDefArg = DefinitionArgument.Specified(
            leftArgName.duplicate(),
            None,
            None,
            suspended = false,
            None
          )
          val opCall = Application.Prefix(
            function             = op,
            arguments            = List(leftCallArg, rightCallArg),
            hasDefaultsSuspended = false,
            location             = None,
            passData,
            diagnostics
          )

          val rightLam = Function.Lambda(
            List(rightDefArg),
            opCall,
            None
          )

          Function.Lambda(
            List(leftDefArg),
            rightLam,
            loc
          )

        } else {
          Application.Prefix(
            function             = op,
            arguments            = List(arg),
            hasDefaultsSuspended = false,
            location             = loc,
            passData,
            diagnostics
          )
        }

      case Section.Sides(op, loc, passData, diagnostics) =>
        val leftArgName = freshNameSupply.newName()
        val leftCallArg =
          CallArgument.Specified(None, leftArgName, None)
        val leftDefArg = DefinitionArgument.Specified(
          leftArgName.duplicate(),
          None,
          None,
          suspended = false,
          None
        )

        val rightArgName = freshNameSupply.newName()
        val rightCallArg =
          CallArgument.Specified(None, rightArgName, None)
        val rightDefArg = DefinitionArgument.Specified(
          rightArgName.duplicate(),
          None,
          None,
          suspended = false,
          None
        )

        val opCall = Application.Prefix(
          function             = op,
          arguments            = List(leftCallArg, rightCallArg),
          hasDefaultsSuspended = false,
          location             = None,
          passData,
          diagnostics
        )

        val rightLambda = Function.Lambda(
          List(rightDefArg),
          opCall,
          None
        )

        Function.Lambda(
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

      case Section.Right(op, arg, loc, passData, diagnostics) =>
        val leftArgName = freshNameSupply.newName()
        val leftCallArg =
          CallArgument.Specified(None, leftArgName, None)
        val leftDefArg =
          DefinitionArgument.Specified(
            leftArgName.duplicate(),
            None,
            None,
            suspended = false,
            None
          )

        if (arg.value.isInstanceOf[Name.Blank]) {
          // Note [Blanks in Sections]
          val rightArgName = freshNameSupply.newName()
          val rightCallArg =
            CallArgument.Specified(None, rightArgName, None)
          val rightDefArg = DefinitionArgument.Specified(
            rightArgName.duplicate(),
            None,
            None,
            suspended = false,
            None
          )

          val opCall = Application.Prefix(
            function             = op,
            arguments            = List(leftCallArg, rightCallArg),
            hasDefaultsSuspended = false,
            location             = None,
            passData,
            diagnostics
          )

          val leftLam = Function.Lambda(
            List(leftDefArg),
            opCall,
            None
          )

          Function.Lambda(
            List(rightDefArg),
            leftLam,
            loc
          )
        } else {
          val opCall = Application.Prefix(
            function             = op,
            arguments            = List(leftCallArg, arg),
            hasDefaultsSuspended = false,
            location             = None,
            passData,
            diagnostics
          )

          Function.Lambda(
            List(leftDefArg),
            opCall,
            loc
          )
        }
    }
  }
}
