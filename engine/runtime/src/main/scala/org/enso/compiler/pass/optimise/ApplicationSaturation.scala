package org.enso.compiler.pass.optimise

import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage._
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.AliasAnalysis
import org.enso.interpreter.node.{ExpressionNode => RuntimeExpression}
import org.enso.interpreter.runtime.callable.argument.CallArgument

/** This optimisation pass recognises fully-saturated applications of known
  * functions and writes analysis data that allows optimisation of them to
  * specific nodes at codegen time.
  *
  * This pass requires the context to provide:
  *
  * - A [[org.enso.compiler.pass.PassConfiguration]] containing an instance of
  *   [[ApplicationSaturation.Configuration]].
  *
  * It must have the following passes run before it:
  *
  * - [[org.enso.compiler.pass.desugar.GenerateMethodBodies]]
  * - [[org.enso.compiler.pass.desugar.SectionsToBinOp]]
  * - [[org.enso.compiler.pass.desugar.OperatorToFunction]]
  * - [[org.enso.compiler.pass.desugar.LambdaShorthandToLambda]]
  * - [[org.enso.compiler.pass.resolve.IgnoredBindings]]
  * - [[LambdaConsolidate]]
  */
case object ApplicationSaturation extends IRPass {

  /** Information on the saturation state of a function. */
  override type Metadata = CallSaturation

  override type Config = Configuration

  /** Executes the analysis pass, marking functions with information about their
    * argument saturation.
    *
    * @param ir the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    val passConfig = moduleContext.passConfiguration
    ir.transformExpressions({
      case x =>
        runExpression(x, new InlineContext(passConfiguration = passConfig))
    })
  }

  /** Executes the analysis pass, marking functions with information about their
    * argument saturation.
    *
    * @param ir the Enso IR to process
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  //noinspection DuplicatedCode
  override def runExpression(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = {
    val knownFunctions =
      inlineContext.passConfiguration
        .flatMap(configs => configs.get(this))
        .getOrElse(
          throw new CompilerError("Pass configuration is missing.")
        )
        .knownFunctions

    ir.transformExpressions {
      case func @ IR.Application.Prefix(fn, args, _, _, _, _) =>
        fn match {
          case name: IR.Name =>
            val aliasInfo =
              name
                .unsafeGetMetadata(
                  AliasAnalysis,
                  "Name occurrence with missing alias information."
                )
                .unsafeAs[AliasAnalysis.Info.Occurrence]

            if (!aliasInfo.graph.linkedToShadowingBinding(aliasInfo.id)) {
              knownFunctions.get(name.name) match {
                case Some(FunctionSpec(arity, codegenHelper)) =>
                  if (args.length == arity) {
                    val argsArePositional = args.forall(arg => arg.name.isEmpty)

                    // TODO [AA] In future this should work regardless of the
                    //  application style. Needs interpreter changes.
                    val saturationInfo = if (argsArePositional) {
                      CallSaturation.Exact(codegenHelper)
                    } else {
                      CallSaturation.ExactButByName()
                    }

                    func
                      .copy(
                        arguments = args.map(
                          _.mapExpressions((ir: IR.Expression) =>
                            runExpression(ir, inlineContext)
                          )
                        )
                      )
                      .updateMetadata(this -->> saturationInfo)

                  } else if (args.length > arity) {
                    func
                      .copy(
                        arguments = args.map(
                          _.mapExpressions((ir: IR.Expression) =>
                            runExpression(ir, inlineContext)
                          )
                        )
                      )
                      .updateMetadata(
                        this -->> CallSaturation.Over(args.length - arity)
                      )
                  } else {
                    func
                      .copy(
                        arguments = args.map(
                          _.mapExpressions((ir: IR.Expression) =>
                            runExpression(ir, inlineContext)
                          )
                        )
                      )
                      .updateMetadata(
                        this -->> CallSaturation.Partial(arity - args.length)
                      )
                  }
                case None =>
                  func
                    .copy(
                      arguments = args.map(
                        _.mapExpressions((ir: IR.Expression) =>
                          runExpression(ir, inlineContext)
                        )
                      )
                    )
                    .updateMetadata(this -->> CallSaturation.Unknown())
              }
            } else {
              func
                .copy(
                  function = runExpression(fn, inlineContext),
                  arguments =
                    args.map(_.mapExpressions(runExpression(_, inlineContext)))
                )
                .updateMetadata(this -->> CallSaturation.Unknown())
            }
          case _ =>
            func
              .copy(
                function = runExpression(fn, inlineContext),
                arguments =
                  args.map(_.mapExpressions(runExpression(_, inlineContext)))
              )
              .updateMetadata(this -->> CallSaturation.Unknown())
        }
    }
  }

  /** Configuration for this pass
    *
    * @param knownFunctions the mapping of known functions
    */
  sealed case class Configuration(
    knownFunctions: KnownFunctionsMapping = Map()
  ) extends IRPass.Configuration {
    override var shouldWriteToContext: Boolean = false
  }

  /** A function for constructing the optimised node for a function. */
  type CodegenHelper = List[CallArgument] => RuntimeExpression

  /** The configuration for this pass.
    *
    * The [[String]] is the name of the known function, while the
    * [[FunctionSpec]] describes said function.
    */
  type KnownFunctionsMapping = Map[String, FunctionSpec]

  /** Describes the saturation state of a function application. */
  sealed trait CallSaturation extends IRPass.Metadata
  object CallSaturation {
    sealed case class Over(additionalArgCount: Int) extends CallSaturation {
      override val metadataName: String =
        "ApplicationSaturation.CallSaturation.Over"
    }
    sealed case class Exact(helper: CodegenHelper) extends CallSaturation {
      override val metadataName: String =
        "ApplicationSaturation.CallSaturation.Exact"
    }
    sealed case class ExactButByName() extends CallSaturation {
      override val metadataName: String =
        "ApplicationSaturation.CallSaturation.ExactButByName"
    }
    sealed case class Partial(unappliedArgCount: Int) extends CallSaturation {
      override val metadataName: String =
        "ApplicationSaturation.CallSaturation.Partial"
    }
    sealed case class Unknown() extends CallSaturation {
      override val metadataName: String =
        "ApplicationSaturation.CallSaturation.Unknown"
    }
  }

  /** A description of a known function
    *
    * @param arity the number of arguments the function expects
    * @param codegenHelper a function that can construct the optimised node to
    *                      represent the function at codegen time.
    */
  sealed case class FunctionSpec(arity: Int, codegenHelper: CodegenHelper)
}
