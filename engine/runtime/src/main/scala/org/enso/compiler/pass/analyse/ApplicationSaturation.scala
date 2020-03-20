package org.enso.compiler.pass.analyse

import org.enso.compiler.core.IR
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.analyse.ApplicationSaturation.{CallSaturation, Default, FunctionSpec, PassConfiguration}
import org.enso.interpreter.node.{ExpressionNode => RuntimeExpression}
import org.enso.interpreter.runtime.callable.argument.CallArgument
import org.enso.interpreter.runtime.scope.{LocalScope, ModuleScope}

import scala.annotation.unused

/** This optimisation pass recognises fully-saturated applications of known
  * functions and writes analysis data that allows optimisation of them to
  * specific nodes at codegen time.
  *
  * PLEASE NOTE: This implementation is _incomplete_ as the analysis it performs
  * is _unconditional_ at this stage. This means that, until we have alias
  * analysis information,
  *
  * PLEASE NOTE: This implementation is _incomplete_ as the analysis it performs
  * only operates for functions where the arguments are applied positionally.
  *
  * @param knownFunctions a mapping from known function names to information
  *                       about that function that can be used for optimisation
  */
case class ApplicationSaturation(
  knownFunctions: PassConfiguration = Default.Config
) extends IRPass {

  /** Information on the saturation state of a function. */
  override type Metadata = CallSaturation

  /** Executes the analysis pass, marking functions with information about their
    * argument saturation.
    *
    * @param ir the Enso IR to process
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(ir: IR.Module): IR.Module =
    ir.transformExpressions({ case x => runExpression(x) })

  /** Executes the analysis pass, marking functions with information about their
    * argument saturation.
    *
    * @param ir the Enso IR to process
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: IR.Expression,
    @unused localScope: Option[LocalScope]   = None,
    @unused moduleScope: Option[ModuleScope] = None
  ): IR.Expression = {
    ir.transformExpressions {
      case func @ IR.Application.Prefix(fn, args, _, _, meta) =>
        fn match {
          case name: IR.Name =>
            val aliasInfo = name
              .getMetadata[AliasAnalysis.Info.Occurrence]
              .getOrElse(
                throw new CompilerError(
                  "Name occurrence with missing alias information."
                )
              )

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

                    func.copy(
                      arguments = args.map(
                        _.mapExpressions(
                          (ir: IR.Expression) => runExpression(ir)
                        )
                      ),
                      passData = meta + saturationInfo
                    )

                  } else if (args.length > arity) {
                    func.copy(
                      arguments = args.map(
                        _.mapExpressions(
                          (ir: IR.Expression) => runExpression(ir)
                        )
                      ),
                      passData = meta + CallSaturation.Over(args.length - arity)
                    )
                  } else {
                    func.copy(
                      arguments = args.map(
                        _.mapExpressions(
                          (ir: IR.Expression) => runExpression(ir)
                        )
                      ),
                      passData = meta + CallSaturation.Partial(
                          arity - args.length
                        )
                    )
                  }
                case None =>
                  func.copy(
                    arguments = args.map(
                      _.mapExpressions((ir: IR.Expression) => runExpression(ir))
                    ),
                    passData = meta + CallSaturation.Unknown()
                  )
              }
            } else {
              func.copy(
                function = runExpression(fn),
                arguments = args.map(_.mapExpressions(runExpression(_))),
                passData = meta + CallSaturation.Unknown()
              )
            }
          case _ =>
            func.copy(
              function = runExpression(fn),
              arguments = args.map(_.mapExpressions(runExpression(_))),
              passData = meta + CallSaturation.Unknown()
            )
        }
    }
  }
}
object ApplicationSaturation {

  /** A function for constructing the optimised node for a function. */
  type CodegenHelper = List[CallArgument] => RuntimeExpression

  /** The configuration for this pass.
    *
    * The [[String]] is the name of the known function, while the
    * [[FunctionSpec]] describes said function.
    */
  type PassConfiguration = Map[String, FunctionSpec]

  /** Describes the saturation state of a function application. */
  sealed trait CallSaturation extends IR.Metadata
  object CallSaturation {
    sealed case class Over(additionalArgCount: Int)   extends CallSaturation
    sealed case class Exact(helper: CodegenHelper)    extends CallSaturation
    sealed case class ExactButByName()                extends CallSaturation
    sealed case class Partial(unappliedArgCount: Int) extends CallSaturation
    sealed case class Unknown()                       extends CallSaturation
  }

  /** A description of a known function
    *
    * @param arity the number of arguments the function expects
    * @param codegenHelper a function that can construct the optimised node to
    *                      represent the function at codegen time.
    */
  sealed case class FunctionSpec(arity: Int, codegenHelper: CodegenHelper)

  /** Defaults for the pass. */
  object Default {

    /** The default configuration of known functions for this pass. */
    val Config: PassConfiguration = Map()
  }
}
