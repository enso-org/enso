package org.enso.compiler.pass.analyse

import org.enso.compiler.Compiler
import org.enso.compiler.context.{InlineContext, ModuleContext}
import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.MetadataStorage.ToPair
import org.enso.compiler.exception.CompilerError
import org.enso.compiler.pass.IRPass
import org.enso.compiler.pass.desugar.{
  FunctionBinding,
  GenerateMethodBodies,
  OperatorToFunction
}

object FrameIndexAnalysis extends IRPass {
  override type Metadata = FrameInfo
  override type Config   = Configuration

  override val precursorPasses: Seq[IRPass] = List(
    FunctionBinding,
    GenerateMethodBodies,
    OperatorToFunction,
    AliasAnalysis
  )

  override val invalidatedPasses: Seq[IRPass] = List()

  sealed class FrameInfo extends IRPass.Metadata {
    override val metadataName: String = "FrameIndexAnalysis.FrameInfo"

    /** @inheritdoc */
    override def prepareForSerialization(compiler: Compiler): FrameInfo = {
      this
    }

    /** @inheritdoc */
    override def restoreFromSerialization(
      compiler: Compiler
    ): Option[FrameInfo] = {
      Some(this)
    }

    /** @inheritdoc */
    override def duplicate(): Option[IRPass.Metadata] = None
  }
  object FrameInfo {
    sealed class FrameIndex(
      val index: Int
    ) extends FrameInfo

    sealed class LocalVariables(
      var variables: List[IR.Identifier] = List()
    ) extends FrameInfo
  }

  /** Executes the pass on the provided `ir`, and returns a possibly transformed
    * or annotated version of `ir`.
    *
    * @param ir            the Enso IR to process
    * @param moduleContext a context object that contains the information needed
    *                      to process a module
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(
    ir: IR.Module,
    moduleContext: ModuleContext
  ): IR.Module = {
    ir.copy(
      bindings = ir.bindings.map(moduleDef => {
        analyseModuleDefinition(moduleDef)
      })
    )
  }

  /** Executes the pass on the provided `ir`, and returns a possibly transformed
    * or annotated version of `ir` in an inline context.
    *
    * @param ir            the Enso IR to process
    * @param inlineContext a context object that contains the information needed
    *                      for inline evaluation
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(
    ir: IR.Expression,
    inlineContext: InlineContext
  ): IR.Expression = {
    analyseExpression(ir, new FrameInfo.LocalVariables())
  }

  private def analyseModuleDefinition(
    ir: IR.Module.Scope.Definition
  ): IR.Module.Scope.Definition = {
    ir match {
      case method: IR.Module.Scope.Definition.Method.Explicit =>
        method.body match {
          case function: IR.Function =>
            method.copy(
              body = analyseFunction(
                function,
                new FrameInfo.LocalVariables()
              )
            )
          case _ =>
            throw new CompilerError(
              "The body of a method should always be a function"
            )
        }
      case method: IR.Module.Scope.Definition.Method.Conversion =>
        method.body match {
          case function: IR.Function =>
            method.copy(
              body = analyseFunction(
                function,
                new FrameInfo.LocalVariables()
              )
            )
          case _ =>
            throw new CompilerError(
              "The body of a method should always be a function"
            )
        }
      case _: IR.Module.Scope.Definition.Method.Binding =>
        throw new CompilerError(
          "Method definition sugar should not occur during frame index analysis"
        )
      case t: IR.Module.Scope.Definition.Type =>
        t.copy(
          params = analyseArgumentDefs(
            t.params,
            new FrameInfo.LocalVariables()
          )
        )
      case _ =>
        val localVars = new FrameInfo.LocalVariables()
        ir.mapExpressions(expr => analyseExpression(expr, localVars))
    }
  }

  private def analyseFunction(
    function: IR.Function,
    parentLocalVars: FrameInfo.LocalVariables
  ): IR.Function = {
    val currentLocalVars = new FrameInfo.LocalVariables()
    function match {
      case lambda @ IR.Function.Lambda(arguments, body, _, _, _, _) =>
        lambda
          .copy(
            arguments = analyseArgumentDefs(arguments, parentLocalVars),
            body      = analyseExpression(body, currentLocalVars)
          )
          .updateMetadata(
            this -->> currentLocalVars
          )
      case _: IR.Function.Binding =>
        throw new CompilerError(
          "Function sugar should not be present during frame index analysis."
        )
    }
  }

  private def analyseArgumentDefs(
    args: List[IR.DefinitionArgument],
    parentLocalVars: FrameInfo.LocalVariables
  ): List[IR.DefinitionArgument] = {
    args.map(arg => {
      parentLocalVars.variables = parentLocalVars.variables ++ List(arg.getId)
      val frameIndex = parentLocalVars.variables.length - 1
      arg.updateMetadata(
        this -->> new FrameInfo.FrameIndex(frameIndex)
      )
    })
  }

  private def analyseExpression(
    expr: IR.Expression,
    localVars: FrameInfo.LocalVariables
  ): IR.Expression = {
    expr match {
      case binding: IR.Expression.Binding =>
        localVars.variables = localVars.variables ++ List(binding.getId)
        val frameIndex = localVars.variables.length - 1
        binding.updateMetadata(
          this -->> new FrameInfo.FrameIndex(frameIndex)
        )
      case _ =>
        expr.mapExpressions((expression: IR.Expression) =>
          analyseExpression(
            expression,
            localVars
          )
        )
    }
  }

  /** @inheritdoc */
  override def updateMetadataInDuplicate[T <: IR](
    sourceIr: T,
    copyOfIr: T
  ): T = {
    ???
  }

  sealed case class Configuration(
    override var shouldWriteToContext: Boolean = false,
    var initialFrameIndexGap: Int              = 0,
    var attachLocalVarMetadata: Boolean        = true
  ) extends IRPass.Configuration
}
