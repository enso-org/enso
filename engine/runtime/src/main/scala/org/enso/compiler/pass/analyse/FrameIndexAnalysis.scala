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

/**
 * Scans the IR and gathers all the Truffle frame indexes of all local variables and
 * function arguments.
 * [[org.enso.compiler.codegen.IrToTruffle]] uses this metadata in order to build frame descriptors
 * [[com.oracle.truffle.api.frame.FrameDescriptor]] for all the root nodes.
 * It is implemented as a separate compiler pass so that:
 *   - Persistance: The metadata is attached to IR nodes and serialized alongside the IR.
 *   - Performance: IR is scanned just once.
 *   - ...
 */
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
    /**
     * Holds an index to a Truffle [[com.oracle.truffle.api.frame.FrameDescriptor]].
     * The value is between [[Configuration.initialFrameIndexGap]] and [[Integer.MAX_VALUE]].
     * Attached to function arguments ([[IR.DefinitionArgument]]) and bindings ([[IR.Expression.Binding]])
     * @param index The index to Truffle frame.
     */
    sealed class FrameIndex(
      val index: Int
    ) extends FrameInfo

    /**
     * Contains an array of local variables, in order of their definition.
     * Attached to functions ([[IR.Function]]).
     * Used for testing purposes.
     * @param variables
     * @param parent
     */
    sealed class LocalVariables(
      var variables: List[IR.Identifier] = List(),
      val parent: Option[LocalVariables] = None
    ) extends FrameInfo {
      def createChild(): LocalVariables = {
        new LocalVariables(parent = Some(this))
      }

      def addLocalVar(varId: IR.Identifier): Int = {
        variables = variables ++ List(varId)
        variables.length - 1
      }
    }
    object LocalVariables {
      def createRoot(): LocalVariables = {
        new LocalVariables()
      }
    }
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
    analyseExpression(ir, FrameInfo.LocalVariables.createRoot())
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
                FrameInfo.LocalVariables.createRoot()
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
                FrameInfo.LocalVariables.createRoot()
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
            FrameInfo.LocalVariables.createRoot()
          )
        )
      case _ =>
        val localVars = FrameInfo.LocalVariables.createRoot()
        ir.mapExpressions(expr => analyseExpression(expr, localVars))
    }
  }

  private def analyseFunction(
    function: IR.Function,
    parentLocalVars: FrameInfo.LocalVariables
  ): IR.Function = {
    val currentLocalVars = parentLocalVars.createChild()
    function match {
      case lambda @ IR.Function.Lambda(arguments, body, _, _, _, _) =>
        lambda
          .copy(
            arguments = analyseArgumentDefs(arguments, currentLocalVars),
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
      val frameIndex = parentLocalVars.addLocalVar(arg.getId)
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
        val frameIndex = localVars.addLocalVar(binding.getId)
        binding
          .copy(
            expression = analyseExpression(binding.expression, localVars)
          )
          .updateMetadata(
          this -->> new FrameInfo.FrameIndex(frameIndex)
          )
      case function: IR.Function =>
        analyseFunction(function, localVars)
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
    initialFrameIndexGap: Int              = 0,
    attachLocalVarMetadata: Boolean        = true
  ) extends IRPass.Configuration
}
