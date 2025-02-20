package org.enso.compiler.refactoring

import org.enso.compiler.core.Implicits.AsMetadata
import org.enso.compiler.core.{ExternalID, IR, Identifier}
import org.enso.compiler.core.ir.{Expression, Name}
import org.enso.compiler.core.ir.expression.Application
import org.enso.compiler.core.ir.module.scope.definition.Method
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.analyse.DataflowAnalysis
import org.enso.compiler.pass.resolve.MethodCalls
import org.enso.pkg.QualifiedName

import java.util.UUID

trait IRUtils {

  /** Find the node by external id.
    *
    * @param ir the syntax tree
    * @param externalId the external id to look for
    * @return the first node with the given external id in `ir`
    */
  def findByExternalId(ir: IR, externalId: UUID @ExternalID): Option[IR] = {
    IR.preorder(
      ir,
      { ir =>
        if (ir.getExternalId.contains(externalId)) {
          return Some(ir)
        }
      }
    )
    None
  }

  /** Find definitions with the provided name.
    *
    * @param ir the IR where to search the definition
    * @param name the definition name to look for
    * @return the list of definitions with the provided name
    */
  def findModuleDefinitions(ir: IR, name: String): Set[IR] = {
    val builder = Set.newBuilder[IR]
    IR.preorder(
      ir,
      {
        case methodExplicit: Method.Explicit
            if methodExplicit.methodName.name == name =>
          builder.addOne(methodExplicit)
        case _ =>
      }
    )
    builder.result()
  }

  /** Find definitions with the provided name.
    *
    * @param scope the IR where to search the definition
    * @param name the definition name to look for
    * @return the list of definitions with the provided name
    */
  def findLocalDefinitions(scope: IR, name: String): Set[IR] = {
    val builder = Set.newBuilder[IR]
    IR.preorder(
      scope,
      {
        case expressionBinding: Expression.Binding
            if expressionBinding.name.name == name =>
          builder.addOne(expressionBinding)
        case _ =>
      }
    )
    builder.result()
  }

  /** Get the [[Expression.Block]] containing the provided expression.
    *
    * @param scope the scope where to look
    * @param expression the expression to look for
    * @return the block containing the provided expression
    */
  def getExpressionBlock(
    scope: IR,
    expression: IR
  ): Option[Expression.Block] = {
    val blocksBuilder = Set.newBuilder[Expression.Block]
    IR.preorder(
      scope,
      {
        case block: Expression.Block => blocksBuilder.addOne(block)
        case _                       =>
      }
    )
    val blocks = blocksBuilder.result()

    blocks.find(block => findById(block, expression.getId).isDefined)
  }

  /** Find usages of a local defined in the body block.
    *
    * @param ir the syntax tree
    * @param literal the literal name of the local
    * @return the list of usages of the given literal in the `ir`
    */
  def findLocalUsages(
    ir: IR,
    literal: Name.Literal
  ): Option[Set[Name.Literal]] = {
    for {
      usages <- findStaticUsages(ir, literal)
    } yield {
      usages.collect {
        case usage: Name.Literal if usage.name == literal.name => usage
      }
    }
  }

  /** Find usages of a method defined on module.
    *
    * @param moduleName the qualified module name
    * @param ir the syntax tree
    * @param node the name of the method
    * @return the list of usages of the given method in the `ir`
    */
  def findModuleMethodUsages(
    moduleName: QualifiedName,
    ir: IR,
    node: Name
  ): Option[Set[Name.Literal]] =
    for {
      usages <- findDynamicUsages(ir, node.name)
    } yield {
      usages.collect {
        case app: Application.Prefix
            if app.function.isInstanceOf[Name.Literal] &&
            app.function.asInstanceOf[Name.Literal].name == node.name =>
          val function = app.function.asInstanceOf[Name.Literal]
          function.getMetadata(MethodCalls) match {
            case Some(resolution) =>
              resolution.target match {
                case BindingsMap.ResolvedModuleMethod(module, _)
                    if module.getName == moduleName =>
                  Some(function)
                case _ =>
                  None
              }
            case None =>
              app.arguments.headOption match {
                case Some(arg) if arg.isSynthetic =>
                  Some(function)
                case _ =>
                  None
              }
          }
      }.flatten
    }

  /** Find usages of a static dependency in the [[DataflowAnalysis]] metadata.
    *
    * @param ir the syntax tree
    * @param literal the name to look for
    * @return the list of usages of the given name in the `ir`
    */
  private def findStaticUsages(
    ir: IR,
    literal: Name.Literal
  ): Option[Set[IR]] = {
    for {
      metadata <- ir.getMetadata(DataflowAnalysis)
      key = DataflowAnalysis.DependencyInfo.Type
        .Static(literal.getId(), literal.getExternalId)
      dependents <- metadata.dependents.get(key)
    } yield {
      dependents
        .flatMap {
          case _: DataflowAnalysis.DependencyInfo.Type.Dynamic =>
            None
          case DataflowAnalysis.DependencyInfo.Type.Static(id, _) =>
            findById(ir, id)
        }
    }
  }

  /** Find usages of a dynamic dependency in the [[DataflowAnalysis]] metadata.
    *
    * @param ir the syntax tree
    * @param name the name to look for
    * @return the list of usages of the given name in the `ir`
    */
  private def findDynamicUsages(
    ir: IR,
    name: String
  ): Option[Set[IR]] = {
    for {
      metadata <- ir.getMetadata(DataflowAnalysis)
      key = DataflowAnalysis.DependencyInfo.Type.Dynamic(name, None)
      dependents <- metadata.dependents.get(key)
    } yield {
      dependents
        .flatMap {
          case _: DataflowAnalysis.DependencyInfo.Type.Dynamic =>
            None
          case DataflowAnalysis.DependencyInfo.Type.Static(id, _) =>
            findById(ir, id)
        }
    }
  }

  /** Find node by id.
    *
    * @param ir the syntax tree
    * @param id the identifier to look for
    * @return the `ir` node with the given identifier
    */
  private def findById(ir: IR, id: UUID @Identifier): Option[IR] = {
    IR.preorder(
      ir,
      { ir =>
        if (ir.getId == id) {
          return Some(ir)
        }
      }
    )
    None
  }
}

object IRUtils extends IRUtils
