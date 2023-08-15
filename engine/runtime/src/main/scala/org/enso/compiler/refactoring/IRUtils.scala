package org.enso.compiler.refactoring

import org.enso.compiler.core.IR
import org.enso.compiler.data.BindingsMap
import org.enso.compiler.pass.analyse.DataflowAnalysis
import org.enso.compiler.pass.resolve.MethodCalls
import org.enso.pkg.QualifiedName

trait IRUtils {

  /** Find the node by external id.
    *
    * @param ir the syntax tree
    * @param externalId the external id to look for
    * @return the first node with the given external id in `ir`
    */
  def findByExternalId(ir: IR, externalId: IR.ExternalId): Option[IR] = {
    ir.preorder.find(_.getExternalId.contains(externalId))
  }

  /** Find usages of a local defined in the body block.
    *
    * @param ir the syntax tree
    * @param literal the literal name of the local
    * @return the list of usages of the given literal in the `ir`
    */
  def findLocalUsages(
    ir: IR,
    literal: IR.Name.Literal
  ): Option[Set[IR.Name.Literal]] = {
    for {
      usages <- findStaticUsages(ir, literal)
    } yield {
      usages.collect {
        case usage: IR.Name.Literal if usage.name == literal.name => usage
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
    node: IR.Name
  ): Option[Set[IR.Name.Literal]] =
    for {
      usages <- findDynamicUsages(ir, node)
    } yield {
      usages
        .collect {
          case usage: IR.Name.Literal
              if usage.isMethod && usage.name == node.name =>
            usage
        }
        .flatMap { symbol =>
          symbol.getMetadata(MethodCalls).flatMap { resolution =>
            resolution.target match {
              case BindingsMap.ResolvedMethod(module, _)
                  if module.getName == moduleName =>
                Some(symbol)
              case _ =>
                None
            }
          }
        }
    }

  /** Find usages of a static dependency in the [[DataflowAnalysis]] metadata.
    *
    * @param ir the syntax tree
    * @param literal the name to look for
    * @return the list of usages of the given name in the `ir`
    */
  private def findStaticUsages(
    ir: IR,
    literal: IR.Name.Literal
  ): Option[Set[IR]] = {
    for {
      metadata <- ir.getMetadata(DataflowAnalysis)
      key = DataflowAnalysis.DependencyInfo.Type
        .Static(literal.getId, literal.getExternalId)
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
    * @param node the name to look for
    * @return the list of usages of the given name in the `ir`
    */
  private def findDynamicUsages(
    ir: IR,
    node: IR.Name
  ): Option[Set[IR]] = {
    for {
      metadata <- ir.getMetadata(DataflowAnalysis)
      key = DataflowAnalysis.DependencyInfo.Type.Dynamic(node.name, None)
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
  private def findById(ir: IR, id: IR.Identifier): Option[IR] = {
    ir.preorder.find(_.getId == id)
  }
}

object IRUtils extends IRUtils
