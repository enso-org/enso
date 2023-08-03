package org.enso.compiler.refactoring

import org.enso.compiler.core.IR
import org.enso.compiler.pass.analyse.DataflowAnalysis
import org.enso.compiler.pass.analyse.DataflowAnalysis.DependencyInfo

trait IRUtils {

  def findByExternalId(ir: IR, externalId: IR.ExternalId): Option[IR] = {
    ir.preorder.find(_.getExternalId.contains(externalId))
  }

  def findById(ir: IR, id: IR.Identifier): Option[IR] = {
    ir.preorder.find(_.getId == id)
  }

  def findUsages(ir: IR, literal: IR.Name.Literal): Option[Set[IR]] = {
    for {
      metadata <- ir.getMetadata(DataflowAnalysis)
      key = DependencyInfo.Type.Static(literal.getId, literal.getExternalId)
      dependents <- metadata.dependents.get(key)
    } yield {
      dependents
        .flatMap {
          case _: DataflowAnalysis.DependencyInfo.Type.Dynamic =>
            None
          case DataflowAnalysis.DependencyInfo.Type.Static(id, _) =>
            IRUtils.findById(ir, id)
        }
        .collect {
          case usage: IR.Name.Literal if usage.name == literal.name => usage
        }
    }
  }
}

object IRUtils extends IRUtils
