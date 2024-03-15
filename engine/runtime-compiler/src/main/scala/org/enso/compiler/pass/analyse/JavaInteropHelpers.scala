package org.enso.compiler.pass.analyse

import org.enso.compiler.core.{CompilerError, IR}

/** This gathers helpers that allow to access AliasAnalysis metadata that normally fails to compile in Java.
 *
 * FIXME Later, we should remove this module by unnesting the nested classes, making them easily accessible from Java.
 */
object JavaInteropHelpers {
  def getAliasAnalysisOccurrenceMetadata(
    ir: IR
  ): AliasAnalysisInfo.Occurrence = {
    val metadata = ir.passData().get(AliasAnalysis).getOrElse {
      throw new CompilerError("Alias analysis pass data not found")
    }

    metadata match {
      case occurrence: AliasAnalysisInfo.Occurrence => occurrence
      case _ =>
        throw new CompilerError(
          "Alias analysis metadata has unexpected type: " + metadata.getClass.getCanonicalName
        )
    }
  }

  def occurrenceAsDef(
    occurrence: AliasAnalysisGraph.Occurrence
  ): AliasAnalysisGraph.Occurrence.Def = {
    occurrence match {
      case defOccurrence: AliasAnalysisGraph.Occurrence.Def => defOccurrence
      case _ =>
        throw new CompilerError(
          "Alias analysis occurrence has unexpected type: " + occurrence.getClass.getCanonicalName
        )
    }
  }
}
