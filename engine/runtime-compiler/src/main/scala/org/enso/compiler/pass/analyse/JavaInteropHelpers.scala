package org.enso.compiler.pass.analyse

import org.enso.compiler.core.CompilerError
import org.enso.compiler.pass.analyse.alias.Graph

/** This gathers helpers that allow to access AliasAnalysis metadata that normally fails to compile in Java.
  *
  * FIXME Later, we should remove this module by unnesting the nested classes, making them easily accessible from Java.
  */
object JavaInteropHelpers {
  def occurrenceAsDef(
    occurrence: Graph.Occurrence
  ): Graph.Occurrence.Def = {
    occurrence match {
      case defOccurrence: Graph.Occurrence.Def => defOccurrence
      case _ =>
        throw new CompilerError(
          "Alias analysis occurrence has unexpected type: " + occurrence.getClass.getCanonicalName
        )
    }
  }
}
