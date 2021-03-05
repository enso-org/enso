package org.enso.interpreter.instrument.execution
import com.oracle.truffle.api.instrumentation.SourceSectionFilter
import com.oracle.truffle.api.instrumentation.SourceSectionFilter.IndexRange
import com.oracle.truffle.api.source.SourceSection
import org.enso.compiler.core.IR
import org.enso.syntax.text.Location

case class LocationFilter(
  include: Vector[SourceSectionFilter.IndexRange],
  exclude: Vector[SourceSectionFilter.IndexRange]
) {

  def getInclude: Array[SourceSectionFilter.IndexRange] =
    include.toArray

  def getExclude: Array[SourceSectionFilter.IndexRange] =
    exclude.toArray
}

object LocationFilter {

  def addExcludingFunctions(ir: IR, span: SourceSection): LocationFilter = {
    val location   = Location(span.getCharIndex, span.getCharEndIndex)
    val targetNode = LocationResolver.findByLocation(ir, location)
    val include    = targetNode.toVector.flatMap(toIndexRange)
    val exclude    = targetNode.toVector.flatMap(functionLocations)

    val lf = new LocationFilter(include, exclude)
    println(lf)
    lf
  }

  def functionLocations(ir: IR): Seq[SourceSectionFilter.IndexRange] =
    ir match {
      case mod: IR.Module =>
        mod.bindings.flatMap(analyseModuleDefinition)
      case method: IR.Module.Scope.Definition.Method =>
        analyseModuleDefinition(method)
      case function: IR.Function =>
        analyseBody(function.body)
      case expression: IR.Expression =>
        expression.preorder.flatMap(analyseExpression)
      case _ =>
        Seq()
    }

  private def analyseModuleDefinition(
    binding: IR.Module.Scope.Definition
  ): Seq[SourceSectionFilter.IndexRange] = {
    binding match {
      case IR.Module.Scope.Definition.Method.Explicit(_, body, _, _, _) =>
        analyseBody(body)
      case _ =>
        Seq()
    }
  }

  @scala.annotation.tailrec
  private def analyseBody(
    expression: IR.Expression
  ): Seq[SourceSectionFilter.IndexRange] =
    expression match {
      case function: IR.Function =>
        analyseBody(function.body)
      case expr =>
        expr.preorder.flatMap(analyseExpression)
    }

  private def analyseExpression(
    expression: IR
  ): Option[SourceSectionFilter.IndexRange] =
    expression match {
      case function: IR.Function => toIndexRange(function)
      case _                     => None
    }

  private def toIndexRange(ir: IR): Option[SourceSectionFilter.IndexRange] =
    ir.location.map(_.location).map(toIndexRange)

  private def toIndexRange(location: Location): SourceSectionFilter.IndexRange =
    IndexRange.between(location.start, location.end)
}
