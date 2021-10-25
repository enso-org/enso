package org.enso.interpreter.instrument.execution

import com.oracle.truffle.api.source.{Source, SourceSection}
import org.enso.compiler.core.IR
import org.enso.syntax.text.Location

import scala.collection.mutable

/** Contains instrumentable source locations.
  *
  * @param sections the list of source sections to instrument
  */
case class LocationFilter(sections: Set[SourceSection]) {

  /** Get the list of source sections to instrument. */
  def getSections: Array[SourceSection] =
    sections.toArray
}

object LocationFilter {

  private type Builder = mutable.Set[Location]

  /** Create the location filter.
    *
    * @param ir the module ir
    * @param span the source section to instrument
    * @return new location filter
    */
  def create(ir: IR.Module, span: SourceSection): LocationFilter = {
    val location   = Location(span.getCharIndex, span.getCharEndIndex)
    val targetNode = LocationResolver.findByLocation(ir, location).getOrElse(ir)
    val locations  = createLocations(targetNode)

    LocationFilter(locations.map(toSection(span.getSource, _)))
  }

  private def createLocations(ir: IR): Set[Location] = {
    val builder = mutable.Set.empty[Location]
    analyzeEnterable(ir, builder)
    builder.result().toSet
  }

  private def analyzeEnterable(ir: IR, builder: Builder): Unit =
    ir match {
      case module: IR.Module =>
        module.bindings.foreach(analyzeModuleDefinition(_, builder))
      case definition: IR.Module.Scope.Definition =>
        analyzeModuleDefinition(definition, builder)
      case function: IR.Function =>
        analyzeBody(function.body, builder)
      case expression: IR.Expression =>
        analyzeExpression(expression, builder)
      case _ =>
    }

  private def analyzeModuleDefinition(
    binding: IR.Module.Scope.Definition,
    builder: Builder
  ): Unit =
    binding match {
      case method: IR.Module.Scope.Definition.Method.Explicit =>
        analyzeBody(method.body, builder)
      case _ =>
    }

  @scala.annotation.tailrec
  private def analyzeBody(ir: IR.Expression, builder: Builder): Unit =
    ir match {
      case function: IR.Function =>
        analyzeBody(function.body, builder)
      case expression =>
        analyzeExpression(expression, builder)
    }

  private def analyzeExpression(expression: IR, builder: Builder): Unit = {
    @scala.annotation.tailrec
    def go(queue: mutable.Queue[IR]): Unit = {
      if (queue.nonEmpty) {
        val element  = queue.dequeue()
        val location = getLocation(element)

        element match {
          case IR.Expression.Binding(_, function: IR.Function, _, _, _)
              if !isSynthetic(function) =>
            builder ++= getLocation(function)
          case IR.Expression.Binding(_, block: IR.Expression.Block, _, _, _) =>
            builder ++= getLocation(block)
          case arg: IR.CallArgument =>
            queue += arg.value
          case _: IR.Function =>
          case _ =>
            builder ++= location
            queue ++= element.children
        }

        go(queue)
      }
    }

    go(mutable.Queue(expression))
  }

  private def isSynthetic(function: IR.Function): Boolean = {
    val irLocation = getLocation(function)
    irLocation.isDefined && irLocation == getLocation(function.body)
  }

  private def toSection(source: Source, location: Location): SourceSection =
    source.createSection(location.start, location.length)

  private def getLocation(ir: IR): Option[Location] =
    ir.location.map(_.location)
}
