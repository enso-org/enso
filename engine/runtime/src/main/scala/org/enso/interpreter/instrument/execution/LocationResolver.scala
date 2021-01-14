package org.enso.interpreter.instrument.execution

import com.oracle.truffle.api.source.SourceSection
import org.enso.compiler.core.IR
import org.enso.interpreter.runtime.Module
import org.enso.syntax.text.Location
import org.enso.text.editing.{model, IndexedSource}

object LocationResolver {

  case class ExpressionId(internalId: IR.Identifier, externalId: IR.ExternalId)

  def getExpressionId(section: SourceSection)(implicit
    ctx: RuntimeContext
  ): Option[ExpressionId] = {
    val moduleName = section.getSource.getName
    val moduleOpt  = ctx.executionService.getContext.findModule(moduleName)
    if (moduleOpt.isEmpty) None
    else {
      val module   = moduleOpt.get()
      val location = sectionToLocation(section, module.getLiteralSource)
      getExpressionId(module.getIr, location)
    }
  }

  def getExpressionId(
    section: SourceSection,
    module: Module
  ): Option[ExpressionId] = {
    val location = sectionToLocation(section, module.getLiteralSource)
    getExpressionId(module.getIr, location)
  }

  def getExpressionId(
    ir: IR,
    location: IR.IdentifiedLocation
  ): Option[ExpressionId] =
    ir.preorder
      .find(_.location.contains(location))
      .flatMap(getExpressionId)

  def getExpressionId(
    ir: IR,
    location: Location
  ): Option[ExpressionId] =
    ir.preorder
      .find(_.location.map(_.location).contains(location))
      .flatMap(getExpressionId)

  def getExpressionId(ir: IR): Option[ExpressionId] =
    ir.getExternalId.map(ExpressionId(ir.getId, _))

  /** Convert truffle source section to the range of text.
    *
    * @param section the source section
    * @return the corresponding text range in the source file
    */
  def sectionToRange(section: SourceSection): model.Range =
    model.Range(
      model.Position(section.getStartLine - 1, section.getStartColumn - 1),
      model.Position(section.getEndLine - 1, section.getEndColumn)
    )

  /** Convert truffle source section to the IR location.
    *
    * @param section the source section
    * @param source the source text
    * @return location of the source section within the source
    */
  def sectionToLocation[A: IndexedSource](
    section: SourceSection,
    source: A
  ): Location = {
    val range = sectionToRange(section)
    Location(
      IndexedSource[A].toIndex(range.start, source),
      IndexedSource[A].toIndex(range.end, source)
    )
  }

  /** Convert the IR location to the text range in the source file.
    *
    * @param location the location of IR node
    * @param source the source text
    * @return the corresponding text range in the source file
    */
  def locationToRange[A: IndexedSource](
    location: Location,
    source: A
  ): model.Range =
    model.Range(
      IndexedSource[A].toPosition(location.start, source),
      IndexedSource[A].toPosition(location.end, source)
    )
}
