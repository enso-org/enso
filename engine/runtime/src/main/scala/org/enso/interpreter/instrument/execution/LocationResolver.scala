package org.enso.interpreter.instrument.execution

import com.oracle.truffle.api.source.SourceSection
import org.enso.compiler.core.IR
import org.enso.interpreter.runtime.Module
import org.enso.syntax.text.Location
import org.enso.text.editing.{model, IndexedSource}

/** Helper methods to convert between the `IR` and source locations, and
  * resolving the expression ids in the source text.
  */
object LocationResolver {

  /** Identifier of the `IR` node with an external id.
    *
    * @param internalId the internal node id
    * @param externalId the external node id
    */
  case class ExpressionId(internalId: IR.Identifier, externalId: IR.ExternalId)

  /** Resolve expression id of the given source section.
    *
    * @param section the source section
    * @param ctx the runtime context
    * @return the expression id of the given source section
    */
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

  /** Resolve expression id of the given source section.
    * @param section the source section
    * @param module the module of the section
    * @return the expression id of the given source section
    */
  def getExpressionId(
    section: SourceSection,
    module: Module
  ): Option[ExpressionId] = {
    val location = sectionToLocation(section, module.getLiteralSource)
    getExpressionId(module.getIr, location)
  }

  /** Resolve expression id of the given `IR` location.
    *
    * @param ir the corresponding `IR`
    * @param location the location in the `IR`
    * @return the expression id of the location in the given ir
    */
  def getExpressionId(
    ir: IR,
    location: IR.IdentifiedLocation
  ): Option[ExpressionId] =
    ir.preorder
      .find(_.location.contains(location))
      .flatMap(getExpressionId)

  /** Resolve expression id of the given source location.
    *
    * @param ir the corresponding `IR`
    * @param location the location in the source text
    * @return the expression id of the source location in the given ir
    */
  def getExpressionId(
    ir: IR,
    location: Location
  ): Option[ExpressionId] =
    ir.preorder
      .find(_.location.map(_.location).contains(location))
      .flatMap(getExpressionId)

  /** Get the id of the given `IR`.
    *
    * @param ir the `IR` to get id from
    * @return the id of the given `IR`
    */
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
