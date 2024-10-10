package org.enso.interpreter.instrument.execution

import com.oracle.truffle.api.source.SourceSection
import org.enso.compiler.core.{ExternalID, IR, Identifier}
import org.enso.compiler.core.ir.Location
import org.enso.compiler.core.ir.IdentifiedLocation
import org.enso.interpreter.runtime.Module
import org.enso.text.editing.{model, IndexedSource}

import java.util.UUID

/** Helper methods to convert between the `IR` and source locations, and
  * resolving the expression ids in the source text.
  */
object LocationResolver {

  /** Identifier of the `IR` node with an external id.
    *
    * @param internalId the internal node id
    * @param externalId the external node id
    */
  case class ExpressionId(
    internalid: UUID @Identifier,
    externalId: UUID @ExternalID
  )

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
    location: IdentifiedLocation
  ): Option[ExpressionId] =
    findByIdentifiedLocation(ir, location).flatMap(getExpressionId)

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
    findByLocation(ir, location).flatMap(getExpressionId)

  /** Get the id of the given `IR`.
    *
    * @param ir the `IR` to get id from
    * @return the id of the given `IR`
    */
  def getExpressionId(ir: IR): Option[ExpressionId] =
    ir.getExternalId.map(ExpressionId(ir.getId, _))

  /** Find the expression by its identified location.
    *
    * @param ir the `IR` to get the expression from
    * @param location the expression location
    * @return the expression with the given location
    */
  private def findByIdentifiedLocation(
    ir: IR,
    location: IdentifiedLocation
  ): Option[IR] = {
    IR.preorder(
      ir,
      { ir =>
        if (ir.location.contains(location)) {
          return Some(ir)
        }
      }
    )
    None
  }

  /** Find the expression by its location.
    *
    * @param ir the `IR` to get the expression from
    * @param location the expression location
    * @return the expression with the given location
    */
  private def findByLocation(ir: IR, location: Location): Option[IR] = {
    IR.preorder(
      ir,
      { ir =>
        if (ir.location.map(_.location).contains(location)) {
          return Some(ir)
        }
      }
    )
    None
  }

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
  private def sectionToLocation[A: IndexedSource](
    section: SourceSection,
    source: A
  ): Location = {
    val range = sectionToRange(section)
    new Location(
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
