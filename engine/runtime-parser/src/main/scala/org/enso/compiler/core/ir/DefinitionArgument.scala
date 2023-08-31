package org.enso.compiler.core.ir

import org.enso.compiler.core.IR
import org.enso.compiler.core.IR.{randomId, Identifier, ToStringHelper}

/** Definition-site arguments in Enso. */
sealed trait DefinitionArgument extends IR {

  /** The name of the argument. */
  val name: Name

  /** The type of the argument */
  val ascribedType: Option[Expression]

  /** The default value of the argument. */
  val defaultValue: Option[Expression]

  /** Whether or not the argument is suspended. */
  val suspended: Boolean

  /** @inheritdoc */
  override def mapExpressions(
    fn: Expression => Expression
  ): DefinitionArgument

  /** @inheritdoc */
  override def setLocation(
    location: Option[IdentifiedLocation]
  ): DefinitionArgument

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): DefinitionArgument

  def withName(ir: Name): DefinitionArgument
}

object DefinitionArgument {

  /** The representation of an argument from a [[Function]] or
    * [[module.scope.Definition.Data]] definition site.
    *
    * To create an ignored argument, the argument name should be an
    * [[Name.Blank]].
    *
    * @param name         the name of the argument
    * @param ascribedType the explicitly ascribed type of the argument, if
    *                     present
    * @param defaultValue the default value of the argument, if present
    * @param suspended    whether or not the argument has its execution suspended
    * @param location     the source location that the node corresponds to
    * @param passData     the pass metadata associated with this node
    * @param diagnostics  compiler diagnostics for this node
    */
  sealed case class Specified(
    override val name: Name,
    override val ascribedType: Option[Expression],
    override val defaultValue: Option[Expression],
    override val suspended: Boolean,
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends DefinitionArgument
      with IRKind.Primitive {
    override protected var id: Identifier = randomId

    /** Creates a copy of `this`.
      *
      * @param name         the name of the argument
      * @param ascribedType the explicitly ascribed type of the argument, if
      *                     present
      * @param defaultValue the default value of the argument, if present
      * @param suspended    whether or not the argument has its execution suspended
      * @param location     the source location that the node corresponds to
      * @param passData     the pass metadata associated with this node
      * @param diagnostics  compiler diagnostics for this node
      * @param id           the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      name: Name                           = name,
      ascribedType: Option[Expression]     = ascribedType,
      defaultValue: Option[Expression]     = defaultValue,
      suspended: Boolean                   = suspended,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: Identifier                       = id
    ): Specified = {
      val res = Specified(
        name,
        ascribedType,
        defaultValue,
        suspended,
        location,
        passData,
        diagnostics
      )
      res.id = id
      res
    }

    override def withName(ir: Name): DefinitionArgument = copy(name = ir)

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Specified =
      copy(
        name = name.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        ascribedType = ascribedType.map(
          _.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          )
        ),
        defaultValue = defaultValue.map(
          _.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          )
        ),
        location = if (keepLocations) location else None,
        passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else randomId
      )

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): Specified = copy(location = location)

    /** @inheritdoc */
    def mapExpressions(fn: Expression => Expression): Specified = {
      copy(
        name         = name.mapExpressions(fn),
        ascribedType = ascribedType.map(fn),
        defaultValue = defaultValue.map(fn)
      )
    }

    /** @inheritdoc */
    override def toString: String =
      s"""
         |DefinitionArgument.Specified(
         |name = $name,
         |ascribedType = $ascribedType,
         |defaultValue = $defaultValue,
         |suspended = $suspended,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] =
      name :: ascribedType.toList ++ defaultValue.toList

    /** @inheritdoc */
    override def showCode(indent: Int): String = {
      val withoutLazy =
        if (defaultValue.isDefined && ascribedType.isDefined) {
          val name        = this.name.showCode(indent)
          val typeExpr    = this.ascribedType.get.showCode(indent)
          val defaultExpr = this.defaultValue.get.showCode(indent)
          s"($name : ($typeExpr) = ($defaultExpr))"
        } else if (defaultValue.isDefined) {
          val name        = this.name.showCode(indent)
          val defaultExpr = this.defaultValue.get.showCode(indent)
          s"($name = $defaultExpr)"
        } else if (ascribedType.isDefined) {
          val name     = this.name.showCode(indent)
          val typeExpr = this.ascribedType.get.showCode(indent)
          s"($name : $typeExpr)"
        } else {
          s"${name.showCode(indent)}"
        }

      if (suspended) {
        s"~$withoutLazy"
      } else {
        withoutLazy
      }
    }
  }
}
