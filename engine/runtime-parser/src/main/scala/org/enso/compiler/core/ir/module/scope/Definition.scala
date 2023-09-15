package org.enso.compiler.core.ir.module.scope

import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.{
  DefinitionArgument,
  DiagnosticStorage,
  Expression,
  IRKind,
  IdentifiedLocation,
  MetadataStorage,
  Name
}
import org.enso.compiler.core.ir.module.Scope
import org.enso.compiler.core.IR.{
  indentLevel,
  mkIndent,
  randomId,
  Identifier,
  ToStringHelper
}

/** A representation of top-level definitions. */
trait Definition extends Scope {

  /** @inheritdoc */
  override def mapExpressions(fn: Expression => Expression): Definition

  /** @inheritdoc */
  override def setLocation(
    location: Option[IdentifiedLocation]
  ): Definition

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Definition
}

object Definition {

  /** The definition of a union type and its members.
    *
    * NB: this should probably be removed once we propagate the union
    * types logic through the runtime and implement statics – the whole
    * notion of desugaring complex type definitions becomes obsolete then.
    *
    * @param name        the name of the union
    * @param members     the members of this union
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Type(
    name: Name,
    params: List[DefinitionArgument],
    members: List[Data],
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Definition
      with IRKind.Primitive {
    override protected var id: Identifier = randomId

    def copy(
      name: Name                           = name,
      params: List[DefinitionArgument]     = params,
      members: List[Data]                  = members,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: Identifier                       = id
    ): Type = {
      val res =
        Type(name, params, members, location, passData, diagnostics)
      res.id = id
      res
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Type =
      copy(
        name = name.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        members = members.map(
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
    ): Type =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(fn: Expression => Expression): Type =
      copy(
        params  = params.map(_.mapExpressions(fn)),
        members = members.map(_.mapExpressions(fn))
      )

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Module.Scope.Definition.Type(
         |name = $name,
         |params = $params,
         |members = $members,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = name :: (params :++ members)

    /** @inheritdoc */
    override def showCode(indent: Int): String = {
      val fields = members.map(_.showCode(indent)).mkString(" | ")

      s"type ${name.showCode(indent)} = $fields"
    }
  }

  /** The definition of an atom constructor and its associated arguments.
    *
    * @param name        the name of the atom
    * @param arguments   the arguments to the atom constructor
    * @param annotations the list of annotations
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Data(
    name: Name,
    arguments: List[DefinitionArgument],
    annotations: List[Name.GenericAnnotation],
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends IR
      with IRKind.Primitive {
    override protected var id: Identifier = randomId

    /** Creates a copy of `this`.
      *
      * @param name        the name of the atom
      * @param arguments   the arguments to the atom constructor
      * @param annotations the list of annotations
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      name: Name                                = name,
      arguments: List[DefinitionArgument]       = arguments,
      annotations: List[Name.GenericAnnotation] = annotations,
      location: Option[IdentifiedLocation]      = location,
      passData: MetadataStorage                 = passData,
      diagnostics: DiagnosticStorage            = diagnostics,
      id: Identifier                            = id
    ): Data = {
      val res = Data(
        name,
        arguments,
        annotations,
        location,
        passData,
        diagnostics
      )
      res.id = id
      res
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Data =
      copy(
        name = name.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        arguments = arguments.map(
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
    override def setLocation(location: Option[IdentifiedLocation]): Data =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(fn: Expression => Expression): Data = {
      copy(
        name        = name.mapExpressions(fn),
        arguments   = arguments.map(_.mapExpressions(fn)),
        annotations = annotations.map(_.mapExpressions(fn))
      )
    }

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Module.Scope.Definition.Data(
         |name = $name,
         |arguments = $arguments,
         |annotations = $annotations,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = name :: arguments ::: annotations

    /** @inheritdoc */
    override def showCode(indent: Int): String = {
      val fields = arguments.map(_.showCode(indent)).mkString(" ")

      s"type ${name.showCode(indent)} $fields"
    }
  }

  /** The definition of a complex type definition that may contain
    * multiple atom and method definitions.
    *
    * @param name        the name of the complex type
    * @param arguments   the (type) arguments to the complex type
    * @param body        the body of the complex type
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class SugaredType(
    name: Name,
    arguments: List[DefinitionArgument],
    body: List[IR],
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Definition
      with IRKind.Sugar {
    override protected var id: Identifier = randomId

    /** Creates a copy of `this`.
      *
      * @param name        the name of the complex type
      * @param arguments   the (type) arguments to the complex type
      * @param body        the body of the complex type
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      name: Name                           = name,
      arguments: List[DefinitionArgument]  = arguments,
      body: List[IR]                       = body,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: Identifier                       = id
    ): SugaredType = {
      val res = SugaredType(
        name,
        arguments,
        body,
        location,
        passData,
        diagnostics
      )
      res.id = id
      res
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): SugaredType =
      copy(
        name = name.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        arguments = arguments.map(
          _.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          )
        ),
        body = body.map(
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
    override def mapExpressions(
      fn: Expression => Expression
    ): SugaredType =
      copy(body = body.map(_.mapExpressions(fn)))

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): SugaredType = copy(location = location)

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Module.Scope.Definition.SugaredType(
         |name = $name,
         |arguments = $arguments,
         |body = $body,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = (name :: arguments) ::: body

    /** @inheritdoc */
    override def showCode(indent: Int): String = {
      val headerArgs = arguments.map(_.showCode(indent)).mkString(" ")
      val header     = s"type ${name.name} $headerArgs"
      val newIndent  = indent + indentLevel
      val bodyStr = body
        .map(mkIndent(newIndent) + _.showCode(newIndent))
        .mkString("\n\n")

      s"$header\n$bodyStr"
    }
  }
}
