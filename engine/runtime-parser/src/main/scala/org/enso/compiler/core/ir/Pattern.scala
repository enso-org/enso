package org.enso.compiler.core.ir

import org.enso.compiler.core.{CompilerError, IR, Identifier}
import org.enso.compiler.core.ir.{Literal => IRLiteral, Name => IRName}
import org.enso.compiler.core.ir.expression.errors
import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}

import java.util.UUID

/** The different types of patterns that can occur in a match. */
trait Pattern extends IR {

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): Pattern

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Pattern

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Pattern
}

object Pattern {

  /** A named pattern.
    *
    * Named patterns take the form of a single identifier (e.g. `a` or `_`).
    * As a result they can be used to represent a catch all pattern (e.g.
    * `_ -> ...` or `a -> ...`).
    *
    * @param name        the name that constitutes the pattern
    * @param location    the source location for this IR node
    * @param passData    any pass metadata associated with the node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Name(
    name: IRName,
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = new MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Pattern
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param name        the name that constitutes the pattern
      * @param location    the source location for this IR node
      * @param passData    any pass metadata associated with the node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the provided values
      */
    def copy(
      name: IRName                         = name,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Name = {
      val res = Name(name, location, passData, diagnostics)
      res.id = id
      res
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Name =
      copy(
        name = name.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Name = {
      copy(name = name.mapExpressions(fn))
    }

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Case.Pattern.Name(
         |name = $name,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Name =
      copy(location = location)

    /** @inheritdoc */
    override def children: List[IR] = List(name)

    /** @inheritdoc */
    override def showCode(indent: Int): String = name.showCode(indent)
  }

  /** A pattern that destructures a constructor application.
    *
    * The first part of the pattern must be a refferent name. The fields of
    * the constructor may be any available kind of pattern.
    *
    * @param constructor the constructor being matched on
    * @param fields      the asserted fields of the constructor
    * @param location    the source location for this IR node
    * @param passData    any pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Constructor(
    constructor: IRName,
    fields: List[Pattern],
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = new MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Pattern
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param constructor the constructor being matched on
      * @param fields      the asserted fields of the constructor
      * @param location    the source location for this IR node
      * @param passData    any pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the new identifier for this node
      * @return a copy of `this`, updated with the provided values
      */
    def copy(
      constructor: IRName                  = constructor,
      fields: List[Pattern]                = fields,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Constructor = {
      val res =
        Constructor(constructor, fields, location, passData, diagnostics)
      res.id = id
      res
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Constructor =
      copy(
        constructor = constructor.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        fields = fields.map(
          _.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          )
        ),
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else null
      )

    /** Checks if the constructor pattern has been desugared.
      *
      * A constructor pattern has been desugared if all of its fields are
      * [[Pattern.Name]].
      *
      * @return `true` if the pattern has been desugared, `false` otherwise
      */
    def isDesugared: Boolean = {
      fields.forall {
        case _: Pattern.Name        => true
        case _: Pattern.Constructor => false
        case _: Pattern.Literal     => true
        case _: Pattern.Type        => true
        case _: Pattern.Documentation =>
          throw new CompilerError(
            "Branch documentation should not be present " +
            "inside a constructor pattern."
          )
        case _: errors.Pattern => true
      }
    }

    /** Gets the patterns fields as [[Pattern.Name]] if they are.
      *
      * @return the fields from `this`
      */
    def fieldsAsNamed: List[Option[Pattern.Name]] = {
      fields.map {
        case f: Name => Some(f)
        case _       => None
      }
    }

    /** Unsafely gets the pattern's fields as if they are [[Pattern.Name]].
      *
      * @return the fields from `this`
      */
    def unsafeFieldsAsNamed: List[Pattern.Name] = {
      fieldsAsNamed.map(_.get)
    }

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Constructor =
      copy(
        constructor = constructor.mapExpressions(fn),
        fields      = fields.map(_.mapExpressions(fn))
      )

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Case.Pattern.Constructor(
         |constructor = $constructor,
         |fields = $fields,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): Constructor = copy(location = location)

    /** @inheritdoc */
    override def children: List[IR] = constructor :: fields

    /** @inheritdoc */
    override def showCode(indent: Int): String = {
      val fieldsStr =
        fields.map(f => s"(${f.showCode(indent)})").mkString(" ")

      s"${constructor.name} $fieldsStr"
    }
  }

  /** A literal pattern.
    *
    * A literal pattern matches on constants.
    *
    * @param literal     the literal representing the pattern
    * @param location    the source location for this IR node
    * @param passData    any pass metadata associated with the node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Literal(
    literal: IRLiteral,
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = new MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Pattern
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param literal     the literal representing the pattern
      * @param location    the source location for this IR node
      * @param passData    any pass metadata associated with the node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the provided values
      */
    def copy(
      literal: IRLiteral                   = literal,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Literal = {
      val res = Literal(literal, location, passData, diagnostics)
      res.id = id
      res
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Literal =
      copy(
        literal = literal.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Literal = {
      copy(literal = literal.mapExpressions(fn))
    }

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Case.Pattern.Literal(
         |literal = $literal,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Literal =
      copy(location = location)

    /** @inheritdoc */
    override def children: List[IR] = List(literal)

    /** @inheritdoc */
    override def showCode(indent: Int): String = literal.showCode(indent)
  }

  /** A type pattern.
    *
    * A type pattern matches on types. Type pattern is composed of two parts:
    * - a single identifier (e.g. `a` or `_`)
    * - a (potentially fully qualified) type name
    * E.g., `a : Foo -> ...` or `_ : Bar -> ...``
    *
    * @param name        the name of the bound variable, or wildcard
    * @param tpe         the name of the type to match on
    * @param location    the source location for this IR node
    * @param passData    any pass metadata associated with the node
    * @param diagnostics compiler diagnostics for this node
    */
  sealed case class Type(
    name: IRName,
    tpe: IRName,
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = new MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Pattern
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param name        the name of the bound variable, or wildcard
      * @param tpe         the name of the type to match on
      * @param location    the source location for this IR node
      * @param passData    any pass metadata associated with the node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the provided values
      */
    def copy(
      name: IRName                         = name,
      tpe: IRName                          = tpe,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Type = {
      val res = Type(name, tpe, location, passData, diagnostics)
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
        tpe = tpe.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Type = {
      copy(name = name.mapExpressions(fn), tpe = tpe.mapExpressions(fn))
    }

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Case.Pattern.Type(
         |name = $name,
         |tpe = $tpe,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Type =
      copy(location = location)

    /** @inheritdoc */
    override def children: List[IR] = List(name, tpe)

    /** @inheritdoc */
    override def showCode(indent: Int): String =
      s"${name.showCode(indent)} : ${tpe.showCode()}"
  }

  /** A dummy pattern used for storing documentation comments between branches
    * in a pattern match.
    *
    * To store a documentation comment next to a branch, a dummy branch is
    * created with its pattern being an instance of this Doc and expression
    * being empty.
    *
    * @param doc         the documentation entity
    * @param location    the source location that the node corresponds to
    * @param passData    the pass metadata associated with this node
    * @param diagnostics compiler diagnostics for this node
    */
  final case class Documentation(
    doc: String,
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = new MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Pattern
      with LazyId {

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Documentation =
      this

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): Documentation =
      copy(location = location)

    /** Creates a copy of `this`.
      *
      * @param doc         the documentation entity
      * @param location    the source location for this IR node
      * @param passData    any pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the new identifier for this node
      * @return a copy of `this`, updated with the provided values
      */
    def copy(
      doc: String                          = doc,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Documentation = {
      val res = Documentation(doc, location, passData, diagnostics)
      res.id = id
      res
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean,
      keepMetadata: Boolean,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Documentation =
      copy(
        doc,
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy else DiagnosticStorage(),
        id = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def children: List[IR] = Nil

    /** @inheritdoc */
    override def toString: String =
      s"""
         |Case.Pattern.Documentation(
         |doc = $doc,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def showCode(indent: Int): String = s"## $doc"
  }
}
