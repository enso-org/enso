package org.enso.compiler.core.ir

import org.enso.compiler.core.{ConstantsNames, IR, Identifier}
import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}

import java.util.UUID

/** Enso names. */
trait Name extends Expression with IRKind.Primitive {
  val name: String

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): Name

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Name

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Name

  /** Checks whether a name is a call-site method name.
    *
    * @return `true` if the name was created through a method call
    */
  def isMethod: Boolean = false

}

object Name {

  /** A representation of a method reference of the form `Type_Path.method`.
    *
    * @param typePointer the type name
    * @param methodName the method on `typeName`
    * @param identifiedLocation the source location that the node corresponds to
    * @param passData the pass metadata associated with this node
    */
  final case class MethodReference(
    typePointer: Option[Name],
    methodName: Name,
    identifiedLocation: IdentifiedLocation,
    passData: MetadataStorage = new MetadataStorage()
  ) extends Name
      with IRKind.Sugar
      with LazyDiagnosticStorage
      with LazyId {

    override val name: String = showCode()

    /** Creates a copy of `this`.
      *
      * @param typePointer the type name
      * @param methodName  the method on `typeName`
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      typePointer: Option[Name]            = typePointer,
      methodName: Name                     = methodName,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): MethodReference = {
      if (
        typePointer != this.typePointer
        || methodName != this.methodName
        || location != this.location
        || passData != this.passData
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res =
          MethodReference(
            typePointer,
            methodName,
            location.orNull,
            passData
          )
        res.diagnostics = diagnostics
        res.id          = id
        res
      } else this
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): MethodReference =
      copy(
        typePointer = typePointer.map(
          _.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          )
        ),
        methodName = methodName.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
        id          = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): MethodReference =
      copy(
        typePointer = typePointer.map(_.mapExpressions(fn)),
        methodName  = methodName.mapExpressions(fn)
      )

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): MethodReference = {
      copy(location = location)
    }

    /** String representation. */
    override def toString: String =
      s"""
         |Name.MethodReference(
         |typePointer = $typePointer,
         |methodName = $methodName,
         |location = $location,
         |passData = $passData,
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] =
      typePointer.map(_ :: methodName :: Nil).getOrElse(methodName :: Nil)

    /** @inheritdoc */
    override def showCode(indent: Int): String = {
      val tPointer = typePointer.map(_.showCode(indent) + ".").getOrElse("")
      s"$tPointer${methodName.showCode(indent)}"
    }

    /** Checks whether `this` and `that` reference the same method.
      *
      * @param that the other method reference to check against
      * @return `true`, if `this` and `that` represent the same method,
      *         otherwise `false`
      */
    def isSameReferenceAs(that: MethodReference): Boolean = {
      val sameTypePointer = typePointer
        .map(thisTp => that.typePointer.exists(_.name == thisTp.name))
        .getOrElse(that.typePointer.isEmpty)
      sameTypePointer && (methodName.name == that.methodName.name)
    }
  }

  object MethodReference {

    /** Generates a location for the reference from the segments.
      *
      * @param segments the reference segments
      * @return a location for the method reference
      */
    def genLocation(segments: List[Name]): Option[IdentifiedLocation] = {
      segments.foldLeft(None: Option[IdentifiedLocation])(
        (identLoc, segment) => {
          identLoc.flatMap(loc => {
            Some(
              new IdentifiedLocation(
                new Location(
                  loc.location.start,
                  segment.location
                    .flatMap(l => Some(l.location.end))
                    .getOrElse(loc.location.end)
                )
              )
            )
          })
        }
      )
    }
  }

  /** A representation of a qualified (multi-part) name.
    *
    * @param parts the segments of the name
    * @param identifiedLocation the source location that the node corresponds to
    * @param passData the pass metadata associated with this node
    * @return a copy of `this`, updated with the specified values
    */
  final case class Qualified(
    parts: List[Name],
    identifiedLocation: IdentifiedLocation,
    passData: MetadataStorage = new MetadataStorage()
  ) extends Name
      with IRKind.Primitive
      with LazyDiagnosticStorage
      with LazyId {

    override val name: String = parts.map(_.name).mkString(".")

    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Name = this

    override def setLocation(location: Option[IdentifiedLocation]): Name =
      copy(location = location)

    /** Creates a copy of `this`.
      *
      * @param parts       the segments of the name
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      parts: List[Name]                    = parts,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Qualified = {
      if (
        parts != this.parts
        || location != this.location
        || passData != this.passData
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = Qualified(parts, location.orNull, passData)
        res.diagnostics = diagnostics
        res.id          = id
        res
      } else this
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Qualified =
      copy(
        parts = parts.map(
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
        diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
        id          = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def children: List[IR] = parts

    /** @inheritdoc */
    override def showCode(indent: Int): String = name
  }

  /** Represents occurrences of blank (`_`) expressions.
    *
    * @param identifiedLocation the source location that the node corresponds to.
    * @param passData the pass metadata associated with this node
    */
  sealed case class Blank(
    identifiedLocation: IdentifiedLocation,
    passData: MetadataStorage = new MetadataStorage()
  ) extends Name
      with IRKind.Sugar
      with LazyDiagnosticStorage
      with LazyId {

    override val name: String = "_"

    /** Creates a copy of `this`.
      *
      * @param location    the source location that the node corresponds to.
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Blank = {
      if (
        location != this.location
        || passData != this.passData
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = Blank(location.orNull, passData)
        res.diagnostics = diagnostics
        res.id          = id
        res
      } else this
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Blank =
      copy(
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
        id          = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Blank =
      this

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Blank =
      copy(location = location)

    /** String representation. */
    override def toString: String =
      s"""
         |Name.Blank(
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".stripMargin

    /** @inheritdoc */
    override def children: List[IR] = List()

    /** @inheritdoc */
    override def showCode(indent: Int): String = "_"
  }

  sealed case class Special(
    specialName: Special.Ident,
    override val identifiedLocation: IdentifiedLocation,
    override val passData: MetadataStorage = new MetadataStorage()
  ) extends Name
      with IRKind.Sugar
      with LazyDiagnosticStorage
      with LazyId {
    override val name: String = s"<special::${specialName}>"

    /** Creates a copy of `this`.
      *
      * @param location    the source location that the node corresponds to.
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      specialName: Special.Ident           = specialName,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Special = {
      if (
        specialName != this.specialName
        || location != this.location
        || passData != this.passData
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = Special(specialName, location.orNull, passData)
        res.diagnostics = diagnostics
        res.id          = id
        res
      } else this
    }

    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Special =
      copy(
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
        id          = if (keepIdentifiers) id else null
      )

    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Special =
      this

    override def setLocation(location: Option[IdentifiedLocation]): Special =
      copy(location = location)

    override def toString: String =
      s"""
         |Name.Special(
         |specialName = $specialName,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".stripMargin

    override def children: List[IR] = List()

    override def showCode(indent: Int): String = name
  }

  object Special {
    sealed trait Ident

    case object NewRef extends Ident

    case object ReadRef extends Ident

    case object WriteRef extends Ident

    case object RunThread extends Ident

    case object JoinThread extends Ident
  }

  /** The representation of a literal name.
    *
    * @param name the literal text of the name
    * @param isMethod is this a method call name
    * @param identifiedLocation the source location that the node corresponds to
    * @param originalName the name which this literal has replaced, if any
    * @param passData the pass metadata associated with this node
    */
  sealed case class Literal(
    override val name: String,
    override val isMethod: Boolean,
    override val identifiedLocation: IdentifiedLocation,
    originalName: Option[Name]             = None,
    override val passData: MetadataStorage = new MetadataStorage()
  ) extends Name
      with LazyDiagnosticStorage
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param name        the literal text of the name
      * @param isMethod    is this a method call name
      * @param location    the source location that the node corresponds to
      * @param originalName the name which this literal has replaced, if any
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      name: String                         = name,
      isMethod: Boolean                    = isMethod,
      location: Option[IdentifiedLocation] = location,
      originalName: Option[Name]           = originalName,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Literal = {
      if (
        name != this.name
        || isMethod != this.isMethod
        || location != this.location
        || originalName != this.originalName
        || passData != this.passData
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res =
          Literal(name, isMethod, location.orNull, originalName, passData)
        res.diagnostics = diagnostics
        res.id          = id
        res
      } else this
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Literal =
      copy(
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
        id          = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Literal =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Literal = this

    /** String representation. */
    override def toString: String =
      s"""
         |Name.Literal(
         |name = $name,
         |isMethod = $isMethod,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = List()

    /** @inheritdoc */
    override def showCode(indent: Int): String = name
  }

  /** Base trait for annotations. */
  sealed trait Annotation extends Name with module.scope.Definition {

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Annotation

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Annotation

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Annotation
  }

  /** The representation of builtin annotation.
    *
    * @param name the annotation text of the name
    * @param identifiedLocation the source location that the node corresponds to
    * @param passData the pass metadata associated with this node
    */
  sealed case class BuiltinAnnotation(
    override val name: String,
    override val identifiedLocation: IdentifiedLocation,
    override val passData: MetadataStorage = new MetadataStorage()
  ) extends Annotation
      with IRKind.Primitive
      with LazyDiagnosticStorage
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param name        the annotation text of the name
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      name: String                         = name,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): BuiltinAnnotation = {
      if (
        name != this.name
        || location != this.location
        || passData != this.passData
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = BuiltinAnnotation(name, location.orNull, passData)
        res.diagnostics = diagnostics
        res.id          = id
        res
      } else this
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): BuiltinAnnotation =
      copy(
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
        id          = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): BuiltinAnnotation =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): BuiltinAnnotation =
      this

    /** String representation. */
    override def toString: String =
      s"""
         |Name.BuiltinAnnotation(
         |name = $name,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = List()

    /** @inheritdoc */
    override def showCode(indent: Int): String = s"@$name"
  }

  /** Common annotations of form `@name expression`.
    *
    * @param name the annotation text of the name
    * @param expression the annotation expression
    * @param identifiedLocation the source location that the node corresponds to
    * @param passData the pass metadata associated with this node
    */
  sealed case class GenericAnnotation(
    override val name: String,
    expression: Expression,
    override val identifiedLocation: IdentifiedLocation,
    override val passData: MetadataStorage = new MetadataStorage()
  ) extends Annotation
      with LazyDiagnosticStorage
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param name        the annotation text of the name
      * @param expression  the annotation expression
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      name: String                         = name,
      expression: Expression               = expression,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): GenericAnnotation = {
      if (
        name != this.name
        || expression != this.expression
        || location != this.location
        || passData != this.passData
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res =
          GenericAnnotation(name, expression, location.orNull, passData)
        res.diagnostics = diagnostics
        res.id          = id
        res
      } else this
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): GenericAnnotation =
      copy(
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
        id          = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): GenericAnnotation =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): GenericAnnotation =
      copy(expression = fn(expression))

    /** String representation. */
    override def toString: String =
      s"""
         |Name.GenericAnnotation(
         |name = $name,
         |expression = $expression,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = List(expression)

    /** @inheritdoc */
    override def showCode(indent: Int): String =
      s"@$name ${expression.showCode(indent)}"
  }

  /** A representation of the name `self`, used to refer to the current type.
    *
    * @param identifiedLocation the source location that the node corresponds to
    * @param synthetic the flag indicating that the name was generated
    * @param passData the pass metadata associated with this node
    */
  sealed case class Self(
    override val identifiedLocation: IdentifiedLocation,
    synthetic: Boolean                     = false,
    override val passData: MetadataStorage = new MetadataStorage()
  ) extends Name
      with LazyDiagnosticStorage
      with LazyId {

    /** Create a [[Self]] object.
      *
      * @param location the source location that the node corresponds to
      * @param synthetic the flag indicating that the name was generated
      * @param passData the pass metadata associated with this node
      * @param diagnostics the compiler diagnostics
      */
    def this(
      location: Option[IdentifiedLocation],
      synthetic: Boolean,
      passData: MetadataStorage,
      diagnostics: DiagnosticStorage
    ) = {
      this(location.orNull, synthetic, passData)
      this.diagnostics = diagnostics
    }

    override val name: String = ConstantsNames.SELF_ARGUMENT

    /** Creates a copy of `self`.
      *
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      synthetic: Boolean                   = synthetic,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Self = {
      if (
        synthetic != this.synthetic
        || location != this.location
        || passData != this.passData
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = Self(location.orNull, synthetic, passData)
        res.diagnostics = diagnostics
        res.id          = id
        res
      } else this
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Self =
      copy(
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
        id          = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Self =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Self = this

    /** String representation. */
    override def toString: String =
      s"""
         |Name.Self(
         |location = $location,
         |synthetic = $synthetic,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = List()

    /** @inheritdoc */
    override def showCode(indent: Int): String = name
  }

  /** A representation of the name `Self`, used to refer to the current type.
    *
    * @param identifiedLocation the source location that the node corresponds to
    * @param passData the pass metadata associated with this node
    */
  sealed case class SelfType(
    identifiedLocation: IdentifiedLocation,
    passData: MetadataStorage = new MetadataStorage()
  ) extends Name
      with LazyDiagnosticStorage
      with LazyId {

    /** Create a [[SelfType]] object.
      *
      * @param location the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics the compiler diagnostics
      */
    def this(
      location: Option[IdentifiedLocation],
      passData: MetadataStorage,
      diagnostics: DiagnosticStorage
    ) = {
      this(location.orNull, passData)
      this.diagnostics = diagnostics
    }

    override val name: String = ConstantsNames.SELF_TYPE_ARGUMENT

    /** Creates a copy of `Self`.
      *
      * @param location    the source location that the node corresponds to
      * @param passData    the pass metadata associated with this node
      * @param diagnostics compiler diagnostics for this node
      * @param id          the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): SelfType = {
      if (
        location != this.location
        || passData != this.passData
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = SelfType(location.orNull, passData)
        res.diagnostics = diagnostics
        res.id          = id
        res
      } else this
    }

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): SelfType =
      copy(
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
        id          = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): SelfType =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): SelfType = this

    /** String representation. */
    override def toString: String =
      s"""
         |Name.SelfType(
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = List()

    /** @inheritdoc */
    override def showCode(indent: Int): String = name
  }
}
