package org.enso.compiler.core.ir
package expression
package errors

import org.enso.compiler.core.Implicits.ShowPassData
import org.enso.compiler.core.{IR, Identifier}

import java.util.UUID

/** Errors pertaining to the redefinition of language constructs that are
  * not allowed to be.
  */
sealed trait Redefined extends Error {

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): Redefined

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Redefined

  /** @inheritdoc */
  override def location: Option[IdentifiedLocation] =
    Option(identifiedLocation)

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Redefined
}

object Redefined {

  /** An error representing the redefinition or incorrect positioning of
    * the `self` argument to methods.
    *
    * @param identifiedLocation the source location of the error
    * @param passData the pass metadata for this node
    */
  sealed case class SelfArg(
    override val identifiedLocation: IdentifiedLocation,
    override val passData: MetadataStorage = new MetadataStorage()
  ) extends Redefined
      with Diagnostic.Kind.Interactive
      with IRKind.Primitive
      with LazyDiagnosticStorage
      with LazyId {

    /** Creates a copy of `self`.
      *
      * @param location    the source location of the error
      * @param passData    the pass metadata for this node
      * @param diagnostics compiler diagnostics associated with the node
      * @param id          the node's identifier
      * @return a copy of `this`, with the specified values updated
      */
    def copy(
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): SelfArg = {
      if (
        location != this.location
        || (passData ne this.passData)
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = SelfArg(location.orNull, passData)
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
    ): SelfArg =
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
    ): SelfArg = copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): SelfArg =
      this

    /** @inheritdoc */
    override def message(source: IdentifiedLocation => String): String =
      "Methods must have only one definition of the `self` argument, and " +
      "it must be the first."

    override def diagnosticKeys(): Array[Any] = Array()

    /** @inheritdoc */
    override def children: List[IR] = List()

    /** @inheritdoc */
    override def showCode(indent: Int): String = "(Redefined This_Arg)"
  }

  /** An error representing the redefinition of a conversion in a given
    * module
    *
    * @param targetType the name of the atom the conversion was being redefined on
    * @param sourceType the source type for the conversion
    * @param identifiedLocation the location in the source to which this error corresponds
    * @param passData the pass metadata for the error
    */
  sealed case class Conversion(
    targetType: Option[Name],
    sourceType: Name,
    override val identifiedLocation: IdentifiedLocation,
    override val passData: MetadataStorage = new MetadataStorage()
  ) extends Redefined
      with Diagnostic.Kind.Interactive
      with module.scope.Definition
      with IRKind.Primitive
      with LazyDiagnosticStorage
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param targetType  the name of the atom the conversion was being
      *                    redefined on
      * @param sourceType  the source type for the conversion
      * @param location    the location in the source to which this error
      *                    corresponds
      * @param passData    the pass metadata for the error
      * @param diagnostics any diagnostics associated with this error.
      * @param id          the identifier for the node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      targetType: Option[Name]             = targetType,
      sourceType: Name                     = sourceType,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Conversion = {
      if (
        targetType != this.targetType
        || sourceType != this.sourceType
        || location != this.location
        || (passData ne this.passData)
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = Conversion(targetType, sourceType, location.orNull, passData)
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
    ): Conversion =
      copy(
        targetType = targetType.map(
          _.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          )
        ),
        sourceType = sourceType
          .duplicate(
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
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): Conversion =
      copy(location = location)

    /** @inheritdoc */
    override def message(source: (IdentifiedLocation => String)): String =
      s"Ambiguous conversion: ${targetType.map(_.name + ".").getOrElse("")}from " +
      s"${sourceType.showCode()} is defined multiple times in this module."

    override def diagnosticKeys(): Array[Any] = targetType
      .map(_.name :: sourceType.showCode() :: Nil)
      .getOrElse(sourceType.showCode() :: Nil)
      .toArray

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Conversion =
      this

    /** String representation. */
    override def toString: String =
      s"""
         |Error.Redefined.Method(
         |targetType = $targetType,
         |sourceType = $sourceType,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".stripMargin

    /** @inheritdoc */
    override def children: List[IR] =
      targetType
        .map(_ :: sourceType :: Nil)
        .getOrElse(sourceType :: Nil)

    /** @inheritdoc */
    override def showCode(indent: Int): String =
      s"(Redefined (Conversion ${targetType.map(_.showCode() + ".").getOrElse("")}from ${sourceType.showCode()}))"
  }

  /** An error representing the redefinition of a method in a given module.
    * This is also known as a method overload.
    *
    * @param typeName the name of the type the method was being redefined on
    * @param methodName the method name being redefined on `atomName`
    * @param identifiedLocation the location in the source to which this error corresponds
    * @param passData the pass metadata for the error
    */
  sealed case class Method(
    typeName: Option[Name],
    methodName: Name,
    override val identifiedLocation: IdentifiedLocation,
    override val passData: MetadataStorage = new MetadataStorage()
  ) extends Redefined
      with Diagnostic.Kind.Interactive
      with module.scope.Definition
      with IRKind.Primitive
      with LazyDiagnosticStorage
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param typeName    the name of the atom the method was being redefined on
      * @param methodName  the method name being redefined on `atomName`
      * @param location    the location in the source to which this error
      *                    corresponds
      * @param passData    the pass metadata for the error
      * @param diagnostics any diagnostics associated with this error.
      * @param id          the identifier for the node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      typeName: Option[Name]               = typeName,
      methodName: Name                     = methodName,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Method = {
      if (
        typeName != this.typeName
        || methodName != this.methodName
        || location != this.location
        || (passData ne this.passData)
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = Method(typeName, methodName, location.orNull, passData)
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
    ): Method =
      copy(
        typeName = typeName.map(
          _.duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          )
        ),
        methodName = methodName
          .duplicate(
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
    override def setLocation(location: Option[IdentifiedLocation]): Method =
      copy(location = location)

    /** @inheritdoc */
    override def message(source: (IdentifiedLocation => String)): String =
      s"Method overloads are not supported: ${typeName.map(_.name + ".").getOrElse("")}" +
      s"${methodName.name} is defined multiple times in this module."

    override def diagnosticKeys(): Array[Any] = {
      typeName
        .map(_.name :: methodName.name :: Nil)
        .getOrElse(methodName.name :: Nil)
        .toArray
    }

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Method = this

    /** String representation. */
    override def toString: String =
      s"""
         |Error.Redefined.Method(
         |atomName = $typeName,
         |methodName = $methodName,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".stripMargin

    /** @inheritdoc */
    override def children: List[IR] =
      typeName
        .map(_ :: methodName :: Nil)
        .getOrElse(methodName :: Nil)

    /** @inheritdoc */
    override def showCode(indent: Int): String =
      s"(Redefined (Method ${typeName.map(_.showCode() + ".").getOrElse("")}$methodName))"
  }

  /** An error representing the redefinition of a method in a given module,
    * when the module defines a method with the same name as an atom.
    * This is also known as a name clash.
    *
    * @param atomName the name of the atom that clashes with the method
    * @param methodName the method name being redefined in the module
    * @param identifiedLocation the location in the source to which this error corresponds
    * @param passData the pass metadata for the error
    */
  sealed case class MethodClashWithAtom(
    atomName: Name,
    methodName: Name,
    override val identifiedLocation: IdentifiedLocation,
    override val passData: MetadataStorage = new MetadataStorage()
  ) extends Redefined
      with Diagnostic.Kind.Interactive
      with module.scope.Definition
      with IRKind.Primitive
      with LazyDiagnosticStorage
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param atomName    the name of the atom that clashes with the method
      * @param methodName  the method name being redefined in the module
      * @param location    the location in the source to which this error
      *                    corresponds
      * @param passData    the pass metadata for the error
      * @param diagnostics any diagnostics associated with this error.
      * @param id          the identifier for the node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      atomName: Name                       = atomName,
      methodName: Name                     = methodName,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): MethodClashWithAtom = {
      if (
        atomName != this.atomName
        || methodName != this.methodName
        || location != this.location
        || (passData ne this.passData)
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = MethodClashWithAtom(
          atomName,
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
    ): MethodClashWithAtom =
      copy(
        atomName = atomName.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        methodName = methodName
          .duplicate(
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
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): MethodClashWithAtom =
      copy(location = location)

    /** @inheritdoc */
    override def message(source: (IdentifiedLocation => String)): String =
      s"Method definitions with the same name as atoms are not supported. " +
      s"Method ${methodName.name} clashes with the atom ${atomName.name} in this module."

    override def diagnosticKeys(): Array[Any] =
      Array(methodName.name, atomName.name)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): MethodClashWithAtom =
      this

    /** String representation. */
    override def toString: String =
      s"""
         |Error.Redefined.MethodClashWithAtom(
         |atomName = $atomName,
         |methodName = $methodName,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".stripMargin

    /** @inheritdoc */
    override def children: List[IR] = List(atomName, methodName)

    /** @inheritdoc */
    override def showCode(indent: Int): String =
      s"(Redefined (MethodClash $atomName $methodName))"
  }

  /** An error representing the redefinition of an atom in a given module.
    *
    * @param typeName the name of the atom being redefined
    * @param identifiedLocation the location in the source to which this error corresponds
    * @param passData the pass metadata for the error
    */
  sealed case class Type(
    typeName: Name,
    override val identifiedLocation: IdentifiedLocation,
    override val passData: MetadataStorage = new MetadataStorage()
  ) extends Redefined
      with Diagnostic.Kind.Interactive
      with module.scope.Definition
      with IRKind.Primitive
      with LazyDiagnosticStorage
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param atomName    the name of the atom the method was being redefined
      *                    on
      * @param location    the location in the source to which this error
      *                    corresponds
      * @param passData    the pass metadata for the error
      * @param diagnostics any diagnostics associated with this error.
      * @param id          the identifier for the node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      typeName: Name                       = typeName,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Type = {
      if (
        typeName != this.typeName
        || location != this.location
        || (passData ne this.passData)
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = Type(typeName, location.orNull, passData)
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
    ): Type =
      copy(
        typeName = typeName.duplicate(
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
    override def setLocation(location: Option[IdentifiedLocation]): Type =
      copy(location = location)

    /** @inheritdoc */
    override def message(source: (IdentifiedLocation => String)): String =
      s"Redefining atoms is not supported: ${typeName.name} is " +
      s"defined multiple times in this module."

    override def diagnosticKeys(): Array[Any] = Array(typeName.name)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Type = this

    /** String representation. */
    override def toString: String =
      s"""
         |Error.Redefined.Atom(
         |atomName = $typeName,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".stripMargin

    /** @inheritdoc */
    override def children: List[IR] = List(typeName)

    /** @inheritdoc */
    override def showCode(indent: Int): String =
      s"(Redefined (Atom $typeName))"
  }

  /** An error representing the redefinition of an atom in a given module.
    *
    * @param name the name of the atom being redefined
    * @param identifiedLocation the location in the source to which this error corresponds
    * @param passData the pass metadata for the error
    */
  sealed case class Arg(
    name: Name,
    override val identifiedLocation: IdentifiedLocation,
    override val passData: MetadataStorage = new MetadataStorage()
  ) extends Redefined
      with Diagnostic.Kind.Interactive
      with module.scope.Definition
      with IRKind.Primitive
      with LazyDiagnosticStorage
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param name    the name of the atom the method was being redefined
      *                    on
      * @param location    the location in the source to which this error
      *                    corresponds
      * @param passData    the pass metadata for the error
      * @param diagnostics any diagnostics associated with this error.
      * @param id          the identifier for the node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      name: Name                           = name,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID                             = id
    ): Arg = {
      if (
        name != this.name
        || location != this.location
        || (passData ne this.passData)
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = Arg(name, location.orNull, passData)
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
    ): Arg =
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
        diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
        id          = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Arg =
      copy(location = location)

    /** @inheritdoc */
    override def message(source: (IdentifiedLocation => String)): String =
      s"Redefining arguments is not supported: ${name.name} is " +
      s"defined multiple times."

    override def diagnosticKeys(): Array[Any] = Array(name.name)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Arg = this

    /** String representation. */
    override def toString: String =
      s"""
         |Error.Redefined.Arg(
         |name = $name,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".stripMargin

    /** @inheritdoc */
    override def children: List[IR] = List(name)

    /** @inheritdoc */
    override def showCode(indent: Int): String =
      s"(Redefined (Argument $name))"
  }

  /** An error representing the redefinition of a binding in a given scope.
    *
    * While bindings in child scopes are allowed to _shadow_ bindings in
    * parent scopes, a binding cannot be redefined within a given scope.
    *
    * @param invalidBinding the invalid binding
    * @param passData       the pass metadata for the error
    */
  sealed case class Binding(
    invalidBinding: Expression.Binding,
    override val passData: MetadataStorage = new MetadataStorage()
  ) extends Redefined
      with Diagnostic.Kind.Interactive
      with IRKind.Primitive
      with LazyDiagnosticStorage
      with LazyId {

    /** Creates a copy of `this`.
      *
      * @param invalidBinding the invalid binding
      * @param passData       the pass metadata for the error
      * @param diagnostics    compiler diagnostics for this node
      * @param id             the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      invalidBinding: Expression.Binding = invalidBinding,
      passData: MetadataStorage          = passData,
      diagnostics: DiagnosticStorage     = diagnostics,
      id: UUID @Identifier               = id
    ): Binding = {
      if (
        invalidBinding != this.invalidBinding
        || (passData ne this.passData)
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = Binding(invalidBinding, passData)
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
    ): Binding =
      copy(
        invalidBinding = invalidBinding
          .duplicate(
            keepLocations,
            keepMetadata,
            keepDiagnostics,
            keepIdentifiers
          ),
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
        id          = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): Binding = this

    /** @inheritdoc */
    override def identifiedLocation: IdentifiedLocation =
      invalidBinding.identifiedLocation

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Binding =
      this

    /** String representation. */
    override def toString: String =
      s"""
         |Error.Redefined.Binding(
         |invalidBinding = $invalidBinding,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".stripMargin

    /** @inheritdoc */
    override def children: List[IR] = List(invalidBinding)

    /** @inheritdoc */
    override def message(source: (IdentifiedLocation => String)): String =
      s"Variable ${invalidBinding.name.name} is being redefined."

    override def diagnosticKeys(): Array[Any] = Array(
      invalidBinding.name.name
    )

    /** @inheritdoc */
    override def showCode(indent: Int): String =
      s"(Redefined (Binding $invalidBinding))"
  }
}
