package org.enso.compiler.core.ir
package module
package scope
package definition

import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}
import org.enso.compiler.core.{IR, Identifier}
import org.enso.persist.Persistance

import java.util.UUID

/** A trait representing method definitions in Enso. */
sealed trait Method extends Definition {
  val methodReference: Name.MethodReference
  val body: Expression
  val isPrivate: Boolean

  /** @inheritdoc */
  override def setLocation(location: Option[IdentifiedLocation]): Method

  /** @inheritdoc */
  override def mapExpressions(
    fn: java.util.function.Function[Expression, Expression]
  ): Method

  /** @inheritdoc */
  override def duplicate(
    keepLocations: Boolean   = true,
    keepMetadata: Boolean    = true,
    keepDiagnostics: Boolean = true,
    keepIdentifiers: Boolean = false
  ): Method

  /** Get the type name for the method. */
  def typeName: Option[Name] = methodReference.typePointer

  /** Get the name of the method. */
  def methodName: Name = methodReference.methodName
}

object Method {

  /** The definition of a method for a given constructor.
    *
    * @param methodReference a reference to the method being defined
    * @param bodyReference the body of the method
    * @param isStatic true if this method is static, false otherwise
    * @param isPrivate true if this method is private, false otherwise
    * @param isStaticWrapperForInstanceMethod true if this method represents a static wrapper for instance method, false otherwise
    * @param identifiedLocation the source location that the node corresponds to
    * @param passData the pass metadata associated with this node
    */
  sealed case class Explicit(
    methodReference: Name.MethodReference,
    bodyReference: Persistance.Reference[Expression],
    isStatic: Boolean,
    override val isPrivate: Boolean,
    isStaticWrapperForInstanceMethod: Boolean,
    override val identifiedLocation: IdentifiedLocation,
    override val passData: MetadataStorage
  ) extends Method
      with IRKind.Primitive
      with LazyDiagnosticStorage
      with LazyId {

    /** Create an [[Explicit]] object from [[Method.Binding]].
      *
      * @param ir the method binding IR
      * @param body the method body expression
      */
    def this(ir: Method.Binding, body: Expression) = {
      this(
        ir.methodReference,
        Persistance.Reference.of(body, false),
        Explicit.computeIsStatic(body),
        ir.isPrivate,
        Explicit.computeIsStaticWrapperForInstanceMethod(body),
        ir.identifiedLocation,
        ir.passData
      )
      diagnostics = ir.diagnostics
    }

    lazy val body: Expression = bodyReference.get(classOf[Expression])

    /** Creates a copy of `this`.
      *
      * @param methodReference a reference to the method being defined
      * @param body            the body of the method
      * @param isStatic        true if this method is static, false otherwise
      * @param isPrivate       true if this method is private, false otherwise
      * @param isStaticWrapperForInstanceMethod true if this method represents a static wrapper for instance method, false otherwise
      * @param location        the source location that the node corresponds to
      * @param passData        the pass metadata associated with this node
      * @param diagnostics     compiler diagnostics for this node
      * @param id              the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      methodReference: Name.MethodReference = methodReference,
      body: Expression                      = body,
      isStatic: Boolean                     = isStatic,
      isPrivate: Boolean                    = isPrivate,
      isStaticWrapperForInstanceMethod: Boolean =
        isStaticWrapperForInstanceMethod,
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Explicit = {
      if (
        methodReference != this.methodReference
        || body != this.body
        || isStatic != this.isStatic
        || isPrivate != this.isPrivate
        || isStaticWrapperForInstanceMethod != this.isStaticWrapperForInstanceMethod
        || location != this.location
        || (passData ne this.passData)
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = Explicit(
          methodReference,
          Persistance.Reference.of(body, false),
          isStatic,
          isPrivate,
          isStaticWrapperForInstanceMethod,
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
    ): Explicit =
      copy(
        methodReference = methodReference.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        body = body.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        isStatic = Explicit.computeIsStatic(body),
        location = if (keepLocations) location else None,
        passData =
          if (keepMetadata) passData.duplicate else new MetadataStorage(),
        diagnostics = if (keepDiagnostics) diagnosticsCopy else null,
        id          = if (keepIdentifiers) id else null
      )

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): Explicit =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Explicit = {
      copy(
        methodReference = methodReference.mapExpressions(fn),
        body            = fn(body)
      )
    }

    /** String representation. */
    override def toString: String =
      s"""
         |Module.Scope.Definition.Method.Explicit(
         |methodReference = $methodReference,
         |body = $body,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] = List(methodReference, body)

    /** @inheritdoc */
    override def showCode(indent: Int): String = {
      val exprStr = if (body.isInstanceOf[Expression.Block]) {
        s"\n${body.showCode(indent)}"
      } else {
        s"${body.showCode(indent)}"
      }

      s"${methodReference.showCode(indent)} = $exprStr"
    }
  }

  object Explicit {

    def unapply(m: Explicit): Option[
      (
        Name.MethodReference,
        Expression,
        Option[IdentifiedLocation],
        MetadataStorage,
        DiagnosticStorage
      )
    ] = {
      Some((m.methodReference, m.body, m.location, m.passData, m.diagnostics))
    }
    def computeIsStatic(body: IR): Boolean = body match {
      case function: Function.Lambda =>
        function.arguments.headOption.map(_.name) match {
          case Some(self: Name.Self) => self.synthetic
          case _                     => false
        }
      case _ =>
        true // if it's not a function, it has no arguments, therefore no `self`
    }

    private def computeIsStaticWrapperForInstanceMethod(body: IR): Boolean =
      body match {
        case function: Function.Lambda =>
          function.arguments.map(_.name) match {
            case (self1: Name.Self) :: (self2: Name.Self) :: _ =>
              self1.synthetic && !self2.synthetic
            case _ => false
          }
        case _ => false
      }
  }

  /** The definition of a method for a given constructor using sugared
    * syntax.
    *
    * @param methodReference a reference to the method being defined
    * @param arguments the arguments to the method
    * @param isPrivate if the method is declared as private (project-private). i.e. with prepended `private` keyword.
    * @param body the body of the method
    * @param identifiedLocation the source location that the node corresponds to
    * @param passData the pass metadata associated with this node
    */
  sealed case class Binding(
    override val methodReference: Name.MethodReference,
    arguments: List[DefinitionArgument],
    isPrivate: Boolean,
    override val body: Expression,
    override val identifiedLocation: IdentifiedLocation,
    override val passData: MetadataStorage = new MetadataStorage()
  ) extends Method
      with IRKind.Sugar
      with LazyDiagnosticStorage
      with LazyId {

    /** Create a [[Binding]] object.
      *
      * @param methodReference a reference to the method being defined
      * @param arguments the arguments to the method
      * @param isPrivate if the method is declared as private (project-private).
      * i.e. with prepended `private` keyword.
      * @param body the body of the method
      * @param identifiedLocation the source location that the node corresponds to
      * @param passData the pass metadata associated with this node
      * @param diagnostics the compiler diagnostics
      */
    def this(
      methodReference: Name.MethodReference,
      arguments: List[DefinitionArgument],
      isPrivate: Boolean,
      body: Expression,
      identifiedLocation: IdentifiedLocation,
      passData: MetadataStorage,
      diagnostics: DiagnosticStorage
    ) = {
      this(
        methodReference,
        arguments,
        isPrivate,
        body,
        identifiedLocation,
        passData
      )
      this.diagnostics = diagnostics
    }

    /** Creates a copy of `this`.
      *
      * @param methodReference a reference to the method being defined
      * @param arguments       the arguments to the method
      * @param body            the body of the method
      * @param location        the source location that the node corresponds to
      * @param passData        the pass metadata associated with this node
      * @param diagnostics     compiler diagnostics for this node
      * @param id              the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      methodReference: Name.MethodReference = methodReference,
      arguments: List[DefinitionArgument]   = arguments,
      isPrivate: Boolean                    = isPrivate,
      body: Expression                      = body,
      location: Option[IdentifiedLocation]  = location,
      passData: MetadataStorage             = passData,
      diagnostics: DiagnosticStorage        = diagnostics,
      id: UUID @Identifier                  = id
    ): Binding = {
      if (
        methodReference != this.methodReference
        || arguments != this.arguments
        || isPrivate != this.isPrivate
        || body != this.body
        || location != this.location
        || (passData ne this.passData)
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = Binding(
          methodReference,
          arguments,
          isPrivate,
          body,
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
    ): Binding =
      copy(
        methodReference = methodReference.duplicate(
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
        body = body.duplicate(
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
    ): Binding =
      copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Binding = {
      copy(
        methodReference = methodReference.mapExpressions(fn),
        arguments       = arguments.map(_.mapExpressions(fn)),
        body            = fn(body)
      )
    }

    /** String representation. */
    override def toString: String =
      s"""
         |Module.Scope.Definition.Method.Binding(
         |methodReference = $methodReference,
         |arguments = $arguments,
         |body = $body,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] =
      (methodReference :: arguments) :+ body

    /** @inheritdoc */
    override def showCode(indent: Int): String = {
      val exprStr = if (body.isInstanceOf[Expression.Block]) {
        s"\n${body.showCode(indent)}"
      } else {
        s"${body.showCode(indent)}"
      }

      val argsStr = arguments.map(_.showCode(indent)).mkString(" ")

      s"${methodReference.showCode(indent)} $argsStr = $exprStr"
    }
  }

  /** A method that represents a conversion from one type to another.
    *
    * @param methodReference a reference to the type on which the conversion is being defined
    * @param sourceTypeName the type of the source value for this conversion
    * @param body the body of the method
    * @param identifiedLocation the source location that the node corresponds to
    * @param passData the pass metadata associated with this node
    */
  sealed case class Conversion(
    override val methodReference: Name.MethodReference,
    sourceTypeName: Expression,
    override val body: Expression,
    override val identifiedLocation: IdentifiedLocation,
    override val passData: MetadataStorage = new MetadataStorage()
  ) extends Method
      with IRKind.Primitive
      with LazyDiagnosticStorage
      with LazyId {

    /** Create a conversion method from [[Method.Binding]].
      *
      * @param ir the method binding IR
      * @param sourceTypeName the type of the source value for this conversion
      * @param body the body of the method
      */
    def this(
      ir: Method.Binding,
      sourceTypeName: Expression,
      body: Expression
    ) = {
      this(
        ir.methodReference,
        sourceTypeName,
        body,
        ir.identifiedLocation,
        ir.passData
      )
      diagnostics = ir.diagnostics
    }

    // Conversion methods cannot be private for now
    override val isPrivate: Boolean = false

    /** Creates a copy of `this`.
      *
      * @param methodReference a reference to the type on which the
      *                        conversion is being defined
      * @param sourceTypeName  the type of the source value for this
      *                        conversion
      * @param body            the body of the method
      * @param location        the source location that the node corresponds to
      * @param passData        the pass metadata associated with this node
      * @param diagnostics     compiler diagnostics for this node
      * @param id              the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      methodReference: Name.MethodReference = methodReference,
      sourceTypeName: Expression            = sourceTypeName,
      body: Expression                      = body,
      location: Option[IdentifiedLocation]  = location,
      passData: MetadataStorage             = passData,
      diagnostics: DiagnosticStorage        = diagnostics,
      id: UUID @Identifier                  = id
    ): Conversion = {
      if (
        methodReference != this.methodReference
        || sourceTypeName != this.sourceTypeName
        || body != this.body
        || location != this.location
        || (passData ne this.passData)
        || diagnostics != this.diagnostics
        || id != this.id
      ) {
        val res = Conversion(
          methodReference,
          sourceTypeName,
          body,
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
      keepLocations: Boolean,
      keepMetadata: Boolean,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Conversion = {
      copy(
        methodReference = methodReference.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        sourceTypeName = sourceTypeName.duplicate(
          keepLocations,
          keepMetadata,
          keepDiagnostics,
          keepIdentifiers
        ),
        body = body.duplicate(
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
    }

    /** @inheritdoc */
    override def setLocation(
      location: Option[IdentifiedLocation]
    ): Conversion = copy(location = location)

    /** @inheritdoc */
    override def mapExpressions(
      fn: java.util.function.Function[Expression, Expression]
    ): Conversion = {
      copy(
        methodReference = methodReference.mapExpressions(fn),
        sourceTypeName  = sourceTypeName.mapExpressions(fn),
        body            = fn(body)
      )
    }

    /** String representation. */
    override def toString: String =
      s"""
         |Module.Scope.Definition.Method.Conversion(
         |methodReference = $methodReference,
         |sourceTypeName = $sourceTypeName,
         |body = $body,
         |location = $location,
         |passData = ${this.showPassData},
         |diagnostics = $diagnostics,
         |id = $id
         |)
         |""".toSingleLine

    /** @inheritdoc */
    override def children: List[IR] =
      List(methodReference, sourceTypeName, body)

    /** @inheritdoc */
    override def showCode(indent: Int): String = {
      val exprStr = if (body.isInstanceOf[Expression.Block]) {
        s"\n${body.showCode(indent)}"
      } else {
        s"${body.showCode(indent)}"
      }

      s"${methodReference.showCode(indent)} = $exprStr"
    }
  }
}
