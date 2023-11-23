package org.enso.compiler.core.ir
package module
package scope
package definition

import org.enso.compiler.core.Implicits.{ShowPassData, ToStringHelper}
import org.enso.compiler.core.{IR, Identifier}
import org.enso.compiler.core.IR.randomId

import java.util.UUID

/** A trait representing method definitions in Enso. */
sealed trait Method extends Definition {
  val methodReference: Name.MethodReference
  val body: Expression

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
    * @param body            the body of the method
    * @param location        the source location that the node corresponds to
    * @param passData        the pass metadata associated with this node
    * @param diagnostics     compiler diagnostics for this node
    */
  sealed case class Explicit(
    override val methodReference: Name.MethodReference,
    val bodySeq: Seq[Expression],
    val isStatic: Boolean,
    val isStaticWrapperForInstanceMethod: Boolean,
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage,
    override val diagnostics: DiagnosticStorage
  ) extends Method
      with IRKind.Primitive {
    def this(
      methodReference: Name.MethodReference,
      body: Expression,
      location: Option[IdentifiedLocation],
      passData: MetadataStorage      = MetadataStorage(),
      diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) = {
      this(
        methodReference,
        Seq(body),
        Explicit.computeIsStatic(body),
        Explicit.computeIsStaticWrapperForInstanceMethod(body),
        location,
        passData,
        diagnostics
      );
    }

    var id: UUID @Identifier = randomId
    lazy val body            = bodySeq.head

    /** Creates a copy of `this`.
      *
      * @param methodReference a reference to the method being defined
      * @param body            the body of the method
      * @param location        the source location that the node corresponds to
      * @param passData        the pass metadata associated with this node
      * @param diagnostics     compiler diagnostics for this node
      * @param id              the identifier for the new node
      * @return a copy of `this`, updated with the specified values
      */
    def copy(
      methodReference: Name.MethodReference = methodReference,
      body: Expression                      = body,
      isStatic: Boolean                     = Explicit.computeIsStatic(body),
      isStaticWrapperForInstanceMethod: Boolean =
        Explicit.computeIsStaticWrapperForInstanceMethod(body),
      location: Option[IdentifiedLocation] = location,
      passData: MetadataStorage            = passData,
      diagnostics: DiagnosticStorage       = diagnostics,
      id: UUID @Identifier                 = id
    ): Explicit = {
      val res = Explicit(
        methodReference,
        List(body),
        isStatic,
        isStaticWrapperForInstanceMethod,
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
        location = if (keepLocations) location else None,
        passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy
          else DiagnosticStorage(),
        id = if (keepIdentifiers) id else randomId
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

    /** @inheritdoc */
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
    private def computeIsStatic(body: IR): Boolean = body match {
      case function: Function.Lambda =>
        function.arguments.headOption.map(_.name) match {
          case Some(Name.Self(_, true, _, _)) => true
          case _                              => false
        }
      case _ =>
        true // if it's not a function, it has no arguments, therefore no `self`
    }

    private def computeIsStaticWrapperForInstanceMethod(body: IR): Boolean =
      body match {
        case function: Function.Lambda =>
          function.arguments.map(_.name) match {
            case Name.Self(_, true, _, _) :: Name.Self(
                  _,
                  false,
                  _,
                  _
                ) :: _ =>
              true
            case _ => false
          }
        case _ => false
      }
  }

  /** The definition of a method for a given constructor using sugared
    * syntax.
    *
    * @param methodReference a reference to the method being defined
    * @param arguments       the arguments to the method
    * @param body            the body of the method
    * @param location        the source location that the node corresponds to
    * @param passData        the pass metadata associated with this node
    * @param diagnostics     compiler diagnostics for this node
    */
  sealed case class Binding(
    override val methodReference: Name.MethodReference,
    arguments: List[DefinitionArgument],
    override val body: Expression,
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Method
      with IRKind.Sugar {
    var id: UUID @Identifier = randomId

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
      body: Expression                      = body,
      location: Option[IdentifiedLocation]  = location,
      passData: MetadataStorage             = passData,
      diagnostics: DiagnosticStorage        = diagnostics,
      id: UUID @Identifier                  = id
    ): Binding = {
      val res = Binding(
        methodReference,
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
        passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy
          else DiagnosticStorage(),
        id = if (keepIdentifiers) id else randomId
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

    /** @inheritdoc */
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
    * @param methodReference a reference to the type on which the
    *                        conversion is being defined
    * @param sourceTypeName  the type of the source value for this
    *                        conversion
    * @param body            the body of the method
    * @param location        the source location that the node corresponds to
    * @param passData        the pass metadata associated with this node
    * @param diagnostics     compiler diagnostics for this node
    */
  sealed case class Conversion(
    override val methodReference: Name.MethodReference,
    sourceTypeName: Expression,
    override val body: Expression,
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Method
      with IRKind.Primitive {
    var id: UUID @Identifier = randomId

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
      val res = Conversion(
        methodReference,
        sourceTypeName,
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
        passData = if (keepMetadata) passData.duplicate else MetadataStorage(),
        diagnostics =
          if (keepDiagnostics) diagnostics.copy
          else DiagnosticStorage(),
        id = if (keepIdentifiers) id else randomId
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

    /** @inheritdoc */
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
