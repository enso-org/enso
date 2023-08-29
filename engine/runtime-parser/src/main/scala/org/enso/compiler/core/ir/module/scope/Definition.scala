package org.enso.compiler.core.ir.module.scope

import org.enso.compiler.core.IR
import org.enso.compiler.core.ir.{
  DiagnosticStorage,
  Expression,
  IRKind,
  IdentifiedLocation,
  MetadataStorage
}
import org.enso.compiler.core.ir.module.Scope
import org.enso.compiler.core.IR.{
  indentLevel,
  mkIndent,
  randomId,
  DefinitionArgument,
  Identifier,
  Name,
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
    name: IR.Name,
    params: List[IR.DefinitionArgument],
    members: List[Data],
    override val location: Option[IdentifiedLocation],
    override val passData: MetadataStorage      = MetadataStorage(),
    override val diagnostics: DiagnosticStorage = DiagnosticStorage()
  ) extends Definition
      with IRKind.Primitive {
    override protected var id: Identifier = randomId

    def copy(
      name: IR.Name                        = name,
      params: List[IR.DefinitionArgument]  = params,
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
         |IR.Module.Scope.Definition.Type(
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
    name: IR.Name,
    arguments: List[DefinitionArgument],
    annotations: List[IR.Name.GenericAnnotation],
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
      name: IR.Name                                = name,
      arguments: List[DefinitionArgument]          = arguments,
      annotations: List[IR.Name.GenericAnnotation] = annotations,
      location: Option[IdentifiedLocation]         = location,
      passData: MetadataStorage                    = passData,
      diagnostics: DiagnosticStorage               = diagnostics,
      id: Identifier                               = id
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
         |IR.Module.Scope.Definition.Data(
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
    name: IR.Name,
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
      name: IR.Name                        = name,
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
         |IR.Module.Scope.Definition.SugaredType(
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

  /** A trait representing method definitions in Enso. */
  sealed trait Method extends Definition {
    val methodReference: IR.Name.MethodReference
    val body: Expression

    /** @inheritdoc */
    override def setLocation(location: Option[IdentifiedLocation]): Method

    /** @inheritdoc */
    override def mapExpressions(fn: Expression => Expression): Method

    /** @inheritdoc */
    override def duplicate(
      keepLocations: Boolean   = true,
      keepMetadata: Boolean    = true,
      keepDiagnostics: Boolean = true,
      keepIdentifiers: Boolean = false
    ): Method

    /** Get the type name for the method. */
    def typeName: Option[IR.Name] = methodReference.typePointer

    /** Get the name of the method. */
    def methodName: IR.Name = methodReference.methodName
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
      override val methodReference: IR.Name.MethodReference,
      override val body: Expression,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Method
        with IRKind.Primitive {
      override protected var id: Identifier = randomId

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
        methodReference: IR.Name.MethodReference = methodReference,
        body: Expression                         = body,
        location: Option[IdentifiedLocation]     = location,
        passData: MetadataStorage                = passData,
        diagnostics: DiagnosticStorage           = diagnostics,
        id: Identifier                           = id
      ): Explicit = {
        val res = Explicit(
          methodReference,
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
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
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
        fn: Expression => Expression
      ): Explicit = {
        copy(
          methodReference = methodReference.mapExpressions(fn),
          body            = fn(body)
        )
      }

      /** @inheritdoc */
      override def toString: String =
        s"""
           |IR.Module.Scope.Definition.Method.Explicit(
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

      def isStatic: Boolean = body match {
        case function: IR.Function.Lambda =>
          function.arguments.headOption.map(_.name) match {
            case Some(IR.Name.Self(_, true, _, _)) => true
            case _                                 => false
          }
        case _ =>
          true // if it's not a function, it has no arguments, therefore no `self`
      }

      def isStaticWrapperForInstanceMethod: Boolean = body match {
        case function: IR.Function.Lambda =>
          function.arguments.map(_.name) match {
            case IR.Name.Self(_, true, _, _) :: IR.Name.Self(
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
      override val methodReference: IR.Name.MethodReference,
      arguments: List[IR.DefinitionArgument],
      override val body: Expression,
      override val location: Option[IdentifiedLocation],
      override val passData: MetadataStorage      = MetadataStorage(),
      override val diagnostics: DiagnosticStorage = DiagnosticStorage()
    ) extends Method
        with IRKind.Sugar {
      override protected var id: Identifier = randomId

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
        methodReference: IR.Name.MethodReference = methodReference,
        arguments: List[IR.DefinitionArgument]   = arguments,
        body: Expression                         = body,
        location: Option[IdentifiedLocation]     = location,
        passData: MetadataStorage                = passData,
        diagnostics: DiagnosticStorage           = diagnostics,
        id: Identifier                           = id
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
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
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
        fn: Expression => Expression
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
           |IR.Module.Scope.Definition.Method.Binding(
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
      override protected var id: Identifier = randomId

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
        id: Identifier                        = id
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
          passData =
            if (keepMetadata) passData.duplicate else MetadataStorage(),
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
        fn: Expression => Expression
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
           |IR.Module.Scope.Definition.Method.Conversion(
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
}
