package org.enso.languageserver.protocol.binary.factory

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary
import org.enso.languageserver.protocol.binary.{
  SuggestionEntryArgument,
  SuggestionEntryAtom,
  SuggestionEntryFunction,
  SuggestionEntryLocal,
  SuggestionEntryMethod,
  SuggestionEntryScope,
  SuggestionsDatabaseUpdateKind
}
import org.enso.languageserver.runtime.SuggestionsDatabaseEventsApi.SuggestionsDatabaseUpdate
import org.enso.searcher.Suggestion

object SuggestionsDatabaseEventsFactory {

  def create(
    update: SuggestionsDatabaseUpdate
  )(implicit builder: FlatBufferBuilder): Int =
    update match {
      case add: SuggestionsDatabaseUpdate.Add =>
        createSuggestionsDatabaseUpdateKindAdd(add)
      case modify: SuggestionsDatabaseUpdate.Modify =>
        createSuggestionsDatabaseUpdateKindUpdate(modify)
      case remove: SuggestionsDatabaseUpdate.Remove =>
        createSuggestionsDatabaseUpdateKindDelete(remove)
    }

  def createSuggestionsDatabaseUpdateKindAdd(
    add: SuggestionsDatabaseUpdate.Add
  )(implicit builder: FlatBufferBuilder): Int = {
    val argumentsOpt = getSuggestionArguments(add.suggestion).map { args =>
      val arguments = args.map(createSuggestionArgument).toArray
      binary.SuggestionsDatabaseUpdate.createArgumentsVector(builder, arguments)
    }
    val selfTypeOpt = getSelfType(add.suggestion).map { selfType =>
      builder.createString(selfType)
    }
    val documentationOpt = getDocumentation(add.suggestion).map { doc =>
      builder.createString(doc)
    }
    val scopeOpt = getSuggestionScope(add.suggestion).map(createScope)

    binary.SuggestionsDatabaseUpdate
      .startSuggestionsDatabaseUpdate(builder)
    binary.SuggestionsDatabaseUpdate.addId(builder, add.id)
    binary.SuggestionsDatabaseUpdate.addKind(
      builder,
      SuggestionsDatabaseUpdateKind.Add
    )
    binary.SuggestionsDatabaseUpdate.addName(
      builder,
      builder.createString(getSuggestionName(add.suggestion))
    )
    binary.SuggestionsDatabaseUpdate.addReturnType(
      builder,
      builder.createString(getReturnType(add.suggestion))
    )
    argumentsOpt.foreach { args =>
      binary.SuggestionsDatabaseUpdate.addArguments(builder, args)
    }
    selfTypeOpt.foreach { selfType =>
      binary.SuggestionsDatabaseUpdate.addSelfType(builder, selfType)
    }
    documentationOpt.foreach { doc =>
      binary.SuggestionsDatabaseUpdate.addDocumentation(builder, doc)
    }
    scopeOpt.foreach { scope =>
      binary.SuggestionsDatabaseUpdate.addScope(builder, scope)
    }
    binary.SuggestionsDatabaseUpdate.endSuggestionsDatabaseUpdate(builder)
  }

  def createSuggestionsDatabaseUpdateKindUpdate(
    modify: SuggestionsDatabaseUpdate.Modify
  )(implicit builder: FlatBufferBuilder): Int = {
    val nameOpt = modify.name.map(builder.createString)
    val argumentsOpt = modify.arguments.map { args =>
      val arguments = args.map(createSuggestionArgument).toArray
      binary.SuggestionsDatabaseUpdate.createArgumentsVector(builder, arguments)
    }
    val selfTypeOpt      = modify.selfType.map(builder.createString)
    val returnTypeOpt    = modify.returnType.map(builder.createString)
    val documentationOpt = modify.documentation.map(builder.createString)
    val scopeOpt         = modify.scope.map(createScope)

    binary.SuggestionsDatabaseUpdate
      .startSuggestionsDatabaseUpdate(builder)
    binary.SuggestionsDatabaseUpdate.addId(builder, modify.id)
    binary.SuggestionsDatabaseUpdate.addKind(
      builder,
      SuggestionsDatabaseUpdateKind.Update
    )
    nameOpt.foreach { name =>
      binary.SuggestionsDatabaseUpdate.addName(builder, name)
    }
    argumentsOpt.foreach { args =>
      binary.SuggestionsDatabaseUpdate.addArguments(builder, args)
    }
    selfTypeOpt.foreach { selfType =>
      binary.SuggestionsDatabaseUpdate.addSelfType(builder, selfType)
    }
    returnTypeOpt.foreach { returnType =>
      binary.SuggestionsDatabaseUpdate.addReturnType(builder, returnType)
    }
    documentationOpt.foreach { doc =>
      binary.SuggestionsDatabaseUpdate.addDocumentation(builder, doc)
    }
    scopeOpt.foreach { scope =>
      binary.SuggestionsDatabaseUpdate.addScope(builder, scope)
    }
    binary.SuggestionsDatabaseUpdate.endSuggestionsDatabaseUpdate(builder)
  }

  def createSuggestionsDatabaseUpdateKindDelete(
    remove: SuggestionsDatabaseUpdate.Remove
  )(implicit builder: FlatBufferBuilder): Int = {
    binary.SuggestionsDatabaseUpdate.startSuggestionsDatabaseUpdate(builder)
    binary.SuggestionsDatabaseUpdate.addId(builder, remove.id)
    binary.SuggestionsDatabaseUpdate.addKind(
      builder,
      SuggestionsDatabaseUpdateKind.Delete
    )
    binary.SuggestionsDatabaseUpdate.endSuggestionsDatabaseUpdate(builder)
  }

  def createSuggestion(
    suggestion: Suggestion
  )(implicit builder: FlatBufferBuilder): Int =
    suggestion match {
      case atom: Suggestion.Atom         => createSuggestionAtom(atom)
      case method: Suggestion.Method     => createSuggestionMethod(method)
      case function: Suggestion.Function => createSuggestionFunction(function)
      case local: Suggestion.Local       => createSuggestionLocal(local)
    }

  def createSuggestionLocal(
    local: Suggestion.Local
  )(implicit builder: FlatBufferBuilder): Int = {
    val scope = createScope(local.scope)
    SuggestionEntryLocal.startSuggestionEntryLocal(builder)
    SuggestionEntryLocal.addName(builder, builder.createString(local.name))
    SuggestionEntryLocal.addReturnType(
      builder,
      builder.createString(local.returnType)
    )
    SuggestionEntryLocal.addScope(builder, scope)
    SuggestionEntryLocal.endSuggestionEntryLocal(builder)
  }

  def createSuggestionFunction(
    function: Suggestion.Function
  )(implicit builder: FlatBufferBuilder): Int = {
    val scope     = createScope(function.scope)
    val arguments = function.arguments.map(createSuggestionArgument).toArray
    val argumentsVector =
      SuggestionEntryFunction.createArgumentsVector(builder, arguments)
    SuggestionEntryFunction.startSuggestionEntryFunction(builder)
    SuggestionEntryFunction.addName(
      builder,
      builder.createString(function.name)
    )
    SuggestionEntryFunction.addArguments(builder, argumentsVector)
    SuggestionEntryFunction.addReturnType(
      builder,
      builder.createString(function.returnType)
    )
    SuggestionEntryFunction.addScope(builder, scope)
    SuggestionEntryFunction.endSuggestionEntryFunction(builder)
  }

  def createSuggestionMethod(
    method: Suggestion.Method
  )(implicit builder: FlatBufferBuilder): Int = {
    val arguments = method.arguments.map(createSuggestionArgument).toArray
    val argumentsVector =
      SuggestionEntryMethod.createArgumentsVector(builder, arguments)
    SuggestionEntryMethod.startSuggestionEntryMethod(builder)
    SuggestionEntryMethod.addName(builder, builder.createString(method.name))
    SuggestionEntryMethod.addArguments(builder, argumentsVector)
    SuggestionEntryMethod.addSelfType(
      builder,
      builder.createString(method.selfType)
    )
    SuggestionEntryMethod.addReturnType(
      builder,
      builder.createString(method.returnType)
    )
    method.documentation.foreach { doc =>
      SuggestionEntryMethod.addDocumentation(builder, builder.createString(doc))
    }
    SuggestionEntryMethod.endSuggestionEntryMethod(builder)
  }

  def createSuggestionAtom(
    atom: Suggestion.Atom
  )(implicit builder: FlatBufferBuilder): Int = {
    val arguments = atom.arguments.map(createSuggestionArgument).toArray
    val argumentsVector =
      SuggestionEntryAtom.createArgumentsVector(builder, arguments)
    SuggestionEntryAtom.startSuggestionEntryAtom(builder)
    SuggestionEntryAtom.addName(builder, builder.createString(atom.name))
    SuggestionEntryAtom.addArguments(builder, argumentsVector)
    SuggestionEntryAtom.addReturnType(
      builder,
      builder.createString(atom.returnType)
    )
    atom.documentation.foreach { doc =>
      SuggestionEntryAtom.addDocumentation(builder, builder.createString(doc))
    }
    SuggestionEntryAtom.endSuggestionEntryAtom(builder)
  }

  def createSuggestionArgument(
    argument: Suggestion.Argument
  )(implicit builder: FlatBufferBuilder): Int = {
    SuggestionEntryArgument.startSuggestionEntryArgument(builder)
    SuggestionEntryArgument.addName(
      builder,
      builder.createString(argument.name)
    )
    SuggestionEntryArgument.addType(
      builder,
      builder.createString(argument.reprType)
    )
    SuggestionEntryArgument.addIsSuspended(builder, argument.isSuspended)
    SuggestionEntryArgument.addHasDefault(builder, argument.hasDefault)
    SuggestionEntryArgument.endSuggestionEntryArgument(builder)
  }

  def createScope(
    scope: Suggestion.Scope
  )(implicit builder: FlatBufferBuilder): Int = {
    SuggestionEntryScope.createSuggestionEntryScope(
      builder,
      scope.start,
      scope.end
    )
  }

  private def getSuggestionName(suggestion: Suggestion): String =
    suggestion match {
      case atom: Suggestion.Atom         => atom.name
      case method: Suggestion.Method     => method.name
      case function: Suggestion.Function => function.name
      case local: Suggestion.Local       => local.name
    }

  private def getSuggestionArguments(
    suggestion: Suggestion
  ): Option[Seq[Suggestion.Argument]] =
    suggestion match {
      case atom: Suggestion.Atom         => Some(atom.arguments)
      case method: Suggestion.Method     => Some(method.arguments)
      case function: Suggestion.Function => Some(function.arguments)
      case _: Suggestion.Local           => None
    }

  private def getSelfType(suggestion: Suggestion): Option[String] =
    suggestion match {
      case _: Suggestion.Atom        => None
      case method: Suggestion.Method => Some(method.selfType)
      case _: Suggestion.Function    => None
      case _: Suggestion.Local       => None
    }

  private def getReturnType(suggestion: Suggestion): String =
    suggestion match {
      case atom: Suggestion.Atom         => atom.returnType
      case method: Suggestion.Method     => method.returnType
      case function: Suggestion.Function => function.returnType
      case local: Suggestion.Local       => local.returnType
    }

  private def getDocumentation(suggestion: Suggestion): Option[String] =
    suggestion match {
      case atom: Suggestion.Atom     => atom.documentation
      case method: Suggestion.Method => method.documentation
      case _: Suggestion.Function    => None
      case _: Suggestion.Local       => None
    }

  private def getSuggestionScope(
    suggestion: Suggestion
  ): Option[Suggestion.Scope] =
    suggestion match {
      case _: Suggestion.Atom            => None
      case _: Suggestion.Method          => None
      case function: Suggestion.Function => Some(function.scope)
      case local: Suggestion.Local       => Some(local.scope)
    }

}
