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
    val nameOffset = builder.createString(getSuggestionName(add.suggestion))
    val argsOffsetOpt = getSuggestionArguments(add.suggestion).map { args =>
      val arguments = args.map(createSuggestionArgument).toArray
      binary.SuggestionsDatabaseUpdate.createArgumentsVector(
        builder,
        arguments
      )
    }
    val returnTypeOffset = builder.createString(getReturnType(add.suggestion))
    val selfTypeOffsetOpt = getSelfType(add.suggestion).map { selfType =>
      builder.createString(selfType)
    }
    val docOffsetOpt = getDocumentation(add.suggestion).map { doc =>
      builder.createString(doc)
    }
    val scopeOpt = getSuggestionScope(add.suggestion)

    binary.SuggestionsDatabaseUpdate
      .startSuggestionsDatabaseUpdate(builder)
    binary.SuggestionsDatabaseUpdate.addId(builder, add.id)
    binary.SuggestionsDatabaseUpdate.addKind(
      builder,
      SuggestionsDatabaseUpdateKind.Add
    )
    binary.SuggestionsDatabaseUpdate.addName(builder, nameOffset)
    binary.SuggestionsDatabaseUpdate.addReturnType(builder, returnTypeOffset)
    argsOffsetOpt.foreach { offset =>
      binary.SuggestionsDatabaseUpdate.addArguments(builder, offset)
    }
    selfTypeOffsetOpt.foreach { offset =>
      binary.SuggestionsDatabaseUpdate.addSelfType(builder, offset)
    }
    docOffsetOpt.foreach { offset =>
      binary.SuggestionsDatabaseUpdate.addDocumentation(builder, offset)
    }
    scopeOpt.foreach { scope =>
      binary.SuggestionsDatabaseUpdate.addScope(builder, createScope(scope))
    }
    binary.SuggestionsDatabaseUpdate.endSuggestionsDatabaseUpdate(builder)
  }

  def createSuggestionsDatabaseUpdateKindUpdate(
    modify: SuggestionsDatabaseUpdate.Modify
  )(implicit builder: FlatBufferBuilder): Int = {
    val nameOffsetOpt = modify.name.map(builder.createString)
    val argsOffsetOpt = modify.arguments.map { args =>
      val arguments = args.map(createSuggestionArgument).toArray
      binary.SuggestionsDatabaseUpdate.createArgumentsVector(builder, arguments)
    }
    val selfTypeOffsetOpt   = modify.selfType.map(builder.createString)
    val returnTypeOffsetOpt = modify.returnType.map(builder.createString)
    val docOffsetOpt        = modify.documentation.map(builder.createString)
    val scopeOpt            = modify.scope

    binary.SuggestionsDatabaseUpdate
      .startSuggestionsDatabaseUpdate(builder)
    binary.SuggestionsDatabaseUpdate.addId(builder, modify.id)
    binary.SuggestionsDatabaseUpdate.addKind(
      builder,
      SuggestionsDatabaseUpdateKind.Update
    )
    nameOffsetOpt.foreach { offset =>
      binary.SuggestionsDatabaseUpdate.addName(builder, offset)
    }
    argsOffsetOpt.foreach { offset =>
      binary.SuggestionsDatabaseUpdate.addArguments(builder, offset)
    }
    selfTypeOffsetOpt.foreach { offset =>
      binary.SuggestionsDatabaseUpdate.addSelfType(builder, offset)
    }
    returnTypeOffsetOpt.foreach { offset =>
      binary.SuggestionsDatabaseUpdate.addReturnType(builder, offset)
    }
    docOffsetOpt.foreach { offset =>
      binary.SuggestionsDatabaseUpdate.addDocumentation(builder, offset)
    }
    scopeOpt.foreach { scope =>
      binary.SuggestionsDatabaseUpdate.addScope(builder, createScope(scope))
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
    val nameOffset       = builder.createString(local.name)
    val returnTypeOffset = builder.createString(local.returnType)
    SuggestionEntryLocal.startSuggestionEntryLocal(builder)
    SuggestionEntryLocal.addName(builder, nameOffset)
    SuggestionEntryLocal.addReturnType(builder, returnTypeOffset)
    SuggestionEntryLocal.addScope(builder, createScope(local.scope))
    SuggestionEntryLocal.endSuggestionEntryLocal(builder)
  }

  def createSuggestionFunction(
    function: Suggestion.Function
  )(implicit builder: FlatBufferBuilder): Int = {
    val nameOffset = builder.createString(function.name)
    val arguments  = function.arguments.map(createSuggestionArgument).toArray
    val argumentsOffsest =
      SuggestionEntryFunction.createArgumentsVector(builder, arguments)
    val returnTypeOffset = builder.createString(function.returnType)
    SuggestionEntryFunction.startSuggestionEntryFunction(builder)
    SuggestionEntryFunction.addName(builder, nameOffset)
    SuggestionEntryFunction.addArguments(builder, argumentsOffsest)
    SuggestionEntryFunction.addReturnType(builder, returnTypeOffset)
    SuggestionEntryFunction.addScope(builder, createScope(function.scope))
    SuggestionEntryFunction.endSuggestionEntryFunction(builder)
  }

  def createSuggestionMethod(
    method: Suggestion.Method
  )(implicit builder: FlatBufferBuilder): Int = {
    val nameOffset = builder.createString(method.name)
    val arguments  = method.arguments.map(createSuggestionArgument).toArray
    val argumentsOffset =
      SuggestionEntryMethod.createArgumentsVector(builder, arguments)
    val selfTypeOffset   = builder.createString(method.selfType)
    val returnTypeOffset = builder.createString(method.returnType)
    val docOffsetOpt     = method.documentation.map(builder.createString)
    SuggestionEntryMethod.startSuggestionEntryMethod(builder)
    SuggestionEntryMethod.addName(builder, nameOffset)
    SuggestionEntryMethod.addArguments(builder, argumentsOffset)
    SuggestionEntryMethod.addSelfType(builder, selfTypeOffset)
    SuggestionEntryMethod.addReturnType(builder, returnTypeOffset)
    docOffsetOpt.foreach { offset =>
      SuggestionEntryMethod.addDocumentation(builder, offset)
    }
    SuggestionEntryMethod.endSuggestionEntryMethod(builder)
  }

  def createSuggestionAtom(
    atom: Suggestion.Atom
  )(implicit builder: FlatBufferBuilder): Int = {
    val nameOffsest = builder.createString(atom.name)
    val arguments   = atom.arguments.map(createSuggestionArgument).toArray
    val argumentsOffset =
      SuggestionEntryAtom.createArgumentsVector(builder, arguments)
    val returnTypeOffset = builder.createString(atom.returnType)
    val docOffsetOpt     = atom.documentation.map(builder.createString)
    SuggestionEntryAtom.startSuggestionEntryAtom(builder)
    SuggestionEntryAtom.addName(builder, nameOffsest)
    SuggestionEntryAtom.addArguments(builder, argumentsOffset)
    SuggestionEntryAtom.addReturnType(builder, returnTypeOffset)
    docOffsetOpt.foreach { offset =>
      SuggestionEntryAtom.addDocumentation(builder, offset)
    }
    SuggestionEntryAtom.endSuggestionEntryAtom(builder)
  }

  def createSuggestionArgument(
    argument: Suggestion.Argument
  )(implicit builder: FlatBufferBuilder): Int = {
    val nameOffset            = builder.createString(argument.name)
    val reprTypeOffset        = builder.createString(argument.reprType)
    val defaultValueOffsetOpt = argument.defaultValue.map(builder.createString)
    SuggestionEntryArgument.startSuggestionEntryArgument(builder)
    SuggestionEntryArgument.addName(builder, nameOffset)
    SuggestionEntryArgument.addType(builder, reprTypeOffset)
    SuggestionEntryArgument.addIsSuspended(builder, argument.isSuspended)
    SuggestionEntryArgument.addHasDefault(builder, argument.hasDefault)
    defaultValueOffsetOpt.foreach { offset =>
      SuggestionEntryArgument.addDefaultValue(builder, offset)
    }
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
