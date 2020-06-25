package org.enso.languageserver.websocket.binary

import org.enso.jsonrpc.test.RetrySpec
import org.enso.languageserver.protocol.binary
import org.enso.languageserver.protocol.binary.{
  OutboundMessage,
  OutboundPayload
}
import org.enso.languageserver.runtime.SearchProtocol.SuggestionsDatabaseUpdate
import org.enso.searcher.Suggestion
import org.scalatest.concurrent.Eventually

class SuggestionsDatabaseEventsProtocolTest
    extends BaseBinaryServerTest
    with Eventually
    with RetrySpec {

  implicit private val decoder = OutboundMessageDecoder

  "A suggestions database events binary protocol" must {

    "send atom add updates" taggedAs Retry() in {
      //given
      val client = newWsClient()
      client.send(createSessionInitCmd())
      client.expectFrame()
      //when
      lastConnectionController ! SuggestionsDatabaseUpdate.Add(
        1,
        suggestion.atom
      )
      val Right(msg) = client.receiveMessage[OutboundMessage]()
      //then
      msg.payloadType() shouldBe OutboundPayload.SUGGESTIONS_DATABASE_UPDATE
      val payload = msg
        .payload(new binary.SuggestionsDatabaseUpdate)
        .asInstanceOf[binary.SuggestionsDatabaseUpdate]
      payload.id() shouldBe 1
      payload.kind() shouldBe binary.SuggestionsDatabaseUpdateKind.Add
      payload.name() shouldBe suggestion.atom.name
      payload.argumentsLength() shouldBe suggestion.atom.arguments.length
      compareArguments(payload.arguments(0), suggestion.atom.arguments(0))
      payload.selfType() shouldBe null
      payload.returnType() shouldBe suggestion.atom.returnType
      payload.documentation() shouldBe null
      payload.scope() shouldBe null
    }

    "send method add updates" taggedAs Retry() in {
      //given
      val client = newWsClient()
      client.send(createSessionInitCmd())
      client.expectFrame()
      //when
      lastConnectionController ! SuggestionsDatabaseUpdate.Add(
        2,
        suggestion.method
      )
      val Right(msg) = client.receiveMessage[OutboundMessage]()
      //then
      msg.payloadType() shouldBe OutboundPayload.SUGGESTIONS_DATABASE_UPDATE
      val payload = msg
        .payload(new binary.SuggestionsDatabaseUpdate)
        .asInstanceOf[binary.SuggestionsDatabaseUpdate]
      payload.id() shouldBe 2
      payload.kind() shouldBe binary.SuggestionsDatabaseUpdateKind.Add
      payload.name() shouldBe suggestion.method.name
      payload.argumentsLength() shouldBe suggestion.method.arguments.length
      compareArguments(payload.arguments(0), suggestion.method.arguments(0))
      compareArguments(payload.arguments(1), suggestion.method.arguments(1))
      payload.selfType() shouldBe suggestion.method.selfType
      payload.returnType() shouldBe suggestion.method.returnType
      payload.documentation() shouldBe suggestion.method.documentation.get
      payload.scope() shouldBe null
    }

    "send function add updates" taggedAs Retry() in {
      //given
      val client = newWsClient()
      client.send(createSessionInitCmd())
      client.expectFrame()
      //when
      lastConnectionController ! SuggestionsDatabaseUpdate.Add(
        3,
        suggestion.function
      )
      val Right(msg) = client.receiveMessage[OutboundMessage]()
      //then
      msg.payloadType() shouldBe OutboundPayload.SUGGESTIONS_DATABASE_UPDATE
      val payload = msg
        .payload(new binary.SuggestionsDatabaseUpdate)
        .asInstanceOf[binary.SuggestionsDatabaseUpdate]
      payload.id() shouldBe 3
      payload.kind() shouldBe binary.SuggestionsDatabaseUpdateKind.Add
      payload.name() shouldBe suggestion.function.name
      payload.argumentsLength() shouldBe suggestion.function.arguments.length
      payload.selfType() shouldBe null
      payload.returnType() shouldBe suggestion.function.returnType
      payload.documentation() shouldBe null
      compareScope(payload.scope(), suggestion.function.scope)
    }

    "send local add updates" taggedAs Retry() in {
      //given
      val client = newWsClient()
      client.send(createSessionInitCmd())
      client.expectFrame()
      //when
      lastConnectionController ! SuggestionsDatabaseUpdate.Add(
        4,
        suggestion.local
      )
      val Right(msg) = client.receiveMessage[OutboundMessage]()
      //then
      msg.payloadType() shouldBe OutboundPayload.SUGGESTIONS_DATABASE_UPDATE
      val payload = msg
        .payload(new binary.SuggestionsDatabaseUpdate)
        .asInstanceOf[binary.SuggestionsDatabaseUpdate]
      payload.id() shouldBe 4
      payload.kind() shouldBe binary.SuggestionsDatabaseUpdateKind.Add
      payload.name() shouldBe suggestion.local.name
      payload.argumentsLength() shouldBe 0
      payload.selfType() shouldBe null
      payload.returnType() shouldBe suggestion.local.returnType
      payload.documentation() shouldBe null
      compareScope(payload.scope(), suggestion.local.scope)
    }

    "send modify updates" taggedAs Retry() in {
      //given
      val client = newWsClient()
      client.send(createSessionInitCmd())
      client.expectFrame()
      val update = SuggestionsDatabaseUpdate.Modify(
        id   = 0,
        name = Some("foo"),
        arguments = Some(
          Seq(
            Suggestion.Argument("a", "Any", true, false, None),
            Suggestion.Argument("b", "Any", false, true, Some("object"))
          )
        ),
        selfType      = Some("Self"),
        returnType    = Some("Text"),
        documentation = Some("Doc"),
        scope         = Some(Suggestion.Scope(0, 100))
      )
      //when
      lastConnectionController ! update
      val Right(msg) = client.receiveMessage[OutboundMessage]()
      //then
      msg.payloadType() shouldBe OutboundPayload.SUGGESTIONS_DATABASE_UPDATE
      val payload = msg
        .payload(new binary.SuggestionsDatabaseUpdate)
        .asInstanceOf[binary.SuggestionsDatabaseUpdate]
      payload.id() shouldBe 0
      payload.kind() shouldBe binary.SuggestionsDatabaseUpdateKind.Update
      payload.name() shouldBe update.name.orNull
      payload.argumentsLength() shouldBe 2
      compareArguments(payload.arguments(0), update.arguments.get(0))
      compareArguments(payload.arguments(1), update.arguments.get(1))
      payload.selfType() shouldBe update.selfType.orNull
      payload.returnType() shouldBe update.returnType.orNull
      payload.documentation() shouldBe update.documentation.orNull
      compareScope(payload.scope(), update.scope.get)
    }

    "send remove updates" taggedAs Retry() in {
      //given
      val client = newWsClient()
      client.send(createSessionInitCmd())
      client.expectFrame()
      //when
      lastConnectionController ! SuggestionsDatabaseUpdate.Remove(101)
      val Right(msg) = client.receiveMessage[OutboundMessage]()
      //then
      msg.payloadType() shouldBe OutboundPayload.SUGGESTIONS_DATABASE_UPDATE
      val payload = msg
        .payload(new binary.SuggestionsDatabaseUpdate)
        .asInstanceOf[binary.SuggestionsDatabaseUpdate]
      payload.id() shouldBe 101
      payload.kind() shouldBe binary.SuggestionsDatabaseUpdateKind.Delete
      payload.name() shouldBe null
      payload.argumentsLength() shouldBe 0
      payload.selfType() shouldBe null
      payload.returnType() shouldBe null
      payload.documentation() shouldBe null
      payload.scope() shouldBe null
    }
  }

  private def compareArguments(
    payload: binary.SuggestionEntryArgument,
    argument: Suggestion.Argument
  ): Unit = {
    payload.name() shouldBe argument.name
    payload.`type`() shouldBe argument.reprType
    payload.isSuspended shouldBe argument.isSuspended
    payload.hasDefault shouldBe argument.hasDefault
    argument.defaultValue.foreach { defaultValue =>
      payload.defaultValue() shouldBe defaultValue
    }
  }

  private def compareScope(
    payload: binary.SuggestionEntryScope,
    scope: Suggestion.Scope
  ): Unit = {
    payload.start() shouldBe scope.start
    payload.end() shouldBe scope.end
  }

  object suggestion {

    val atom: Suggestion.Atom =
      Suggestion.Atom(
        name          = "MyType",
        arguments     = Seq(Suggestion.Argument("a", "Any", false, false, None)),
        returnType    = "MyAtom",
        documentation = None
      )

    val method: Suggestion.Method =
      Suggestion.Method(
        name = "foo",
        arguments = Seq(
          Suggestion.Argument("this", "MyType", false, false, None),
          Suggestion.Argument("foo", "Number", false, true, Some("42"))
        ),
        selfType      = "MyType",
        returnType    = "Number",
        documentation = Some("My doc")
      )

    val function: Suggestion.Function =
      Suggestion.Function(
        name       = "print",
        arguments  = Seq(),
        returnType = "IO",
        scope      = Suggestion.Scope(7, 10)
      )

    val local: Suggestion.Local =
      Suggestion.Local(
        name       = "x",
        returnType = "Number",
        scope      = Suggestion.Scope(15, 17)
      )
  }
}
