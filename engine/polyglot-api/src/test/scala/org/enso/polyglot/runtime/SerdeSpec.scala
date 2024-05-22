package org.enso.polyglot.runtime

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import org.enso.polyglot.Suggestion
import org.enso.polyglot.data.Tree
import Runtime.Api.SuggestionAction
import Runtime.{Api, ApiEnvelope}

import java.util.UUID

class SerdeSpec extends AnyFlatSpec with Matchers {

  it should "serialize and deserialize API messages in JSON" in {
    val message: ApiEnvelope = Api.Response(
      Api.SuggestionsDatabaseModuleUpdateNotification(
        module  = "Dummy",
        actions = Vector.empty,
        exports = Vector.empty,
        updates = Tree.Root(
          children = Vector(
            Tree.Node(
              element = Api.SuggestionUpdate(
                suggestion = Suggestion
                  .Module("local.New_Project_1.Main", documentation = None),
                action = SuggestionAction.Add()
              ),
              children = Vector.empty
            ),
            Tree.Node(
              element = Api.SuggestionUpdate(
                suggestion = Suggestion
                  .DefinedMethod(
                    externalId    = Some(UUID.randomUUID()),
                    module        = "local.New_Project_1.Main",
                    name          = "main",
                    arguments     = Seq.empty,
                    selfType      = "local.New_Project_1.Main",
                    returnType    = "Standard.Base.Any.Any",
                    isStatic      = true,
                    documentation = None,
                    annotations   = Seq.empty
                  ),
                action = SuggestionAction.Add()
              ),
              children = Vector(
                Tree.Node(
                  element = Api.SuggestionUpdate(
                    suggestion = Suggestion
                      .Local(
                        externalId = Some(UUID.randomUUID()),
                        module     = "local.New_Project_1.Main",
                        name       = "main",
                        returnType = "Standard.Base.Any.Any",
                        scope = Suggestion.Scope(
                          Suggestion.Position(0, 1),
                          Suggestion.Position(2, 3)
                        ),
                        documentation = None
                      ),
                    action = SuggestionAction.Add()
                  ),
                  children = Vector.empty
                ),
                Tree.Node(
                  element = Api.SuggestionUpdate(
                    suggestion = Suggestion
                      .Type(
                        externalId = Some(UUID.randomUUID()),
                        module     = "Standard.Base.Data.Set",
                        name       = "Set",
                        params =
                          Seq.empty, // Set(Suggestion.Argument("foo", "bar", true, false, None, None)),
                        returnType = "Standard.Base.Data.Set.Set",
                        parentType = Some("Standard.Base.Any.Any"),
                        documentation =
                          None //Some(" An unordered collection of unique values")
                        //reexports = Set("foo")
                      ),
                    action = SuggestionAction.Modify(documentation = Some(None))
                  ),
                  children = Vector.empty
                ),
                Tree.Node(
                  element = Api.SuggestionUpdate(
                    suggestion = Suggestion
                      .Type(
                        externalId = Some(UUID.randomUUID()),
                        module     = "Standard.Base.Data.Vector",
                        name       = "Set",
                        params =
                          Seq.empty, // Set(Suggestion.Argument("foo", "bar", true, false, None, None)),
                        returnType = "Standard.Base.Data.Set.Set",
                        parentType = Some("Standard.Base.Any.Any"),
                        documentation =
                          Some(" An unordered collection of unique values"),
                        reexports = Set("foo")
                      ),
                    action = SuggestionAction.Modify(documentation = Some(None))
                  ),
                  children = Vector.empty
                )
              )
            )
          )
        )
      )
    )

    val d1 = Api.serialize(message)
    val d2 = Api.deserializeApiEnvelope(d1).get

    message should equal(d2)

    val libLoaded =
      Api.Response(
        None,
        Api.LibraryLoaded(
          "Standard",
          "Base",
          "0.0.0-dev",
          new java.io.File(
            "enso/built-distribution/enso-engine-0.0.0-dev-linux-amd64/enso-0.0.0-dev/lib/Standard/Base/0.0.0-dev"
          )
        )
      )
    val e1 = Api.serialize(libLoaded)
    val e2 = Api.deserializeApiEnvelope(e1).get

    libLoaded should equal(e2)
  }

}
