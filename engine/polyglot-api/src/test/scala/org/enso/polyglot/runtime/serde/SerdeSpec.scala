package org.enso.polyglot.runtime.serde

import java.util.UUID

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import org.enso.polyglot.Suggestion
import org.enso.polyglot.data.Tree
import org.enso.polyglot.runtime.Runtime.Api.SuggestionAction
import org.enso.polyglot.runtime.Runtime.{Api, ApiEnvelope}

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
                        params = Seq(
                          Suggestion
                            .Argument("foo", "bar", true, false, None, None)
                        ),
                        returnType = "Standard.Base.Data.Set.Set",
                        parentType = Some("Standard.Base.Any.Any"),
                        documentation =
                          Some(" An unordered collection of unique values"),
                        reexports = Set("foo")
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
                        params = Seq(
                          Suggestion
                            .Argument("foo", "bar", true, false, None, None)
                        ),
                        returnType = "Standard.Base.Data.Set.Set",
                        parentType = Some("Standard.Base.Any.Any"),
                        documentation =
                          Some(" An unordered collection of unique values"),
                        reexports = Set("foo")
                      ),
                    action = SuggestionAction.Modify(
                      documentation = Some(None),
                      returnType    = Some("foo")
                    )
                  ),
                  children = Vector.empty
                )
              )
            )
          )
        )
      )
    )

    val d1 = ApiSerde.serialize(message)
    val d2 = ApiSerde.deserializeApiEnvelope(d1).get

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
    val e1 = ApiSerde.serialize(libLoaded)
    val e2 = ApiSerde.deserializeApiEnvelope(e1).get

    libLoaded should equal(e2)
  }

  it should "serialize and deserialize Module suggestion" in {
    val message: ApiEnvelope = Api.Response(
      Api.SuggestionsDatabaseModuleUpdateNotification(
        module  = "local.Test.Main",
        actions = Vector.empty,
        exports = Vector.empty,
        updates = Tree.Root(
          children = Vector(
            Tree.Node(
              element = Api.SuggestionUpdate(
                suggestion = Suggestion
                  .Module(
                    "local.Test.Main",
                    documentation = Some(" A test module")
                  ),
                action = SuggestionAction.Add()
              ),
              children = Vector.empty
            )
          )
        )
      )
    )

    val d1 = ApiSerde.serialize(message)
    val d2 = ApiSerde.deserializeApiEnvelope(d1).get

    message should equal(d2)
  }

  it should "serialize and deserialize Type suggestion" in {
    val message: ApiEnvelope = Api.Response(
      Api.SuggestionsDatabaseModuleUpdateNotification(
        module  = "local.Test.Main",
        actions = Vector.empty,
        exports = Vector.empty,
        updates = Tree.Root(
          children = Vector(
            Tree.Node(
              element = Api.SuggestionUpdate(
                suggestion = Suggestion
                  .Type(
                    externalId = Some(UUID.randomUUID()),
                    module     = "local.Test.Main",
                    name       = "MyType",
                    params = Seq(
                      Suggestion
                        .Argument(
                          "a",
                          "Standard.Base.Any.Any",
                          false,
                          false,
                          None,
                          None
                        )
                    ),
                    returnType    = "local.Test.Main.MyType",
                    parentType    = Some("Standard.Base.Any.Any"),
                    documentation = Some(" A custom type")
                  ),
                action = SuggestionAction.Add()
              ),
              children = Vector.empty
            )
          )
        )
      )
    )

    val d1 = ApiSerde.serialize(message)
    val d2 = ApiSerde.deserializeApiEnvelope(d1).get

    message should equal(d2)
  }

  it should "serialize and deserialize Constructor suggestion" in {
    val message: ApiEnvelope = Api.Response(
      Api.SuggestionsDatabaseModuleUpdateNotification(
        module  = "local.Test.Main",
        actions = Vector.empty,
        exports = Vector.empty,
        updates = Tree.Root(
          children = Vector(
            Tree.Node(
              element = Api.SuggestionUpdate(
                suggestion = Suggestion
                  .Constructor(
                    externalId = Some(UUID.randomUUID()),
                    module     = "local.Test.Main",
                    name       = "MyCons",
                    arguments = Seq(
                      Suggestion
                        .Argument(
                          "foo",
                          "Standard.Base.Any.Any",
                          false,
                          false,
                          None,
                          None
                        ),
                      Suggestion
                        .Argument(
                          "bar",
                          "Standard.Base.Any.Any",
                          false,
                          false,
                          None,
                          None
                        )
                    ),
                    returnType    = "local.Test.Main.MyType",
                    documentation = Some(" A constructor"),
                    annotations   = Seq("a", "b")
                  ),
                action = SuggestionAction.Add()
              ),
              children = Vector.empty
            )
          )
        )
      )
    )

    val d1 = ApiSerde.serialize(message)
    val d2 = ApiSerde.deserializeApiEnvelope(d1).get

    message should equal(d2)
  }

  it should "serialize and deserialize Getter suggestion" in {
    val message: ApiEnvelope = Api.Response(
      Api.SuggestionsDatabaseModuleUpdateNotification(
        module  = "local.Test.Main",
        actions = Vector.empty,
        exports = Vector.empty,
        updates = Tree.Root(
          children = Vector(
            Tree.Node(
              element = Api.SuggestionUpdate(
                suggestion = Suggestion
                  .Getter(
                    externalId = Some(UUID.randomUUID()),
                    module     = "local.Test.Main",
                    name       = "foo",
                    arguments = Seq(
                      Suggestion
                        .Argument(
                          "self",
                          "local.Test.Main.MyType",
                          false,
                          false,
                          None,
                          None
                        )
                    ),
                    selfType      = "local.Test.Main.MyType",
                    returnType    = "Standard.Base.Any.Any",
                    documentation = None,
                    annotations   = Seq.empty
                  ),
                action = SuggestionAction.Add()
              ),
              children = Vector.empty
            )
          )
        )
      )
    )

    val d1 = ApiSerde.serialize(message)
    val d2 = ApiSerde.deserializeApiEnvelope(d1).get

    message should equal(d2)
  }

  it should "serialize and deserialize DefinedMethod suggestion" in {
    val message: ApiEnvelope = Api.Response(
      Api.SuggestionsDatabaseModuleUpdateNotification(
        module  = "local.Test.Main",
        actions = Vector.empty,
        exports = Vector.empty,
        updates = Tree.Root(
          children = Vector(
            Tree.Node(
              element = Api.SuggestionUpdate(
                suggestion = Suggestion
                  .DefinedMethod(
                    externalId    = Some(UUID.randomUUID()),
                    module        = "local.Test.Main",
                    name          = "main",
                    arguments     = Seq.empty,
                    selfType      = "local.Test.Main",
                    returnType    = "Standard.Base.Any.Any",
                    isStatic      = true,
                    documentation = Some(" The main method"),
                    annotations   = Seq("a")
                  ),
                action = SuggestionAction.Add()
              ),
              children = Vector.empty
            )
          )
        )
      )
    )

    val d1 = ApiSerde.serialize(message)
    val d2 = ApiSerde.deserializeApiEnvelope(d1).get

    message should equal(d2)
  }

  it should "serialize and deserialize Conversion suggestion" in {
    val message: ApiEnvelope = Api.Response(
      Api.SuggestionsDatabaseModuleUpdateNotification(
        module  = "local.Test.Main",
        actions = Vector.empty,
        exports = Vector.empty,
        updates = Tree.Root(
          children = Vector(
            Tree.Node(
              element = Api.SuggestionUpdate(
                suggestion = Suggestion
                  .Conversion(
                    externalId = Some(UUID.randomUUID()),
                    module     = "local.Test.Main",
                    arguments = Seq(
                      Suggestion.Argument(
                        "that",
                        "Standard.Base.Data.Numbers.Number",
                        false,
                        false,
                        None,
                        None
                      )
                    ),
                    selfType      = "local.Test.Main.MyType",
                    returnType    = "local.Test.Main.MyType",
                    documentation = Some(" A conversion")
                  ),
                action = SuggestionAction.Add()
              ),
              children = Vector.empty
            )
          )
        )
      )
    )

    val d1 = ApiSerde.serialize(message)
    val d2 = ApiSerde.deserializeApiEnvelope(d1).get

    message should equal(d2)
  }

  it should "serialize and deserialize Function suggestion" in {
    val message: ApiEnvelope = Api.Response(
      Api.SuggestionsDatabaseModuleUpdateNotification(
        module  = "local.Test.Main",
        actions = Vector.empty,
        exports = Vector.empty,
        updates = Tree.Root(
          children = Vector(
            Tree.Node(
              element = Api.SuggestionUpdate(
                suggestion = Suggestion
                  .Function(
                    externalId = Some(UUID.randomUUID()),
                    module     = "local.Test.Main",
                    name       = "helper",
                    arguments = Seq(
                      Suggestion
                        .Argument(
                          "x",
                          "Standard.Base.Any.Any",
                          false,
                          false,
                          None,
                          None
                        )
                    ),
                    returnType = "Standard.Base.Any.Any",
                    scope = Suggestion.Scope(
                      Suggestion.Position(1, 4),
                      Suggestion.Position(5, 0)
                    ),
                    documentation = None
                  ),
                action = SuggestionAction.Add()
              ),
              children = Vector.empty
            )
          )
        )
      )
    )

    val d1 = ApiSerde.serialize(message)
    val d2 = ApiSerde.deserializeApiEnvelope(d1).get

    message should equal(d2)
  }

  it should "serialize and deserialize Local suggestion" in {
    val message: ApiEnvelope = Api.Response(
      Api.SuggestionsDatabaseModuleUpdateNotification(
        module  = "local.Test.Main",
        actions = Vector.empty,
        exports = Vector.empty,
        updates = Tree.Root(
          children = Vector(
            Tree.Node(
              element = Api.SuggestionUpdate(
                suggestion = Suggestion
                  .Local(
                    externalId = Some(UUID.randomUUID()),
                    module     = "local.Test.Main",
                    name       = "x",
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
            )
          )
        )
      )
    )

    val d1 = ApiSerde.serialize(message)
    val d2 = ApiSerde.deserializeApiEnvelope(d1).get

    message should equal(d2)
  }

}
