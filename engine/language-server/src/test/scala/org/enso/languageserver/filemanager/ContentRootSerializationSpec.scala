package org.enso.languageserver.filemanager

import io.circe.Json
import io.circe.syntax._
import io.circe.literal._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.UUID

class ContentRootSerializationSpec extends AnyWordSpec with Matchers {
  "ContentRoot" should {
    "correctly serialize" in {
      val id = UUID.randomUUID()

      def toJson(contentRoot: ContentRoot): Json = contentRoot.asJson

      toJson(ContentRoot.FileSystemRoot(id, path = "/")) shouldEqual
      json"""{
              "type": "FileSystemRoot",
              "id": $id,
              "path": "/"
            }"""

      toJson(ContentRoot.Project(id)) shouldEqual
      json"""{
              "type": "Project",
              "id": $id
            }"""

      toJson(ContentRoot.Home(id)) shouldEqual
      json"""{
              "type": "Home",
              "id": $id
            }"""

      toJson(ContentRoot.Library(id, "foo", "Bar", "baz")) shouldEqual
      json"""{
              "type": "Library",
              "id": $id,
              "namespace": "foo",
              "name": "Bar",
              "version": "baz"
            }"""

      toJson(ContentRoot.Custom(id)) shouldEqual
      json"""{
              "type": "Custom",
              "id": $id
            }"""
    }
  }
}
