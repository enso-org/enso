package org.enso.languageserver.websocket.json

import io.circe.literal._
import org.apache.commons.io.FileUtils
import org.eclipse.jgit.api.{Git => JGit}
import org.eclipse.jgit.lib.Repository
import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.storage.file.FileRepositoryBuilder

import org.enso.languageserver.boot.ProfilingConfig
import org.enso.languageserver.data._
import org.enso.testkit.RetrySpec

import java.io.File
import java.nio.file.{Files, Path, Paths}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

class VcsManagerTest extends BaseServerTest with RetrySpec {

  override def mkConfig: Config = {
    val directoriesDir = Files.createTempDirectory(null).toRealPath()
    sys.addShutdownHook(FileUtils.deleteQuietly(directoriesDir.toFile))
    Config(
      testContentRoot,
      FileManagerConfig(timeout = 3.seconds),
      VcsManagerConfig(timeout  = 5.seconds),
      PathWatcherConfig(),
      ExecutionContextConfig(requestTimeout = 3.seconds),
      ProjectDirectoriesConfig.initialize(testContentRoot.file),
      ProfilingConfig()
    )
  }

  "Initializing project" must {
    "create a repository" taggedAs Retry in {
      val client = getInitialisedWsClient()
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo", "bar", "baz.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": null
          }
          """)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "vcs/init",
            "id": 2,
            "params": {
              "root": {
                "rootId": $testContentRootId,
                "segments": []
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": null
          }
          """)

      val path =
        Paths.get(testContentRoot.file.toString, "foo", "bar", "baz.txt")
      Files.readAllLines(path).get(0) shouldBe "123456789"

      testContentRoot.file.toPath.resolve(".git").toFile should exist
    }

    "fail to create a repository for an already existing project" taggedAs Retry in withCleanRoot {
      val client = getInitialisedWsClient()
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "vcs/init",
            "id": 1,
            "params": {
              "root": {
                "rootId": $testContentRootId,
                "segments": []
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "error": {
              "code": 1000,
              "message": "repository already exists"
            }
          }
          """)
    }
  }

  "Save project" must {
    "create a commit with a timestamp" taggedAs Retry in withCleanRoot {
      val client = getInitialisedWsClient()
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 1,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo", "bar", "baz.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": null
          }
          """)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "vcs/save",
            "id": 2,
            "params": {
              "root": {
                "rootId": $testContentRootId,
                "segments": []
              }
            }
          }
          """)
      client.fuzzyExpectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": {
              "name" : "*",
              "sha"  : "*"
            }
          }
          """)
      commits(testContentRoot.file) should have length 2

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 3,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo", "bar", "baz.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 3,
            "result": null
          }
          """)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "vcs/save",
            "id": 4,
            "params": {
              "root": {
                "rootId": $testContentRootId,
                "segments": []
              }
            }
          }
          """)
      client.fuzzyExpectJson(json"""
          { "jsonrpc": "2.0",
            "id": 4,
            "result": {
              "name" : "*",
              "sha"  : "*"
            }
          }
          """)
      commits(testContentRoot.file) should have length 3
    }
  }

  def withCleanRoot[T](test: => T): T = {
    FileUtils.deleteQuietly(testContentRoot.file.toPath.resolve(".git").toFile)
    val path = testContentRoot.file
    val jgit =
      JGit
        .init()
        .setDirectory(path)
        .setBare(false)
        .call()

    jgit
      .commit()
      .setAllowEmpty(true)
      .setAll(true)
      .setMessage("Initial commit")
      .setAuthor("Enso VCS", "vcs@enso.io")
      .call()
    test
  }

  private def repository(path: Path): Repository = {
    val builder = new FileRepositoryBuilder();
    builder
      .setGitDir(path.resolve(".git").toFile)
      .setMustExist(true)
      .build()
  }

  def commits(root: File): List[RevCommit] = {
    val jgit = new JGit(repository(root.toPath))
    jgit.log().call().asScala.toList
  }

}
