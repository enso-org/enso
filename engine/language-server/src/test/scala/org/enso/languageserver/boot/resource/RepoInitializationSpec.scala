package org.enso.languageserver.boot.resource

import akka.actor.ActorSystem
import akka.testkit._
import org.apache.commons.io.FileUtils
import org.enso.languageserver.boot.{ProfilingConfig, StartupConfig}
import org.enso.languageserver.data._
import org.enso.languageserver.event.InitializedEvent
import org.enso.languageserver.filemanager.{ContentRoot, ContentRootWithFile}
import org.enso.logger.ReportLogsOnFailure
import org.enso.searcher.memory.InmemorySuggestionsRepo
import org.enso.testkit.{FlakySpec, ToScalaFutureConversions}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import java.nio.file.Files
import java.util.UUID
import scala.annotation.unused
import scala.concurrent.Await
import scala.concurrent.duration._

class RepoInitializationSpec
    extends TestKit(ActorSystem("TestSystem"))
    with ImplicitSender
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with ToScalaFutureConversions
    with FlakySpec
    with ReportLogsOnFailure {

  import system.dispatcher

  val Timeout: FiniteDuration = 10.seconds.dilated

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system, verifySystemShutdown = true)
    super.afterAll()
  }

  "RepoInitialization" should {

    "initialize repositories" in withDb { (config, suggestionsRepo) =>
      system.eventStream.subscribe(self, classOf[InitializedEvent])

      val component =
        new RepoInitialization(
          system.dispatcher,
          config.directories,
          system.eventStream,
          suggestionsRepo
        )

      val resourceInitialization = component.init()
      Await.result(resourceInitialization, Timeout)

      expectMsg(InitializedEvent.SuggestionsRepoInitialized)
    }

    "recreate suggestion database when schema version is incorrect" in withDb {
      (config, suggestionsRepo) =>
        system.eventStream.subscribe(self, classOf[InitializedEvent])

        val component =
          new RepoInitialization(
            system.dispatcher,
            config.directories,
            system.eventStream,
            suggestionsRepo
          )

        val action =
          for {
            _ <- suggestionsRepo.init
            _ <- component.init()
          } yield ()

        Await.result(action, Timeout)
        expectMsg(InitializedEvent.SuggestionsRepoInitialized)
    }

    /*"recreate corrupted suggestion database file" taggedAs Flaky in withConfig {
      config =>
        // initialize
        withRepos(config) { (sqlDatabase, suggestionsRepo) =>
          val component =
            new RepoInitialization(
              system.dispatcher,
              config.directories,
              system.eventStream,
              sqlDatabase,
              suggestionsRepo
            )

          val init =
            for {
              _       <- component.init()
              version <- suggestionsRepo.getSchemaVersion
            } yield version

          val version1 = Await.result(init, Timeout)
          version1 shouldEqual SchemaVersion.CurrentVersion
        }

        // corrupt
        val bytes: Array[Byte] = Array(1, 2, 3)
        Files.delete(config.directories.suggestionsDatabaseFile.toPath)
        Files.write(
          config.directories.suggestionsDatabaseFile.toPath,
          bytes,
          StandardOpenOption.CREATE
        )
        withRepos(config) { (sqlDatabase, suggestionsRepo) =>
          sqlDatabase.open()
          an[SQLiteException] should be thrownBy Await.result(
            suggestionsRepo.getSchemaVersion,
            Timeout
          )
        }

        // re-initialize
        withRepos(config) { (sqlDatabase, suggestionsRepo) =>
          val component =
            new RepoInitialization(
              system.dispatcher,
              config.directories,
              system.eventStream,
              sqlDatabase,
              suggestionsRepo
            )

          val action =
            for {
              _       <- component.init()
              version <- suggestionsRepo.getSchemaVersion
            } yield version

          val version2 = Await.result(action, Timeout)
          version2 shouldEqual SchemaVersion.CurrentVersion
          expectMsg(InitializedEvent.SuggestionsRepoInitialized)
        }
    }*/

  }

  def newConfig(root: ContentRootWithFile): Config = {
    Config(
      root,
      FileManagerConfig(timeout = 3.seconds.dilated),
      VcsManagerConfig(),
      PathWatcherConfig(),
      ExecutionContextConfig(requestTimeout = 3.seconds.dilated),
      ProjectDirectoriesConfig.initialize(root.file),
      ProfilingConfig(),
      StartupConfig(),
      None
    )
  }

  def withConfig(test: Config => Any): Unit = {
    val testContentRoot = Files.createTempDirectory(null).toRealPath()
    sys.addShutdownHook(FileUtils.deleteQuietly(testContentRoot.toFile))
    val config = newConfig(
      ContentRootWithFile(
        ContentRoot.Project(UUID.randomUUID()),
        testContentRoot.toFile
      )
    )

    test(config)
  }

  def withRepos(
    @unused config: Config
  )(test: InmemorySuggestionsRepo => Any): Unit = {
    val suggestionsRepo = new InmemorySuggestionsRepo()
    test(suggestionsRepo)
  }

  def withDb(
    test: (
      Config,
      InmemorySuggestionsRepo
    ) => Any
  ): Unit = {
    withConfig { config =>
      withRepos(config) { suggestionsRepo =>
        test(config, suggestionsRepo)
      }
    }
  }
}
