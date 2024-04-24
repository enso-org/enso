package org.enso.languageserver.libraries

import akka.actor.ActorSystem
import akka.testkit._
import org.enso.distribution.FileSystem.PathSyntax
import org.enso.editions.LibraryName
import org.enso.librarymanager.LibraryLocations
import org.enso.logger.ReportLogsOnFailure
import org.enso.pkg.PackageManager
import org.enso.testkit.WithTemporaryDirectory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.BeforeAndAfterAll
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime

import scala.concurrent.duration.FiniteDuration

class LocalLibraryManagerSpec
    extends TestKit(ActorSystem("TestSystem"))
    with ImplicitSender
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with WithTemporaryDirectory
    with ReportLogsOnFailure {

  val Timeout: FiniteDuration = 10.seconds

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system, verifySystemShutdown = true)
    super.afterAll()
  }

  "LocalLibraryManager" should {
    "find the libraries it has itself created" in {
      val projectRoot = getTestDirectory / "project-root"
      PackageManager.Default.create(projectRoot.toFile, "Test_Project_123")
      val localLibraryRoot = getTestDirectory / "local-library-root"
      val libraryLocations = LibraryLocations(
        List(localLibraryRoot),
        getTestDirectory / "library-cache-root",
        List()
      )
      val manager =
        system.actorOf(
          LocalLibraryManager.props(projectRoot.toFile, libraryLocations)
        )

      val myLibraryName = LibraryName("user456", "My_Library")

      manager ! LocalLibraryManagerProtocol.Create(
        myLibraryName,
        Seq(),
        Seq(),
        "CC0"
      )
      expectMsg(Timeout, LocalLibraryManagerProtocol.EmptyResponse())

      manager ! LocalLibraryManagerProtocol.FindLibrary(myLibraryName)
      expectMsgPF(Timeout, "FindLibraryResponse") {
        case LocalLibraryManagerProtocol.FindLibraryResponse(Some(root)) =>
          assert(root.location.startsWith(localLibraryRoot))
      }

      manager ! LocalLibraryManagerProtocol.ListLocalLibraries
      val foundLibraries = expectMsgPF(Timeout, "ListLocalLibrariesResponse") {
        case LocalLibraryManagerProtocol.ListLocalLibrariesResponse(
              libraries
            ) =>
          libraries
      }
      foundLibraries.map(entry =>
        LibraryName(entry.namespace, entry.name)
      ) should contain theSameElementsAs Seq(myLibraryName)
    }
  }
}
