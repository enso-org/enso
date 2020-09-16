package org.enso.launcher.locking

import java.nio.file.{Files, Path}

import nl.gn0s1s.bump.SemVer
import org.enso.cli.TaskProgress
import org.enso.launcher.cli.GlobalCLIOptions
import org.enso.launcher.components.{ComponentsManager, RuntimeVersion}
import org.enso.launcher.installation.DistributionManager
import org.enso.launcher.releases.engine.{EngineRelease, EngineReleaseProvider}
import org.enso.launcher.releases.runtime.GraalCEReleaseProvider
import org.enso.launcher.releases.testing.FakeReleaseProvider
import org.enso.launcher.{
  components,
  FakeEnvironment,
  FileSystem,
  WithTemporaryDirectory
}
import org.enso.launcher.FileSystem.PathSyntax
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Try

class ConcurrencyTest
    extends AnyWordSpec
    with Matchers
    with WithTemporaryDirectory
    with FakeEnvironment {

  case class WrapEngineRelease(
    originalRelease: EngineRelease,
    callback: String => Unit
  ) extends EngineRelease {
    override def version: SemVer = originalRelease.version
    override def manifest: components.Manifest =
      originalRelease.manifest
    override def isBroken: Boolean = originalRelease.isBroken
    override def packageFileName: String =
      originalRelease.packageFileName
    override def downloadPackage(
      destination: Path
    ): TaskProgress[Unit] = {
      callback(packageFileName)
      originalRelease.downloadPackage(destination)
    }
  }

  def makeComponentsManager(
    releaseCallback: String => Unit,
    lockWaitsCallback: String => Unit
  ): ComponentsManager = {
    val env = fakeInstalledEnvironment()

    val resourceManager = new ResourceManager(TestLocalLockManager) {
      override def withResource[R](
        resource: Resource,
        lockType: LockType,
        waitingAction: Option[Resource => Unit]
      )(action: => R): R = {
        val overriddenWaitingAction = (resource: Resource) => {
          lockWaitsCallback(resource.name)
          waitingAction.foreach(_.apply(resource))
        }
        super.withResource(resource, lockType, Some(overriddenWaitingAction))(
          action
        )
      }
    }

    val distributionManager = new DistributionManager(env, resourceManager)
    val fakeReleasesRoot =
      Path.of(
        getClass
          .getResource("/org/enso/launcher/components/fake-releases")
          .toURI
      )
    val engineProvider = new EngineReleaseProvider(
      FakeReleaseProvider(
        fakeReleasesRoot.resolve("enso"),
        copyIntoArchiveRoot = Seq("manifest.yaml")
      )
    ) {
      override def fetchRelease(version: SemVer): Try[EngineRelease] =
        super.fetchRelease(version).map(WrapEngineRelease(_, releaseCallback))
    }
    val runtimeProvider = new GraalCEReleaseProvider(
      FakeReleaseProvider(fakeReleasesRoot.resolve("graalvm"))
    ) {
      override def downloadPackage(
        version: RuntimeVersion,
        destination: Path
      ): TaskProgress[Unit] = {
        releaseCallback(packageFileName(version))
        super.downloadPackage(version, destination)
      }
    }

    val componentsManager = new ComponentsManager(
      GlobalCLIOptions(
        autoConfirm  = true,
        hideProgress = true,
        useJSON      = false
      ),
      distributionManager,
      resourceManager,
      engineProvider,
      runtimeProvider
    )

    componentsManager
  }

  "locks" should {
    "synchronize parallel installations with the same runtime" in {

      /**
        * Two threads start installing different engines in parallel, but these
        * engines use the same runtime. The second thread is stalled on
        * downloading its engine package, so that the first one can start
        * downloading the runtime. When it starts downloading, it is suspended
        * and the second thread is resumed so it also wants to download the
        * runtime. It should however wait on the runtime lock, when we see that
        * it indeed waits, the first is resumed to finish downloading the
        * runtime. Then the second thread should automatically resume and not
        * install the runtime a second time, seeing changes from first thread.
        *
        * This test also checks that the temporary directory is cleaned at
        * startup, but only if it is safe to do so (so other processes are not
        * using it).
        */
      val sync = new TestSynchronizer

      val engine1 = SemVer(0, 0, 1)
      val engine2 = engine1.withPreRelease("pre")

      val tmpRoot = getTestDirectory / "test_data" / "tmp"
      Files.createDirectories(tmpRoot)
      val garbage = tmpRoot / "garbage.txt"
      FileSystem.writeTextFile(garbage, "Garbage")

      sync.startThread("t1") {
        val componentsManager = makeComponentsManager(
          releaseCallback = { asset =>
            if (asset.startsWith("graalvm-")) {
              sync.signal("t1-downloads-runtime")
              sync.waitFor("t2-waits-for-runtime")
            }
          },
          lockWaitsCallback = resource =>
            throw new IllegalStateException(
              s"t1 should not be waiting on $resource."
            )
        )

        componentsManager.findOrInstallEngine(engine1, complain = false)
      }

      sync.waitFor("t1-downloads-runtime")

      sync.startThread("t2") {
        val componentsManager = makeComponentsManager(
          releaseCallback = { asset =>
            if (asset.startsWith("graalvm-")) {
              throw new IllegalStateException(
                "t2 should not download runtime, " +
                "as it should have been done by t1 already."
              )
            }
          },
          lockWaitsCallback = {
            case resource if resource.startsWith("runtime") =>
              sync.signal("t2-waits-for-runtime")
            case resource =>
              throw new IllegalStateException(s"Unexpected wait on $resource.")
          }
        )

        componentsManager.findOrInstallEngine(engine2, complain = false)
      }

      sync.join()

      assert(
        Files.notExists(garbage),
        "The temporary directory should have been cleaned."
      )
    }

    "synchronize installation and run" ignore {}

    "synchronize uninstallation and run" ignore {}

    "synchronize upgrades" ignore {}

    "synchronize main lock" in {

      /**
        * First two threads start and acquire the shared lock, than the third
        * thread tries to acquire an exclusive lock (in practice that will be our
        * (un)installer), it should wait for the other threads to finish. When
        * the threads see that it started waiting (the waiting notification is
        * normally used to tell the user what the application is waiting for),
        * the two threads finish and after that the third one is able to acquire
        * the exclusive lock.
        */
      val sync = new TestSynchronizer
      sync.startThread("t1") {
        val resourceManager = new ResourceManager(TestLocalLockManager)
        resourceManager.initializeMainLock()
        sync.report("shared-start")
        sync.signal("started-1")
        sync.waitFor("finish-1")
        sync.report("shared-end")
        resourceManager.releaseMainLock()
      }

      sync.startThread("t2") {
        val resourceManager = new ResourceManager(TestLocalLockManager)
        resourceManager.initializeMainLock()
        sync.report("shared-start")
        sync.signal("started-2")
        sync.waitFor("finish-2")
        sync.report("shared-end")
        resourceManager.releaseMainLock()
      }

      sync.waitFor("started-1")
      sync.waitFor("started-2")

      sync.startThread("t3") {
        val resourceManager = new ResourceManager(TestLocalLockManager)
        resourceManager.initializeMainLock()
        sync.report("t3-start")
        resourceManager.acquireExclusiveMainLock(() => {
          sync.report("t3-wait")
          sync.signal("waiting")
        })
        sync.report("t3-end")
        sync.signal("finish-all")
        resourceManager.releaseMainLock()
      }

      sync.waitFor("waiting")
      Thread.sleep(1000)

      sync.signal("finish-1")
      sync.signal("finish-2")

      sync.waitFor("finish-all")

      sync.join()
      sync.summarizeReports() shouldEqual Seq(
        "shared-start",
        "shared-start",
        "t3-start",
        "t3-wait",
        "shared-end",
        "shared-end",
        "t3-end"
      )
    }
  }
}
