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
import org.scalatest.BeforeAndAfterEach
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Try

class ConcurrencyTest
    extends AnyWordSpec
    with Matchers
    with WithTemporaryDirectory
    with FakeEnvironment
    with BeforeAndAfterEach {

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

  var testLocalLockManager: Option[TestLocalLockManager] = None

  override def beforeEach(): Unit = {
    super.beforeEach()
    testLocalLockManager = Some(new TestLocalLockManager)
  }

  /**
    * A separate [[LockManager]] for each test case.
    * @return
    */
  def lockManager: LockManager = testLocalLockManager.get

  /**
    * Each call creates a distinct [[ResourceManager]], but all resource
    * managers created within one test case share the same [[lockManager]], so
    * that they see each other's locks.
    */
  def makeNewResourceManager(): ResourceManager =
    new ResourceManager(lockManager)

  /**
    * Creates a [[DistributionManager]] and [[ComponentsManager]] that can be
    * used in a test.
    *
    * @param releaseCallback called when a release asset is fetched
    * @param lockWaitsCallback called when a lock with the given name is not
    *                          acquired immediately
    * @return a tuple of [[DistributionManager]] and [[ComponentsManager]]
    */
  def makeManagers(
    releaseCallback: String => Unit,
    lockWaitsCallback: String => Unit
  ): (DistributionManager, ComponentsManager) = {
    val env = fakeInstalledEnvironment()
    val resourceManager = new ResourceManager(lockManager) {
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

    (distributionManager, componentsManager)
  }

  /**
    * Helper function, acts as [[makeManagers]] but returns only the
    * [[ComponentsManager]].
    */
  def makeComponentsManager(
    releaseCallback: String => Unit,
    lockWaitsCallback: String => Unit
  ): ComponentsManager =
    makeManagers(
      releaseCallback   = releaseCallback,
      lockWaitsCallback = lockWaitsCallback
    )._2

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
        val (distributionManager, componentsManager) = makeManagers(
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

        distributionManager.tryCleaningTemporaryDirectory()
        componentsManager.findOrInstallEngine(engine1, complain = false)
      }

      sync.waitFor("t1-downloads-runtime")

      sync.startThread("t2") {
        val (distributionManager, componentsManager) = makeManagers(
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

        distributionManager.tryCleaningTemporaryDirectory()
        componentsManager.findOrInstallEngine(engine2, complain = false)
      }

      sync.join()

      assert(
        Files.notExists(garbage),
        "The temporary directory should have been cleaned."
      )
    }

    "synchronize installation and usage" in {

      /**
        * The first thread starts installing the engine, but is suspended when
        * downloading the package. The second thread then tries to use it, but
        * it should wait until the installation is finished.
        */
      val sync = new TestSynchronizer

      val engineVersion = SemVer(0, 0, 1)

      sync.startThread("t1") {
        val componentsManager = makeComponentsManager(
          releaseCallback = { asset =>
            if (asset.startsWith("enso-engine-")) {
              sync.signal("t1-downloads-engine")
              sync.waitFor("t2-waits-for-engine")
            } else if (asset.startsWith("graal")) {
              sync.report("installation-continues")
            }
          },
          lockWaitsCallback = resource =>
            throw new IllegalStateException(
              s"t1 should not be waiting on $resource."
            )
        )

        componentsManager.findOrInstallEngine(engineVersion, complain = false)
      }

      sync.waitFor("t1-downloads-engine")

      sync.startThread("t2") {
        val componentsManager = makeComponentsManager(
          releaseCallback = { asset =>
            throw new IllegalStateException(
              s"t2 should not be downloading $asset."
            )
          },
          lockWaitsCallback = {
            case resource if resource.startsWith("engine") =>
              sync.signal("t2-waits-for-engine")
            case resource =>
              throw new IllegalStateException(s"Unexpected wait on $resource.")
          }
        )

        componentsManager.withEngineAndRuntime(engineVersion) { (_, _) =>
          sync.report("using-engine")
        }
      }

      sync.join()
      sync.summarizeReports() shouldEqual Seq(
        "installation-continues",
        "using-engine"
      )
    }

    "synchronize uninstallation and usage" in {

      /**
        * The first thread starts using the engine, while in the meantime
        * another thread starts uninstalling it. The second thread has to wait
        * with uninstalling until the first one finishes using it.
        */
      val sync = new TestSynchronizer

      val engineVersion = SemVer(0, 0, 1)

      sync.startThread("t1") {
        val componentsManager = makeComponentsManager(
          releaseCallback = _ => (),
          lockWaitsCallback = resource =>
            throw new IllegalStateException(
              s"t1 should not be waiting on $resource."
            )
        )

        componentsManager.withEngine(engineVersion) { _ =>
          sync.report("t1-start-using")
          sync.signal("t1-start-using")
          sync.waitFor("t2-waits-with-uninstall")
          sync.report("t1-end-using")
        }
      }

      sync.waitFor("t1-start-using")

      sync.startThread("t2") {
        val componentsManager = makeComponentsManager(
          releaseCallback = asset =>
            throw new IllegalStateException(s"t2 should not download $asset."),
          lockWaitsCallback = {
            case resource if resource.startsWith("engine") =>
              sync.report("t2-start-uninstall-and-wait")
              sync.signal("t2-waits-with-uninstall")
            case resource =>
              throw new IllegalStateException(s"Unexpected wait on $resource.")
          }
        )

        componentsManager.uninstallEngine(engineVersion)
        sync.report("t2-uninstalled")
      }

      sync.join()
      sync.summarizeReports() shouldEqual Seq(
        "t1-start-using",
        "t2-start-uninstall-and-wait",
        "t1-end-using",
        "t2-uninstalled"
      )
    }

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
        val resourceManager = makeNewResourceManager()
        resourceManager.initializeMainLock()
        sync.report("shared-start")
        sync.signal("started-1")
        sync.waitFor("finish-1")
        sync.report("shared-end")
        resourceManager.releaseMainLock()
      }

      sync.startThread("t2") {
        val resourceManager = makeNewResourceManager()
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
        val resourceManager = makeNewResourceManager()
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
