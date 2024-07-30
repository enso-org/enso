import org.enso.base.enso_cloud.EnsoPathFileSystemSPI;
import org.enso.base.file_system.FileSystemSPI;

module org.enso.base {
  requires java.logging;
  requires org.graalvm.collections;

  uses FileSystemSPI;

  exports org.enso.base.file_system;

  provides FileSystemSPI with
      EnsoPathFileSystemSPI;
}
