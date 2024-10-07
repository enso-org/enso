import org.enso.aws.database.RedshiftConnectionDetailsSPI;
import org.enso.aws.file_system.S3FileSystemSPI;
import org.enso.base.file_system.FileSystemSPI;
import org.enso.database.DatabaseConnectionDetailsSPI;

module org.enso.std.aws {
  requires java.base;
  requires org.enso.std.base;
  requires org.enso.std.database;
  requires java.logging;
  requires org.enso.wrapper.aws;

  provides FileSystemSPI with
      S3FileSystemSPI;
  provides DatabaseConnectionDetailsSPI with
      RedshiftConnectionDetailsSPI;
}
