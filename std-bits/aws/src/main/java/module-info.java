import org.enso.aws.database.RedshiftConnectionDetailsSPI;
import org.enso.aws.file_system.S3FileSystemSPI;
import org.enso.base.file_system.FileSystemSPI;
import org.enso.database.DatabaseConnectionDetailsSPI;

module org.enso.std.aws {
  requires java.base;
  requires org.enso.std.base;
  requires org.enso.std.database;
  requires java.logging;
  requires software.amazon.awssdk.http;
  requires software.amazon.awssdk.regions;
  requires software.amazon.awssdk.services.s3;
  requires software.amazon.awssdk.auth;
  requires software.amazon.awssdk.core;
  requires software.amazon.awssdk.profiles;
  requires software.amazon.awssdk.awscore;

  provides FileSystemSPI with
      S3FileSystemSPI;
  provides DatabaseConnectionDetailsSPI with
      RedshiftConnectionDetailsSPI;
}
