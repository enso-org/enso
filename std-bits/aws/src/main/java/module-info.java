module org.enso.aws {
  requires java.base;
  requires org.enso.base;

  provides FileSystemSPI with
      org.enso.aws.file_system.S3FileSystemSPI;
}
