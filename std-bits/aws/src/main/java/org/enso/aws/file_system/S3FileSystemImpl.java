package org.enso.aws.file_system;

import org.enso.base.file_system.FileSystemSPI;

<<<<<<< HEAD:std-bits/aws/src/main/java/org/enso/aws/file_system/S3FileSystemSPI.java
public class S3FileSystemSPI extends FileSystemSPI {
=======
@org.openide.util.lookup.ServiceProvider(service = FileSystemSPI.class)
public final class S3FileSystemImpl extends FileSystemSPI {
>>>>>>> origin/develop:std-bits/aws/src/main/java/org/enso/aws/file_system/S3FileSystemImpl.java
  @Override
  protected String getModuleName() {
    return "Standard.AWS.S3.S3_File";
  }

  @Override
  protected String getTypeName() {
    return "S3_File";
  }

  @Override
  protected String getProtocol() {
    return "s3";
  }
}
