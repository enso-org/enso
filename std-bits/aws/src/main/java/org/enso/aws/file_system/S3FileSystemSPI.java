package org.enso.aws.file_system;

import org.enso.base.file_system.FileSystemSPI;

@org.openide.util.lookup.ServiceProvider(service = FileSystemSPI.class)
public class S3FileSystemSPI extends FileSystemSPI {
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
