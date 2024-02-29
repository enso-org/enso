package org.enso.base.enso_cloud;

import org.enso.base.file_system.FileSystemSPI;

@org.openide.util.lookup.ServiceProvider(service = FileSystemSPI.class)
public class EnsoPathFileSystemSPI extends FileSystemSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Base.Enso_Cloud.Enso_File";
  }

  @Override
  protected String getTypeName() {
    return "Enso_File";
  }

  @Override
  protected String getProtocol() {
    return "enso";
  }
}
