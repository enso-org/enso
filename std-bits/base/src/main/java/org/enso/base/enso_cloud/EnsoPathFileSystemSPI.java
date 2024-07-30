package org.enso.base.enso_cloud;

import org.enso.base.file_system.FileSystemSPI;

/**
 * Registers the `enso://` protocol for resolving file paths.
 *
 * <p>See `Enso_File.new` for more information on path resolution.
 */
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
