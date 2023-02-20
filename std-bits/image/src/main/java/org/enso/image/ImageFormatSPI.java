package org.enso.image;

import org.enso.base.file_format.FileFormatSPI;

@org.openide.util.lookup.ServiceProvider(service = FileFormatSPI.class)
public class ImageFormatSPI extends FileFormatSPI {
  @Override
  protected String getModuleName() {
    return "Standard.Image.Image_File_Format";
  }

  @Override
  protected String getTypeName() {
    return "Image_File_Format";
  }
}
