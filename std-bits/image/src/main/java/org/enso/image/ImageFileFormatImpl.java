package org.enso.image;

import org.enso.base.file_format.FileFormatSPI;

<<<<<<< HEAD:std-bits/image/src/main/java/org/enso/image/ImageFormatSPI.java
public class ImageFormatSPI extends FileFormatSPI {
=======
@org.openide.util.lookup.ServiceProvider(service = FileFormatSPI.class)
public final class ImageFileFormatImpl extends FileFormatSPI {
>>>>>>> origin/develop:std-bits/image/src/main/java/org/enso/image/ImageFileFormatImpl.java
  @Override
  protected String getModuleName() {
    return "Standard.Image.Image_File_Format";
  }

  @Override
  protected String getTypeName() {
    return "Image_File_Format";
  }
}
