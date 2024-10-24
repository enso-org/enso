import org.enso.base.file_format.FileFormatSPI;
import org.enso.image.ImageFormatSPI;

module org.enso.std.image {
  requires org.graalvm.polyglot;
  requires org.enso.std.base;
  requires opencv;

  provides FileFormatSPI with
      ImageFormatSPI;

  opens org.enso.image;
  opens org.enso.image.data;
}
