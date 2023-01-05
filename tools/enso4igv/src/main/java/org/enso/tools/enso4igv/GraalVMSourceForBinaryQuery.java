package org.enso.tools.enso4igv;

import java.net.URL;
import java.net.MalformedURLException;
import javax.swing.event.ChangeListener;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.URLMapper;
import org.netbeans.api.java.queries.SourceForBinaryQuery;
import org.netbeans.spi.java.queries.SourceForBinaryQueryImplementation;
import org.netbeans.spi.java.queries.SourceForBinaryQueryImplementation2;
import org.openide.util.Exceptions;

@org.openide.util.lookup.ServiceProvider(service = SourceForBinaryQueryImplementation.class, position = 100000)
public class GraalVMSourceForBinaryQuery implements SourceForBinaryQueryImplementation2 {

  public GraalVMSourceForBinaryQuery() {
  }

  @Override
  public SourceForBinaryQueryImplementation2.Result findSourceRoots2(URL binaryRoot) {
    var binaryRootS = binaryRoot.toExternalForm();
    String srcZipS = null;
    String srcZipIn = null;
    if (binaryRootS.startsWith("nbjrt:")) {
      int end = binaryRootS.indexOf('!');
      if (end >= 0) {
        srcZipS = binaryRootS.substring(6, end) + "lib/src.zip";
        String reminder = binaryRootS.substring(end + 1);
        final String prefix = "/modules/";
        if (reminder.startsWith(prefix)) {
          srcZipIn = reminder.substring(prefix.length());
        }
      }
    }
    if (srcZipS != null) {
      try {
        URL srcZip = FileUtil.getArchiveRoot(new URL(srcZipS));
        FileObject fo = URLMapper.findFileObject(srcZip);
        if (fo != null) {
          if (srcZipIn != null) {
            fo = fo.getFileObject(srcZipIn);
          }
          if (fo != null) {
            return new R(fo);
          }
        }
      } catch (MalformedURLException mue) {
        Exceptions.printStackTrace(mue);
      }
    }
    return null;
  }

  @Override
  public SourceForBinaryQuery.Result findSourceRoots(URL binaryRoot) {
    return this.findSourceRoots2(binaryRoot);
  }

  private static class R implements SourceForBinaryQueryImplementation2.Result {
    private final FileObject root;

    private R(FileObject fo) {
      root = fo;
    }

    @Override
    public FileObject[] getRoots() {
      return root.isValid() ? new FileObject[]{root} : new FileObject[0];
    }

    @Override
    public void addChangeListener(ChangeListener l) {
    }

    @Override
    public void removeChangeListener(ChangeListener l) {
    }

    @Override
    public boolean preferSources() {
      return false;
    }
  }
}
