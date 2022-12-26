package org.enso.tools.enso4igv;

import java.net.URL;
import javax.swing.event.ChangeListener;
import org.netbeans.api.java.queries.SourceForBinaryQuery;
import org.netbeans.spi.java.queries.SourceForBinaryQueryImplementation;
import org.netbeans.spi.java.queries.SourceForBinaryQueryImplementation2;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.FileUtil;
import org.openide.filesystems.URLMapper;
import org.openide.util.lookup.ServiceProvider;

@ServiceProvider(service = SourceForBinaryQueryImplementation.class)
public final class NextToSourceForBinaryQueryImpl implements SourceForBinaryQueryImplementation {
  @Override
  public SourceForBinaryQuery.Result findSourceRoots(URL binaryRoot) {
    var file = FileUtil.getArchiveFile(binaryRoot);
    if (file != null) {
      var fo = URLMapper.findFileObject(file);
      if (fo != null) {
        var src = fo.getParent().getFileObject(fo.getName() + "-sources", fo.getExt());
        if (src != null) {
          return new SourceForBinaryQueryImplementation2.Result() {
            @Override
            public boolean preferSources() {
              return false;
            }

            @Override
            public FileObject[] getRoots() {
              return new FileObject[] { FileUtil.getArchiveRoot(src) };
            }

            @Override
            public void addChangeListener(ChangeListener l) {
            }

            @Override
            public void removeChangeListener(ChangeListener l) {
            }
          };
        }
      }
    }
    return null;
  }
}
