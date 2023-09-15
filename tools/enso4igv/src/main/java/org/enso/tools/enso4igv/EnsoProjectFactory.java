package org.enso.tools.enso4igv;

import java.io.IOException;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.spi.project.ProjectFactory;
import org.netbeans.spi.project.ProjectFactory2;
import org.netbeans.spi.project.ProjectState;
import org.openide.filesystems.FileObject;
import org.openide.util.ImageUtilities;
import org.openide.util.lookup.ServiceProvider;

@ServiceProvider(service = ProjectFactory.class, position = 135)
public final class EnsoProjectFactory implements ProjectFactory2 {
  static int isProjectCheck(FileObject fo) {
    if (!fo.isFolder()) {
      return 0;
    }
    if (fo.getFileObject(".enso-sources") != null) {
      return 1;
    } else if (
      fo.getFileObject("README.md") != null &&
      fo.getFileObject("build.sbt") != null &&
      fo.getFileObject("engine/runtime") != null
    ) {
      return 2;
    } else {
      return 0;
    }
  }

  private static Project createProjectOrNull(FileObject fo, ProjectState ps) {
    return switch (isProjectCheck(fo)) {
      case 1 -> new EnsoSbtProject(fo, ps);
      case 2 -> new EnsoRootProject(fo, ps);
      default -> null;
    };
  }

  @Override
  public boolean isProject(FileObject fo) {
    return isProjectCheck(fo) != 0;
  }
  
  @Override
  public Project loadProject(FileObject fo, ProjectState ps) throws IOException {
    return createProjectOrNull(fo, ps);
  }


  public void saveProject(Project prjct) throws IOException, ClassCastException {
  }

  @Override
  public ProjectManager.Result isProject2(FileObject fo) {
    if (isProject(fo)) {
      java.awt.Image img = ImageUtilities.loadImage("org/enso/tools/enso4igv/enso.svg");
      return new ProjectManager.Result(ImageUtilities.image2Icon(img));
    }
    return null;
  }
  
}
