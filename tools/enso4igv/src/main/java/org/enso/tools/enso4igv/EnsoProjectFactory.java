package org.enso.tools.enso4igv;

import java.awt.Image;
import java.io.IOException;
import org.enso.tools.enso4igv.enso.EnsoYamlProject;
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
    if (!fo.isFolder() || fo.getName().contains("bazel")) {
      return 0;
    }
    var yaml = fo.getFileObject("package.yaml");
    if (yaml != null && !fo.getNameExt().equals("0.0.0-dev")) {
        return 3;
    }
    var dev000 = fo.getFileObject("0.0.0-dev/package.yaml");
    if (dev000 != null) {
        return 3;
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

  private static Project createProjectOrNull(FileObject fo, ProjectState ps) throws IOException {
    return switch (isProjectCheck(fo)) {
      case 1 -> new EnsoSbtProject(fo, ps);
      case 2 -> new EnsoRootProject(fo, ps);
      case 3 -> EnsoYamlProject.create(fo, ps);
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
    var img = findImageForType(fo);
    return img == null ? null : new ProjectManager.Result(ImageUtilities.image2Icon(img));
  }

    private static Image findImageForType(FileObject fo) {
        var img = switch (isProjectCheck(fo)) {
            case 1 -> ImageUtilities.loadImage("org/enso/tools/enso4igv/enso-duke.svg");
            case 2, 3 -> ImageUtilities.loadImage("org/enso/tools/enso4igv/enso.svg");
            default -> null;
        };  return img;
    }

}
