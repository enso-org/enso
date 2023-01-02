package org.enso.tools.enso4igv;

import java.io.IOException;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectManager;
import org.netbeans.spi.project.ProjectFactory;
import org.netbeans.spi.project.ProjectFactory2;
import org.netbeans.spi.project.ProjectState;
import org.openide.filesystems.FileObject;
import org.openide.util.ImageUtilities;
import org.openide.util.Lookup;
import org.openide.util.lookup.Lookups;
import org.openide.util.lookup.ServiceProvider;

public class EnsoSbtProject implements Project {
    private final FileObject prj;
    private final ProjectState ps;
    private final Lookup lkp;

    private EnsoSbtProject(FileObject fo, ProjectState ps) {
        this.prj = fo;
        this.ps = ps;
        this.lkp = Lookups.fixed(
            this,
            new EnsoSbtClassPathProvider(this),
            new EnsoLogicalView(this)
        );
    }

    public FileObject getProjectDirectory() {
        return prj;
    }

    public Lookup getLookup() {
        return lkp;
    }

    @Override
    public String toString() {
        return "EnsoSbtProject{prj=" + prj + "}";
    }

    @ServiceProvider(service = ProjectFactory.class)
    public static final class Factory implements ProjectFactory2 {
        public boolean isProject(FileObject fo) {
            return fo.getFileObject(".enso-sources") != null;
        }

        public Project loadProject(FileObject fo, ProjectState ps) throws IOException {
            if (isProject(fo)) {
                return new EnsoSbtProject(fo, ps);
            } else {
                return null;
            }
        }

        public void saveProject(Project prjct) throws IOException, ClassCastException {
        }

        @Override
        public ProjectManager.Result isProject2(FileObject fo) {
            if (isProject(fo)) {
                var img = ImageUtilities.loadImage("org/enso/tools/enso4igv/enso.svg");
                return new ProjectManager.Result(ImageUtilities.image2Icon(img));
            }
            return null;
        }
    }
}
