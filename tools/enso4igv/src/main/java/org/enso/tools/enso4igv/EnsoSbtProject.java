package org.enso.tools.enso4igv;

import java.awt.GraphicsEnvironment;
import java.util.Collections;
import java.util.Set;
import javax.swing.event.ChangeListener;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ProjectContainerProvider;
import org.netbeans.spi.project.ProjectState;
import org.netbeans.spi.project.SubprojectProvider;
import org.openide.filesystems.FileObject;
import org.openide.util.Lookup;
import org.openide.util.lookup.Lookups;

final class EnsoSbtProject implements Project {
    private final FileObject prj;
    private final ProjectState ps;
    private final Lookup lkp;

    EnsoSbtProject(FileObject fo, ProjectState ps) {
        this.prj = fo;
        this.ps = ps;
        this.lkp = Lookups.fixed(
            this,
            new EnsoSbtClassPathProvider(this),
            new EnsoLogicalView(this),
            new OwnSubproject()
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

    private final class OwnSubproject implements ProjectContainerProvider, SubprojectProvider {
        @Override
        public Set<? extends Project> getSubprojects() {
            if (GraphicsEnvironment.isHeadless()) {
                return Collections.emptySet();
            } else {
                return Collections.singleton(EnsoSbtProject.this);
            }
        }

        @Override
        public void addChangeListener(ChangeListener cl) {
        }

        @Override
        public void removeChangeListener(ChangeListener cl) {
        }

        @Override
        public ProjectContainerProvider.Result getContainedProjects() {
            var result = new ProjectContainerProvider.Result(getSubprojects(), false);
            return result;
        }
    }
}
