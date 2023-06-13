package org.enso.tools.enso4igv;

import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ProjectState;
import org.openide.filesystems.FileObject;
import org.openide.util.Lookup;
import org.openide.util.lookup.Lookups;

public class EnsoSbtProject implements Project {
    private final FileObject prj;
    private final ProjectState ps;
    private final Lookup lkp;

    EnsoSbtProject(FileObject fo, ProjectState ps) {
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
}
