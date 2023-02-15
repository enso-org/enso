package org.enso.tools.enso4igv;

import java.util.Arrays;
import java.util.List;
import javax.swing.Action;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import org.enso.tools.enso4igv.EnsoSbtClassPathProvider.EnsoSources;
import org.enso.tools.enso4igv.EnsoSbtClassPathProvider.OtherEnsoSources;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectUtils;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.spi.java.project.support.ui.PackageView;
import org.netbeans.spi.project.ui.LogicalViewProvider;
import org.netbeans.spi.project.ui.support.CommonProjectActions;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.filesystems.*;
import org.openide.nodes.AbstractNode;
import org.openide.nodes.ChildFactory;
import org.openide.nodes.Children;
import org.openide.nodes.Node;
import org.openide.util.NbCollections;
import org.openide.util.lookup.Lookups;

@ActionReferences({
    @ActionReference(position = 3100, id = @ActionID(category = "Project", id = "org-netbeans-modules-project-ui-CloseProject"), path = "Projects/ensosbtprj/Actions", separatorBefore = 3000),
})
final class EnsoLogicalView implements LogicalViewProvider  {
    private final EnsoSbtProject p;

    EnsoLogicalView(EnsoSbtProject p) {
        this.p = p;
    }

    @Override
    public Node createLogicalView() {
        return new EnsoSbtProjectNode(p);
    }

    @Override
    public Node findPath(Node root, Object target) {
        Project prj = root.getLookup().lookup(Project.class);
        if (prj == null) {
            return null;
        }

        if (target instanceof FileObject) {
            FileObject fo = (FileObject) target;
            for (Node n : root.getChildren().getNodes(true)) {
                Node result = PackageView.findPath(n, target);
                if (result != null) {
                    return result;
                }
            }
        }

        return null;
    }

    private static final class EnsoSbtProjectNode extends AbstractNode {

        private final EnsoSbtProject project;

        public EnsoSbtProjectNode(EnsoSbtProject p) {
            super(Children.create(new EnsoRoots(p), true), Lookups.fixed(p));
            this.project = p;
            setDisplayName();
            setIconBaseWithExtension("org/enso/tools/enso4igv/enso.svg");
        }

        private void setDisplayName() {
            setDisplayName(ProjectUtils.getInformation(project).getDisplayName());
        }

        @Override
        public String getHtmlDisplayName() {
            return null;
        }

        @Override
        public Action[] getActions(boolean context) {
            return CommonProjectActions.forType("ensosbtprj"); // NOI18N
        }
    }

    private static final class EnsoRoots extends ChildFactory<SourceGroup> implements ChangeListener {
        private final EnsoSbtProject prj;

        EnsoRoots(EnsoSbtProject project) {
            this.prj = project;
        }

        @Override
        protected boolean createKeys(List<SourceGroup> toPopulate) {
            var arr = Arrays.asList(ProjectUtils.getSources(prj).getSourceGroups(null));
            toPopulate.addAll(arr);
            return true;
        }

        @Override
        protected Node createNodeForKey(SourceGroup key) {
            return PackageView.createPackageView(key);
        }

        @Override
        public void stateChanged(ChangeEvent e) {
            refresh(false);
        }
    }
}
