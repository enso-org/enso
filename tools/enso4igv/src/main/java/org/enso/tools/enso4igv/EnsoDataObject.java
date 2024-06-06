package org.enso.tools.enso4igv;

import java.beans.PropertyChangeListener;
import java.io.IOException;
import java.util.Set;
import javax.swing.Icon;
import javax.swing.event.ChangeListener;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.SourceGroup;
import org.netbeans.api.project.Sources;
import org.netbeans.core.spi.multiview.MultiViewElement;
import org.netbeans.core.spi.multiview.text.MultiViewEditorElement;
import org.netbeans.modules.textmate.lexer.api.GrammarRegistration;
import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.MIMEResolver;
import org.openide.loaders.DataObject;
import org.openide.loaders.DataObjectExistsException;
import org.openide.loaders.MultiDataObject;
import org.openide.loaders.MultiFileLoader;
import org.openide.util.Lookup;
import org.openide.util.NbBundle.Messages;
import org.openide.util.lookup.Lookups;
import org.openide.windows.TopComponent;

@Messages({
    "LBL_Enso_LOADER=Files of Enso"
})
@MIMEResolver.ExtensionRegistration(
        displayName = "#LBL_Enso_LOADER",
        mimeType = "application/x-enso",
        extension = {"enso"},
        position = 79753
)
@GrammarRegistration(mimeType = "application/x-enso", grammar = "enso.tmLanguage.json")
@DataObject.Registration(
        mimeType = "application/x-enso",
        iconBase = "org/enso/tools/enso4igv/enso.svg",
        displayName = "#LBL_Enso_LOADER",
        position = 300
)
@ActionReferences({
    @ActionReference(
            path = "Loaders/application/x-enso/Actions",
            id = @ActionID(category = "System", id = "org.openide.actions.OpenAction"),
            position = 100,
            separatorAfter = 200
    ),
    @ActionReference(
            path = "Loaders/application/x-enso/Actions",
            id = @ActionID(category = "Project", id = "org.netbeans.modules.project.ui.RunSingle"),
            position = 230
    ),
    @ActionReference(
            path = "Loaders/application/x-enso/Actions",
            id = @ActionID(category = "Debug", id = "org.netbeans.modules.debugger.ui.actions.DebugFileAction"),
            position = 270,
            separatorAfter = 290
    ),
    @ActionReference(
            path = "Loaders/application/x-enso/Actions",
            id = @ActionID(category = "Edit", id = "org.openide.actions.CutAction"),
            position = 300
    ),
    @ActionReference(
            path = "Loaders/application/x-enso/Actions",
            id = @ActionID(category = "Edit", id = "org.openide.actions.CopyAction"),
            position = 400,
            separatorAfter = 500
    ),
    @ActionReference(
            path = "Loaders/application/x-enso/Actions",
            id = @ActionID(category = "Edit", id = "org.openide.actions.DeleteAction"),
            position = 600
    ),
    @ActionReference(
            path = "Loaders/application/x-enso/Actions",
            id = @ActionID(category = "System", id = "org.openide.actions.RenameAction"),
            position = 700,
            separatorAfter = 800
    ),
    @ActionReference(
            path = "Loaders/application/x-enso/Actions",
            id = @ActionID(category = "System", id = "org.openide.actions.SaveAsTemplateAction"),
            position = 900,
            separatorAfter = 1000
    ),
    @ActionReference(
            path = "Loaders/application/x-enso/Actions",
            id = @ActionID(category = "System", id = "org.openide.actions.FileSystemAction"),
            position = 1100,
            separatorAfter = 1200
    ),
    @ActionReference(
            path = "Loaders/application/x-enso/Actions",
            id = @ActionID(category = "System", id = "org.openide.actions.ToolsAction"),
            position = 1300
    ),
    @ActionReference(
            path = "Loaders/application/x-enso/Actions",
            id = @ActionID(category = "System", id = "org.openide.actions.PropertiesAction"),
            position = 1400
    ),

    // editor popups
    @ActionReference(
            path = "Editors/application/x-enso/Popup",
            id = @ActionID(category = "Project", id = "org.netbeans.modules.project.ui.RunSingle"),
            position = 30
    ),
    @ActionReference(
            path = "Editors/application/x-enso/Popup",
            id = @ActionID(category = "Debug", id = "org.netbeans.modules.debugger.ui.actions.DebugFileAction"),
            position = 70,
            separatorAfter = 90
    ),

})
public class EnsoDataObject extends MultiDataObject {

    public EnsoDataObject(FileObject pf, MultiFileLoader loader) throws DataObjectExistsException, IOException {
        super(pf, loader);
        registerEditor("application/x-enso", true);
        registerTruffleMimeType("application/x-enso");
    }

    @Override
    protected int associateLookup() {
        return 1;
    }

    @MultiViewElement.Registration(
            displayName = "#LBL_Enso_EDITOR",
            iconBase = "org/enso/tools/enso4igv/enso.svg",
            mimeType = "application/x-enso",
            persistenceType = TopComponent.PERSISTENCE_ONLY_OPENED,
            preferredID = "Enso",
            position = 1000
    )
    @Messages("LBL_Enso_EDITOR=Source")
    public static MultiViewEditorElement createEditor(Lookup lkp) {
        return new MultiViewEditorElement(lkp);
    }

    private void registerTruffleMimeType(String mime) throws IOException {
    }
}
