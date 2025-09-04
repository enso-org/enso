package org.enso.tools.enso4igv;

import org.openide.awt.ActionID;
import org.openide.awt.ActionReference;
import org.openide.awt.ActionReferences;
import static org.enso.tools.enso4igv.EnsoJavaActions.PATH;

@ActionReferences(value = {
    // new file actions
    @ActionReference(position = 1100, separatorAfter = 2000, id = @ActionID(category = "Project", id = "org.netbeans.modules.project.ui.NewFile$WithSubMenu"), path = PATH),
    /*
    // execution actions
    @ActionReference(position = 3100, separatorBefore = 3000, id = @ActionID(category = "Project", id = "org.netbeans.modules.project.ui.RunProject"), path = PATH),
    @ActionReference(position = 3200, id = @ActionID(category = "Debug", id = "org.netbeans.modules.debugger.ui.actions.DebugProjectAction"), path = PATH),
    @ActionReference(position = 3300, id = @ActionID(category = "Project", id = "org.netbeans.modules.project.ui.TestProject"), path = PATH),
    */
    // textual actions
    @ActionReference(position = 4100, separatorBefore = 4000, id = @ActionID(category = "Edit", id = "org.openide.actions.FindAction"), path = PATH),
    // project manipulation
    @ActionReference(position = 4200, id = @ActionID(category = "Project", id = "org.netbeans.modules.project.ui.actions.OpenSubprojects"), path = PATH),
    @ActionReference(position = 4300, id = @ActionID(category = "Project", id = "org.netbeans.modules.project.ui.CloseProject"), path = PATH),
    // customizer
    @ActionReference(position = 5100, separatorBefore = 5000, id = @ActionID(category = "Project", id = "org.netbeans.modules.project.ui.CustomizeProject"), path = PATH)
})
final class EnsoJavaActions {
    static final String ID = "org-enso-tools-sbt";
    static final String PATH = "Projects/" + ID + "/Actions";
}
