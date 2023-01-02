package org.enso.tools.enso4igv.scala;

import java.io.IOException;
import java.net.URL;
import java.util.Collections;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.netbeans.api.debugger.Breakpoint;
import org.netbeans.api.debugger.DebuggerManager;
import org.netbeans.api.debugger.jpda.LineBreakpoint;
import org.netbeans.spi.debugger.ActionsProvider;
import org.netbeans.spi.debugger.ActionsProviderListener;
import org.netbeans.spi.debugger.jpda.EditorContext;
import org.openide.filesystems.FileObject;
import org.openide.filesystems.URLMapper;
import org.openide.util.Exceptions;

@ActionsProvider.Registration(path="netbeans-JPDASession", actions={ "toggleBreakpoint" }, activateForMIMETypes={ "text/x-scala" })
public class ToggleScalaBreakpointActionProvider extends ActionsProvider {
    @ActionsProvider.Registration(path="", actions={ "toggleBreakpoint" }, activateForMIMETypes={ "text/x-scala" })
    public static class Snd extends ToggleScalaBreakpointActionProvider {
    }

    public ToggleScalaBreakpointActionProvider() {
    }

    @Override
    public Set getActions() {
        return Collections.singleton("toggleBreakpoint");
    }

    @Override
    public void doAction(Object o) {
        final DebuggerManager m = DebuggerManager.getDebuggerManager ();
        for (EditorContext ctx : m.lookup(null, EditorContext.class)) {
            String url = ctx.getCurrentURL();
            int line = ctx.getCurrentLineNumber();
            if (url != null && line >= 0) {
                if (!url.endsWith(".scala")) {
                    continue;
                }
                if (removeExistingBreakpoint(m, line, url)) {
                    return;
                }
                createNewBreakpoint(url, line, m);
                return;
            }
        }
    }

    private static final Pattern PACKAGE = Pattern.compile("package *([\\p{Alnum}+\\.$]+) *");
    private void createNewBreakpoint(String url, int line, final DebuggerManager m) {
        try {
            var b = LineBreakpoint.create(url, line);

            FileObject fo = URLMapper.findFileObject(new URL(url));
            for (String code : fo.asLines()) {
                Matcher match = PACKAGE.matcher(code);
                if (match.matches()) {
                    String pkg = match.group(1);
                    int slash = url.lastIndexOf("/");
                    int dot = url.indexOf('.', slash + 1);
                    if (dot >= 0) {
                        String filter = pkg + "." + url.substring(slash + 1, dot) + "*";
                        b.setPreferredClassName(filter);
                    }
                    break;
                }
            }
            m.addBreakpoint(b);
        } catch (IOException ex) {
            Exceptions.printStackTrace(ex);
        }
    }

    private boolean removeExistingBreakpoint(final DebuggerManager m, int line, String url) {
        for (Breakpoint b : m.getBreakpoints()) {
            if (b instanceof LineBreakpoint lb) {
                if (lb.getLineNumber() == line && url.equals(lb.getURL())) {
                    m.removeBreakpoint(lb);
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public boolean isEnabled(Object o) {
        final DebuggerManager m = DebuggerManager.getDebuggerManager ();
        for (EditorContext ctx : m.lookup(null, EditorContext.class)) {
            String url = ctx.getCurrentURL();
            int line = ctx.getCurrentLineNumber();
            if (url != null && line >= 0) {
                if (url.endsWith(".scala")) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public void addActionsProviderListener(ActionsProviderListener al) {
    }

    @Override
    public void removeActionsProviderListener(ActionsProviderListener al) {
    }

}
