package org.enso.tools.enso4igv.enso;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import org.enso.compiler.core.EnsoParser;
import org.enso.compiler.core.ir.IdentifiedLocation;
import org.enso.compiler.core.ir.expression.errors.Syntax;
import org.netbeans.api.editor.mimelookup.MimeRegistration;
import org.netbeans.api.lsp.Diagnostic;
import org.netbeans.spi.lsp.ErrorProvider;
import org.openide.cookies.EditorCookie;
import scala.Function1;

@MimeRegistration(mimeType = "application/x-enso", service = ErrorProvider.class)
public final class EnsoErrorProvider implements ErrorProvider {

    private static final Logger LOG = Logger.getLogger(EnsoErrorProvider.class.getName());

    @Override
    public List<? extends Diagnostic> computeErrors(Context ctx) {
        var arr = new ArrayList<Diagnostic>();
        try {
            if (ctx.errorKind() == Kind.ERRORS) {
                LOG.log(Level.FINE, "Processing errors for {0}", ctx.file().getPath());
                var text = toText(ctx);
                Function1<IdentifiedLocation, String> where = (loc) -> {
                    return text.substring(loc.start(), loc.end());
                };
                var moduleIr = EnsoParser.compile(text);
                moduleIr.preorder().foreach((p) -> {
                    if (p instanceof Syntax err && err.location().isDefined()) {
                        var loc = err.location().get();
                        var msg = err.message(where);
                        var builder = Diagnostic.Builder.create(() -> loc.start(), () -> loc.end(), msg);
                        builder.setSeverity(Diagnostic.Severity.Error);
                        var code = where.apply(loc);
                        LOG.log(Level.FINE, "Error with code {0}", code);
                        builder.setCode(code);
                        arr.add(builder.build());
                    }
                    return null;
                });
                LOG.log(Level.FINE, "Found {1} errors in {0}", new Object[]{ctx.file().getPath(), arr.size()});
            }
        } catch (IOException | BadLocationException ex) {
            LOG.log(Level.WARNING, "Error accessing " + ctx.file(), ex);
        }
        return arr;
    }

    private String toText(Context ctx) throws IOException, BadLocationException {
        var ec = ctx.file().getLookup().lookup(EditorCookie.class);
        if (ec != null && ec.getDocument() instanceof Document doc) {
            LOG.log(Level.INFO, "File {0} loaded from document", ctx.file().getPath());
            return doc.getText(0, doc.getLength());
        } else {
            LOG.log(Level.INFO, "File {0} loaded from disk", ctx.file().getPath());
            return ctx.file().asText();
        }
    }
}
