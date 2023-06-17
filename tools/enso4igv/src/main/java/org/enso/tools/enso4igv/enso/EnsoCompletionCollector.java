package org.enso.tools.enso4igv.enso;

import java.util.function.Consumer;
import javax.swing.text.Document;
import org.netbeans.api.editor.mimelookup.MimeRegistration;
import org.netbeans.api.lsp.Completion;
import org.netbeans.spi.lsp.CompletionCollector;

@MimeRegistration(mimeType = "application/x-enso", service = CompletionCollector.class)
public class EnsoCompletionCollector implements CompletionCollector {
  @Override
  public boolean collectCompletions(Document dcmnt, int i, Completion.Context cntxt, Consumer<Completion> cnsmr) {
    cnsmr.accept(CompletionCollector.newBuilder("Hi from Enso!").build());
    return true;
  }
  
}
