package org.enso.tools.enso4igv.enso;

import java.util.Collections;
import java.util.List;
import javax.swing.text.Document;
import org.netbeans.api.editor.mimelookup.MimeRegistration;
import org.netbeans.api.lsp.StructureElement;
import org.netbeans.spi.lsp.StructureProvider;

@MimeRegistration(mimeType = "application/x-enso", service = StructureProvider.class)
public final class EnsoStructure implements StructureProvider {
  @Override
  public List<StructureElement> getStructure(Document dcmnt) {
    return Collections.singletonList(StructureProvider.newBuilder("example", StructureElement.Kind.Module).build());
  }
}
