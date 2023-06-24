package org.enso.tools.enso4igv.enso;

import java.util.ArrayList;
import java.util.List;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import org.enso.compiler.core.EnsoParser;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.IR$Module$Scope$Definition$Data;
import org.enso.compiler.core.IR$Module$Scope$Definition$SugaredType;
import org.netbeans.api.editor.mimelookup.MimeRegistration;
import org.netbeans.api.lsp.StructureElement;
import org.netbeans.spi.lsp.StructureProvider;
import scala.collection.Iterator;

@MimeRegistration(mimeType = "application/x-enso", service = StructureProvider.class)
public final class EnsoStructure implements StructureProvider {
  @Override
  public List<StructureElement> getStructure(Document dcmnt) {
    try {
      var parser = new EnsoParser();
      var text = dcmnt.getText(0, dcmnt.getLength());
      var moduleIr = parser.compile(text);
      var arr = new ArrayList();
      var it = moduleIr.bindings().iterator();
      collectStructure(arr, moduleIr.bindings().iterator());
      return arr;
    } catch (BadLocationException ex) {
      throw new IllegalStateException(ex);
    }
  }

  private static void collectStructure(ArrayList arr, Iterator<? extends IR> it) {
    while (it.hasNext()) {
      var b = it.next();
      collectStructureItem(arr, b);
    }
  }
  private static void collectStructureItem(ArrayList arr, IR b) {
    switch (b) {
      case IR$Module$Scope$Definition$SugaredType type -> {
        var e = StructureProvider.newBuilder(type.name().name(), StructureElement.Kind.Class).build();
        arr.add(e);
        collectStructure(arr, type.body().iterator());
      }
      case IR$Module$Scope$Definition$Data data -> {
        var e = StructureProvider.newBuilder(data.name().name(), StructureElement.Kind.Constructor).build();
        arr.add(e);
      }
      default -> {}
    }
  }
}
