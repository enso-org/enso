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
      var arr = new ArrayList<StructureElement>();
      var it = moduleIr.bindings().iterator();
      collectStructure(arr, moduleIr.bindings().iterator());
      return arr;
    } catch (BadLocationException ex) {
      throw new IllegalStateException(ex);
    }
  }

  private static void collectStructure(List<StructureElement> arr, Iterator<? extends IR> it) {
    while (it.hasNext()) {
      var b = it.next();
      collectStructureItem(arr, b);
    }
  }
  private static void collectStructureItem(List<StructureElement> arr, IR b) {
    switch (b) {
      case IR$Module$Scope$Definition$SugaredType type -> {
        var bldr = StructureProvider.newBuilder(type.name().name(), StructureElement.Kind.Class);
        var children = new ArrayList<StructureElement>();
        collectStructure(children, type.body().iterator());
        bldr.children(children);
        var e = bldr.build();
        arr.add(e);
      }

      case IR$Module$Scope$Definition$Data data -> {
        var bldr = StructureProvider.newBuilder(data.name().name(), StructureElement.Kind.Constructor);
        var e = bldr.build();
        arr.add(e);
      }

      default -> {}
    }
  }
}
