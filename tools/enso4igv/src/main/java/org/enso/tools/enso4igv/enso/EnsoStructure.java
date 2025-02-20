package org.enso.tools.enso4igv.enso;

import java.util.ArrayList;
import java.util.List;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import org.enso.compiler.core.EnsoParser;
import org.enso.compiler.core.IR;
import org.enso.compiler.core.ir.Function;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.netbeans.api.editor.mimelookup.MimeRegistration;
import org.netbeans.api.lsp.StructureElement;
import org.netbeans.spi.lsp.StructureProvider;
import org.openide.filesystems.FileObject;
import org.openide.util.Lookup;
import scala.collection.Iterator;

@MimeRegistration(mimeType = "application/x-enso", service = StructureProvider.class)
public final class EnsoStructure implements StructureProvider {
  @Override
  public List<StructureElement> getStructure(Document dcmnt) {
      return collectStructure(dcmnt);
  }

  static List<StructureElement> collectStructure(Document dcmnt) {
    FileObject file = null;
    if (dcmnt.getProperty(Document.StreamDescriptionProperty) instanceof Lookup.Provider p) {
      if (p.getLookup().lookup(FileObject.class) instanceof FileObject fo) {
        file = fo;
      }
    }
    var arr = new ArrayList<StructureElement>();
    try {
      var text = dcmnt.getText(0, dcmnt.getLength());
      var moduleIr = EnsoParser.compile(text);
      var it = moduleIr.bindings().iterator();
      collectStructure(file, arr, it);
      return arr;
    } catch (LinkageError err) {
      err.printStackTrace();
      throw new IllegalStateException(err);
    } catch (BadLocationException ex) {
      throw new IllegalStateException(ex);
    }
  }

  private static void collectStructure(FileObject file, List<StructureElement> arr, Iterator<? extends IR> it) {
    while (it.hasNext()) {
      var b = it.next();
      collectStructureItem(file, arr, b);
    }
  }
  private static void collectStructureItem(FileObject file, List<StructureElement> arr, IR ir) {
    var b = switch (ir) {
      case Definition.SugaredType type -> {
        var bldr = StructureProvider.newBuilder(type.name().name(), StructureElement.Kind.Class);
        var children = new ArrayList<StructureElement>();
        collectStructure(file, children, type.body().iterator());
        bldr.children(children);
        yield bldr;
      }

      case Definition.Data data -> {
        var bldr = StructureProvider.newBuilder(data.name().name(), StructureElement.Kind.Constructor);
        yield bldr;
      }

      case Function.Binding funcBinding ->  {
        var bldr = StructureProvider.newBuilder(funcBinding.name().name(), StructureElement.Kind.Method);
        yield bldr;
      }

      case Method.Binding bind -> {
        var bldr = StructureProvider.newBuilder(bind.methodName().name(), StructureElement.Kind.Method);
        yield bldr;
      }
      default -> null;
    };
    if (b != null) {
      if (ir.location() != null && ir.location().isDefined()) {
        var loc = ir.location().get().location();
        b.selectionStartOffset(loc.start());
        b.selectionEndOffset(loc.end());
        b.expandedStartOffset(loc.start());
        b.expandedEndOffset(loc.end());
      }
      b.file(file);
      var e = b.build();
      arr.add(e);
    }
  }
}
