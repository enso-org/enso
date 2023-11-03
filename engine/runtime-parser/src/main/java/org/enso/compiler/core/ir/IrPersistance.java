package org.enso.compiler.core.ir;

import java.io.IOException;
import org.enso.compiler.core.Persistance;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.openide.util.lookup.ServiceProvider;
import scala.Option;
import scala.collection.immutable.List;

public final class IrPersistance {
  private IrPersistance() {}

  @ServiceProvider(service = Persistance.class)
  public static final class PersistIdentifiedLocation extends Persistance<IdentifiedLocation> {
    public PersistIdentifiedLocation() {
      super(IdentifiedLocation.class, false, 2);
    }

    @Override
    protected void writeObject(IdentifiedLocation obj, Output out) throws IOException {
      out.writeInline(Location.class, obj.location());
    }

    @Override
    protected IdentifiedLocation readObject(Input in) throws IOException, ClassNotFoundException {
      var obj = in.readInline(Location.class);
      return new IdentifiedLocation((Location) obj, Option.empty());
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistScalaOption extends Persistance<Option> {
    public PersistScalaOption() {
      super(Option.class, true, 4431);
    }

    @Override
    protected void writeObject(Option obj, Output out) throws IOException {
      out.writeObject(obj.isEmpty() ? null : obj.get());
    }

    @Override
    protected Option readObject(Input in) throws IOException, ClassNotFoundException {
      var obj = in.readObject();
      return Option.apply(obj);
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistScalaList extends Persistance<List> {
    public PersistScalaList() {
      super(List.class, true, 4432);
    }

    @Override
    protected void writeObject(List list, Output out) throws IOException {
      var size = list.size();
      out.writeInt(size);
      var l = list.reverse();
      for (var i = 0; i < size; i++) {
        out.writeObject(l.head());
        l = (List) l.tail();
      }
    }

    @Override
    @SuppressWarnings("unchecked")
    protected List readObject(Input in) throws IOException, ClassNotFoundException {
      var size = in.readInt();
      List list = scala.collection.immutable.Nil$.MODULE$;
      for (var i = 0; i < size; i++) {
        var elem = in.readObject();
        list = scala.collection.immutable.$colon$colon$.MODULE$.apply(elem, list);
      }
      return list;
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistIrModule extends Persistance<Module> {
    public PersistIrModule() {
      super(Module.class, false, 201);
    }

    @Override
    protected void writeObject(Module obj, Output out) throws IOException {
      out.writeInline(List.class, obj.imports());
      out.writeInline(List.class, obj.exports());
      out.writeInline(List.class, obj.bindings());
      out.writeBoolean(obj.isPrivate());
      out.writeInline(Option.class, obj.location());
      out.writeInline(MetadataStorage.class, obj.passData());
      out.writeInline(DiagnosticStorage.class, obj.diagnostics());
    }

    @SuppressWarnings("unchecked")
    @Override
    protected Module readObject(Input in) throws IOException, ClassNotFoundException {
      List<Import> imports = in.readInline(List.class);
      List<Export> exports = in.readInline(List.class);
      List<Definition> bindings = in.readInline(List.class);
      var isPrivate = in.readBoolean();
      Option<IdentifiedLocation> location = in.readInline(Option.class);
      var meta = in.readInline(MetadataStorage.class);
      var diag = in.readInline(DiagnosticStorage.class);
      return new Module(imports, exports, bindings, isPrivate, location, meta, diag);
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistMetadataStorage extends Persistance<MetadataStorage> {
    public PersistMetadataStorage() {
      super(MetadataStorage.class, false, 301);
    }

    @Override
    protected void writeObject(MetadataStorage obj, Output out) throws IOException {}

    @Override
    @SuppressWarnings("unchecked")
    protected MetadataStorage readObject(Input in) throws IOException, ClassNotFoundException {
      return new MetadataStorage(
          (scala.collection.immutable.List) scala.collection.immutable.Nil$.MODULE$);
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistDiagnosticStorage extends Persistance<DiagnosticStorage> {
    public PersistDiagnosticStorage() {
      super(DiagnosticStorage.class, false, 302);
    }

    @Override
    protected void writeObject(DiagnosticStorage obj, Output out) throws IOException {}

    @Override
    @SuppressWarnings("unchecked")
    protected DiagnosticStorage readObject(Input in) throws IOException, ClassNotFoundException {
      return new DiagnosticStorage(
          (scala.collection.immutable.List) scala.collection.immutable.Nil$.MODULE$);
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistNameLiteral extends Persistance<Name.Literal> {
    public PersistNameLiteral() {
      super(Name.Literal.class, false, 351);
    }

    @Override
    protected void writeObject(Name.Literal obj, Output out) throws IOException {
      out.writeUTF(obj.name());
      out.writeBoolean(obj.isMethod());
      out.writeInline(Option.class, obj.location());
      out.writeInline(Option.class, obj.originalName());
      out.writeInline(MetadataStorage.class, obj.passData());
      out.writeInline(DiagnosticStorage.class, obj.diagnostics());
    }

    @Override
    @SuppressWarnings("unchecked")
    protected Name.Literal readObject(Input in) throws IOException, ClassNotFoundException {
      var name = in.readUTF();
      var isMethod = in.readBoolean();
      var location = in.readInline(Option.class);
      var originalName = in.readInline(Option.class);
      var meta = in.readInline(MetadataStorage.class);
      var diag = in.readInline(DiagnosticStorage.class);
      return new Name.Literal(name, isMethod, location, originalName, meta, diag);
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistNameQualified extends Persistance<Name.Qualified> {
    public PersistNameQualified() {
      super(Name.Qualified.class, false, 352);
    }

    @Override
    protected void writeObject(Name.Qualified obj, Output out) throws IOException {
      out.writeInline(List.class, obj.parts());
      out.writeInline(Option.class, obj.location());
      out.writeInline(MetadataStorage.class, obj.passData());
      out.writeInline(DiagnosticStorage.class, obj.diagnostics());
    }

    @Override
    @SuppressWarnings("unchecked")
    protected Name.Qualified readObject(Input in) throws IOException, ClassNotFoundException {
      var parts = in.readInline(List.class);
      var location = in.readInline(Option.class);
      var meta = in.readInline(MetadataStorage.class);
      var diag = in.readInline(DiagnosticStorage.class);
      return new Name.Qualified(parts, location, meta, diag);
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistImportModuleStorage extends Persistance<Import.Module> {
    public PersistImportModuleStorage() {
      super(Import.Module.class, false, 342);
    }

    @Override
    protected void writeObject(Import.Module obj, Output out) throws IOException {
      out.writeInline(Name.Qualified.class, obj.name());
      out.writeInline(Option.class, obj.rename());
      out.writeBoolean(obj.isAll());
      out.writeInline(Option.class, obj.onlyNames());
      out.writeInline(Option.class, obj.hiddenNames());
      out.writeInline(Option.class, obj.location());
      out.writeBoolean(obj.isSynthetic());
      out.writeInline(MetadataStorage.class, obj.passData());
      out.writeInline(DiagnosticStorage.class, obj.diagnostics());
    }

    @Override
    @SuppressWarnings("unchecked")
    protected Import.Module readObject(Input in) throws IOException, ClassNotFoundException {
      var name = in.readInline(Name.Qualified.class);
      var rename = in.readInline(Option.class);
      var isAll = in.readBoolean();
      var onlyNames = in.readInline(Option.class);
      var hiddenNames = in.readInline(Option.class);
      var location = in.readInline(Option.class);
      var isSynthetic = in.readBoolean();
      var meta = in.readInline(MetadataStorage.class);
      var diag = in.readInline(DiagnosticStorage.class);
      return new Import.Module(
          name, rename, isAll, onlyNames, hiddenNames, location, isSynthetic, meta, diag);
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistExportModuleStorage extends Persistance<Export.Module> {
    public PersistExportModuleStorage() {
      super(Export.Module.class, false, 343);
    }

    @Override
    protected void writeObject(Export.Module obj, Output out) throws IOException {
      out.writeInline(Name.Qualified.class, obj.name());
      out.writeInline(Option.class, obj.rename());
      out.writeBoolean(obj.isAll());
      out.writeInline(Option.class, obj.onlyNames());
      out.writeInline(Option.class, obj.hiddenNames());
      out.writeInline(Option.class, obj.location());
      out.writeBoolean(obj.isSynthetic());
      out.writeInline(MetadataStorage.class, obj.passData());
      out.writeInline(DiagnosticStorage.class, obj.diagnostics());
    }

    @Override
    @SuppressWarnings("unchecked")
    protected Export.Module readObject(Input in) throws IOException, ClassNotFoundException {
      var name = in.readInline(Name.Qualified.class);
      var rename = in.readInline(Option.class);
      var isAll = in.readBoolean();
      var onlyNames = in.readInline(Option.class);
      var hiddenNames = in.readInline(Option.class);
      var location = in.readInline(Option.class);
      var isSynthetic = in.readBoolean();
      var meta = in.readInline(MetadataStorage.class);
      var diag = in.readInline(DiagnosticStorage.class);
      return new Export.Module(
          name, rename, isAll, onlyNames, hiddenNames, location, isSynthetic, meta, diag);
    }
  }
}
