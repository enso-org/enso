package org.enso.compiler.core.ir;

import java.io.IOException;
import java.util.NoSuchElementException;
import org.enso.compiler.core.Persistance;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.core.ir.expression.Foreign;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.core.ir.module.scope.imports.Polyglot;
import org.enso.compiler.core.ir.type.Set;
import org.enso.interpreter.dsl.Persistable;
import org.openide.util.lookup.ServiceProvider;
import scala.Option;
import scala.collection.Iterator;
import scala.collection.SeqFactory;
import scala.collection.immutable.AbstractSeq;
import scala.collection.immutable.List;
import scala.collection.immutable.Seq;

@Persistable(clazz = Polyglot.Java.class, id = 703)
@Persistable(clazz = DefinitionArgument.Specified.class, id = 704)
@Persistable(clazz = Name.Self.class, id = 705)
@Persistable(clazz = Literal.Number.class, id = 706)
@Persistable(clazz = Literal.Text.class, id = 707)
@Persistable(clazz = CallArgument.Specified.class, id = 708)
@Persistable(clazz = Definition.Type.class, id = 709)
@Persistable(clazz = Definition.Data.class, id = 710)
@Persistable(clazz = Name.Blank.class, id = 711)
@Persistable(clazz = Expression.Block.class, id = 751)
@Persistable(clazz = Expression.Binding.class, id = 752)
@Persistable(clazz = Application.Prefix.class, id = 753)
@Persistable(clazz = Application.Force.class, id = 754)
@Persistable(clazz = Application.Sequence.class, id = 755)
@Persistable(clazz = Case.Expr.class, id = 761)
@Persistable(clazz = Case.Branch.class, id = 762)
@Persistable(clazz = Pattern.Constructor.class, id = 763)
@Persistable(clazz = Pattern.Name.class, id = 764)
@Persistable(clazz = Pattern.Literal.class, id = 765)
@Persistable(clazz = Pattern.Type.class, id = 766)
@Persistable(clazz = Method.Conversion.class, id = 771)
@Persistable(clazz = Set.Union.class, id = 772)
@Persistable(clazz = Foreign.Definition.class, id = 781)
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
  public static final class PersistString extends Persistance<String> {
    public PersistString() {
      super(String.class, true, 4437);
    }

    @Override
    protected void writeObject(String obj, Output out) throws IOException {
      out.writeUTF(obj);
    }

    @Override
    protected String readObject(Input in) throws IOException, ClassNotFoundException {
      var obj = in.readUTF();
      return obj;
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
  public static final class PersistScalaSeq extends Persistance<Seq> {
    public PersistScalaSeq() {
      super(Seq.class, true, 4433);
    }

    @Override
    protected void writeObject(Seq list, Output out) throws IOException {
      var size = list.size();
      out.writeInt(size);
      for (var i = 0; i < size; i++) {
        out.writeObject(list.apply(i));
      }
    }

    @Override
    @SuppressWarnings("unchecked")
    protected Seq readObject(Input in) throws IOException, ClassNotFoundException {
      var size = in.readInt();
      Reference<?>[] arr = new Reference<?>[size];
      for (var i = 0; i < size; i++) {
        arr[i] = in.readReference(Object.class);
      }
      return new AbstractSeq() {
        @Override
        public Object apply(int i) throws IndexOutOfBoundsException {
          return arr[i].get(Object.class);
        }

        @Override
        public int length() {
          return size;
        }

        @Override
        public boolean isDefinedAt(int idx) {
          return 0 <= idx && idx < size;
        }

        @Override
        public boolean isDefinedAt(Object idx) {
          throw new IllegalStateException();
        }

        @Override
        public Object apply(Object i) throws IndexOutOfBoundsException {
          throw new IllegalStateException();
        }

        @Override
        public SeqFactory iterableFactory() {
          return super.iterableFactory();
        }

        @Override
        public Iterator iterator() {
          return new Iterator() {
            private int at;

            @Override
            public boolean hasNext() {
              return at < size;
            }

            @Override
            public Object next() throws NoSuchElementException {
              if (at >= size) {
                throw new NoSuchElementException();
              }
              return apply(at++);
            }
          };
        }
      };
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
  public static final class PersistPolyglot extends Persistance<Polyglot> {
    public PersistPolyglot() {
      super(Polyglot.class, false, 343);
    }

    @Override
    protected void writeObject(Polyglot obj, Output out) throws IOException {
      out.writeObject(obj.entity());
      out.writeInline(Option.class, obj.rename());
      out.writeInline(Option.class, obj.location());
      out.writeInline(MetadataStorage.class, obj.passData());
      out.writeInline(DiagnosticStorage.class, obj.diagnostics());
    }

    @Override
    @SuppressWarnings("unchecked")
    protected Polyglot readObject(Input in) throws IOException, ClassNotFoundException {
      var entity = (Polyglot.Entity) in.readObject();
      var rename = in.readInline(Option.class);
      var location = in.readInline(Option.class);
      var meta = in.readInline(MetadataStorage.class);
      var diag = in.readInline(DiagnosticStorage.class);
      return new Polyglot(entity, rename, location, meta, diag);
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistExportModuleStorage extends Persistance<Export.Module> {
    public PersistExportModuleStorage() {
      super(Export.Module.class, false, 344);
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

  @ServiceProvider(service = Persistance.class)
  public static final class PersistMethodExplicit extends Persistance<Method.Explicit> {
    public PersistMethodExplicit() {
      super(Method.Explicit.class, false, 361);
    }

    @Override
    protected void writeObject(Method.Explicit obj, Output out) throws IOException {
      out.writeInline(Name.MethodReference.class, obj.methodReference());
      out.writeInline(Seq.class, obj.bodyList());
      out.writeInline(Option.class, obj.location());
      out.writeInline(MetadataStorage.class, obj.passData());
      out.writeInline(DiagnosticStorage.class, obj.diagnostics());
    }

    @Override
    @SuppressWarnings("unchecked")
    protected Method.Explicit readObject(Input in) throws IOException, ClassNotFoundException {
      var ref = in.readInline(Name.MethodReference.class);
      var bodyList = in.readInline(Seq.class);
      var location = in.readInline(Option.class);
      var meta = in.readInline(MetadataStorage.class);
      var diag = in.readInline(DiagnosticStorage.class);
      return new Method.Explicit(ref, bodyList, location, meta, diag);
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistMethodReference extends Persistance<Name.MethodReference> {
    public PersistMethodReference() {
      super(Name.MethodReference.class, false, 362);
    }

    @Override
    protected void writeObject(Name.MethodReference obj, Output out) throws IOException {
      out.writeInline(Option.class, obj.typePointer());
      out.writeObject(obj.methodName());
      out.writeInline(Option.class, obj.location());
      out.writeInline(MetadataStorage.class, obj.passData());
      out.writeInline(DiagnosticStorage.class, obj.diagnostics());
    }

    @Override
    @SuppressWarnings("unchecked")
    protected Name.MethodReference readObject(Input in) throws IOException, ClassNotFoundException {
      var typePointer = in.readInline(Option.class);
      var methodName = (Name) in.readObject();
      var location = in.readInline(Option.class);
      var meta = in.readInline(MetadataStorage.class);
      var diag = in.readInline(DiagnosticStorage.class);
      return new Name.MethodReference(typePointer, methodName, location, meta, diag);
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistFunctionLambda extends Persistance<Function.Lambda> {
    public PersistFunctionLambda() {
      super(Function.Lambda.class, false, 363);
    }

    @Override
    protected void writeObject(Function.Lambda obj, Output out) throws IOException {
      out.writeInline(List.class, obj.arguments());
      out.writeObject(obj.body());
      out.writeInline(Option.class, obj.location());
      out.writeBoolean(obj.canBeTCO());
      out.writeInline(MetadataStorage.class, obj.passData());
      out.writeInline(DiagnosticStorage.class, obj.diagnostics());
    }

    @Override
    @SuppressWarnings("unchecked")
    protected Function.Lambda readObject(Input in) throws IOException, ClassNotFoundException {
      var arguments = in.readInline(List.class);
      var body = (Expression) in.readObject();
      var location = in.readInline(Option.class);
      var canBeTCO = in.readBoolean();
      var meta = in.readInline(MetadataStorage.class);
      var diag = in.readInline(DiagnosticStorage.class);
      return new Function.Lambda(arguments, body, location, canBeTCO, meta, diag);
    }
  }

  @SuppressWarnings("unchecked")
  private static <T> scala.collection.immutable.List<T> nil() {
    return (scala.collection.immutable.List<T>) scala.collection.immutable.Nil$.MODULE$;
  }

  private static <T> scala.collection.immutable.List<T> join(
      T head, scala.collection.immutable.List<T> tail) {
    return scala.collection.immutable.$colon$colon$.MODULE$.apply(head, tail);
  }
}
