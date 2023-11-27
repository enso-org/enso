package org.enso.compiler.core.ir;

import java.io.IOException;
import java.util.Map;
import java.util.UUID;
import org.enso.compiler.core.ir.expression.Application;
import org.enso.compiler.core.ir.expression.Case;
import org.enso.compiler.core.ir.expression.Foreign;
import org.enso.compiler.core.ir.expression.warnings.Unused;
import org.enso.compiler.core.ir.module.scope.Definition;
import org.enso.compiler.core.ir.module.scope.Export;
import org.enso.compiler.core.ir.module.scope.Import;
import org.enso.compiler.core.ir.module.scope.definition.Method;
import org.enso.compiler.core.ir.module.scope.imports.Polyglot;
import org.enso.compiler.core.ir.type.Set;
import org.enso.persist.Persistable;
import org.enso.persist.Persistance;
import org.openide.util.lookup.ServiceProvider;
import scala.Option;
import scala.Tuple2;
import scala.collection.immutable.List;
import scala.collection.immutable.Seq;

@Persistable(clazz = Module.class, id = 201)
@Persistable(clazz = Name.Literal.class, id = 351)
@Persistable(clazz = Import.Module.class, id = 342)
@Persistable(clazz = Polyglot.class, id = 343)
@Persistable(clazz = Export.Module.class, id = 344)
@Persistable(clazz = Name.Qualified.class, id = 352)
@Persistable(clazz = Method.Explicit.class, id = 361)
@Persistable(clazz = Name.MethodReference.class, id = 362)
@Persistable(clazz = Function.Lambda.class, id = 363)
@Persistable(clazz = Polyglot.Java.class, id = 703)
@Persistable(clazz = DefinitionArgument.Specified.class, id = 704)
@Persistable(clazz = Name.Self.class, id = 705)
@Persistable(clazz = Literal.Number.class, id = 706)
@Persistable(clazz = Literal.Text.class, id = 707)
@Persistable(clazz = CallArgument.Specified.class, id = 708)
@Persistable(clazz = Definition.Type.class, id = 709)
@Persistable(clazz = Definition.Data.class, id = 710)
@Persistable(clazz = Name.Blank.class, id = 711)
@Persistable(clazz = Name.GenericAnnotation.class, id = 712)
@Persistable(clazz = Name.SelfType.class, id = 713)
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
@Persistable(clazz = Set.Intersection.class, id = 773)
@Persistable(clazz = Foreign.Definition.class, id = 781)
@Persistable(clazz = Type.Function.class, id = 782)
@Persistable(clazz = Name.BuiltinAnnotation.class, id = 783)
@Persistable(clazz = Type.Error.class, id = 784)
@Persistable(clazz = Unused.Binding.class, id = 785)
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
      out.writeInline(Option.class, obj.id());
    }

    @Override
    @SuppressWarnings("unchecked")
    protected IdentifiedLocation readObject(Input in) throws IOException, ClassNotFoundException {
      var obj = in.readInline(Location.class);
      var id = in.readInline(Option.class);
      return IdentifiedLocation.create((Location) obj, id);
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistUUID extends Persistance<UUID> {
    public PersistUUID() {
      super(UUID.class, false, 73);
    }

    @Override
    protected void writeObject(UUID obj, Output out) throws IOException {
      out.writeLong(obj.getLeastSignificantBits());
      out.writeLong(obj.getMostSignificantBits());
    }

    @Override
    protected UUID readObject(Input in) throws IOException, ClassNotFoundException {
      var least = in.readLong();
      var most = in.readLong();
      return new UUID(most, least);
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
  public static final class PersistLong extends Persistance<Long> {
    public PersistLong() {
      super(Long.class, true, 4438);
    }

    @Override
    protected void writeObject(Long obj, Output out) throws IOException {
      out.writeLong(obj);
    }

    @Override
    protected Long readObject(Input in) throws IOException, ClassNotFoundException {
      var obj = in.readLong();
      return obj;
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistDouble extends Persistance<Double> {
    public PersistDouble() {
      super(Double.class, true, 4439);
    }

    @Override
    protected void writeObject(Double obj, Output out) throws IOException {
      out.writeDouble(obj);
    }

    @Override
    protected Double readObject(Input in) throws IOException, ClassNotFoundException {
      var obj = in.readDouble();
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
  public static final class PersistScalaMap extends Persistance<scala.collection.immutable.Map> {
    public PersistScalaMap() {
      super(scala.collection.immutable.Map.class, true, 4444);
    }

    @Override
    @SuppressWarnings("unchecked")
    protected void writeObject(scala.collection.immutable.Map map, Output out) throws IOException {
      var size = map.size();
      out.writeInt(size);
      var it = map.iterator();
      while (size-- > 0) {
        var tuple = (Tuple2) it.next();
        out.writeObject(tuple._1());
        out.writeObject(tuple._2());
      }
    }

    @Override
    @SuppressWarnings("unchecked")
    protected scala.collection.immutable.Map readObject(Input in)
        throws IOException, ClassNotFoundException {
      var map = new IrLazyMap(in);
      var immutableMap = new IrLazyImMap(map);
      return immutableMap;
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistScalaMutableMap
      extends Persistance<scala.collection.mutable.Map> {
    public PersistScalaMutableMap() {
      super(scala.collection.mutable.Map.class, true, 4949);
    }

    @Override
    @SuppressWarnings("unchecked")
    protected void writeObject(scala.collection.mutable.Map map, Output out) throws IOException {
      var size = map.size();
      out.writeInt(size);
      var it = map.iterator();
      while (it.hasNext()) {
        var tuple = (Tuple2) it.next();
        out.writeObject(tuple._1());
        out.writeObject(tuple._2());
      }
    }

    @Override
    @SuppressWarnings("unchecked")
    protected scala.collection.mutable.Map readObject(Input in)
        throws IOException, ClassNotFoundException {
      var size = in.readInt();
      var map = scala.collection.mutable.Map$.MODULE$.empty();
      for (var i = 0; i < size; i++) {
        var key = in.readObject();
        var value = in.readObject();
        map.put(key, value);
      }
      return map;
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistScalaSet extends Persistance<scala.collection.immutable.Set> {
    public PersistScalaSet() {
      super(scala.collection.immutable.Set.class, true, 4445);
    }

    @Override
    @SuppressWarnings("unchecked")
    protected void writeObject(scala.collection.immutable.Set set, Output out) throws IOException {
      var size = set.size();
      out.writeInt(size);
      var it = set.iterator();
      while (it.hasNext()) {
        var obj = it.next();
        out.writeObject(obj);
      }
    }

    @Override
    @SuppressWarnings("unchecked")
    protected scala.collection.immutable.Set readObject(Input in)
        throws IOException, ClassNotFoundException {
      var size = in.readInt();
      var map = scala.collection.immutable.Set$.MODULE$.empty();
      for (var i = 0; i < size; i++) {
        var elem = in.readObject();
        map = map.$plus(elem);
      }
      return map;
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistMap extends Persistance<Map> {
    public PersistMap() {
      super(Map.class, true, 4440);
    }

    @Override
    protected void writeObject(Map m, Output out) throws IOException {
      var size = m.size();
      out.writeInt(size);
      var it = m.entrySet().iterator();
      while (it.hasNext()) {
        var entry = (Map.Entry) it.next();
        out.writeObject(entry.getKey());
        out.writeObject(entry.getValue());
      }
    }

    @Override
    @SuppressWarnings("unchecked")
    protected Map readObject(Input in) throws IOException, ClassNotFoundException {
      return new IrLazyMap(in);
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
      return new IrLazySeq(arr, size);
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistMetadataStorage extends Persistance<MetadataStorage> {
    public PersistMetadataStorage() {
      super(MetadataStorage.class, false, 381);
    }

    @Override
    @SuppressWarnings("unchecked")
    protected void writeObject(MetadataStorage obj, Output out) throws IOException {
      var map =
          obj.map(
              (processingPass, data) -> {
                var t = new Tuple2<>(processingPass, data);
                return t;
              });
      out.writeInline(scala.collection.immutable.Map.class, map);
    }

    @Override
    @SuppressWarnings("unchecked")
    protected MetadataStorage readObject(Input in) throws IOException, ClassNotFoundException {
      var map = in.readInline(scala.collection.immutable.Map.class);
      var storage = new MetadataStorage(map);
      return storage;
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

  @SuppressWarnings("unchecked")
  private static <T> scala.collection.immutable.List<T> nil() {
    return (scala.collection.immutable.List<T>) scala.collection.immutable.Nil$.MODULE$;
  }

  private static <T> scala.collection.immutable.List<T> join(
      T head, scala.collection.immutable.List<T> tail) {
    return scala.collection.immutable.$colon$colon$.MODULE$.apply(head, tail);
  }

  @SuppressWarnings("unchecked")
  private static <E extends Throwable> E raise(Class<E> clazz, Throwable t) throws E {
    throw (E) t;
  }
}
