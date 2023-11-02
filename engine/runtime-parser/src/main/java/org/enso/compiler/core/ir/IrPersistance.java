package org.enso.compiler.core.ir;

import java.io.IOException;
import org.enso.compiler.core.Persistance;
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
}
