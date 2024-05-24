package org.enso.compiler.pass.analyse.types;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.enso.persist.Persistable;
import org.enso.persist.Persistance;
import org.openide.util.lookup.ServiceProvider;

@Persistable(clazz = InferredType.class, id = 34000)
@Persistable(clazz = TypeRepresentation.TopType.class, id = 34001)
@Persistable(clazz = TypeRepresentation.TypeObject.class, id = 34002)
@Persistable(clazz = TypeRepresentation.ArrowType.class, id = 34003)
@Persistable(clazz = TypeRepresentation.AtomType.class, id = 34004)
@Persistable(clazz = TypeRepresentation.IntersectionType.class, id = 34005)
@Persistable(clazz = TypeRepresentation.SumType.class, id = 34006)
@Persistable(clazz = TypeRepresentation.UnresolvedSymbol.class, id = 34007)
@Persistable(clazz = AtomTypeInterfaceFromBindingsMap.class, id = 34010)
public final class TypeInferencePersistance {
  private TypeInferencePersistance() {}

  @ServiceProvider(service = Persistance.class)
  public static final class PersistJavaList extends Persistance<List> {
    public PersistJavaList() {
      super(List.class, true, 34011);
    }

    @Override
    protected void writeObject(List list, Output out) throws IOException {
      var size = list.size();
      out.writeInt(size);
      for (var i = 0; i < size; i++) {
        out.writeObject(list.get(i));
      }
    }

    @Override
    protected List readObject(Input in) throws IOException, ClassNotFoundException {
      var size = in.readInt();
      var list = new ArrayList<>(size);
      for (var i = 0; i < size; i++) {
        var elem = in.readObject();
        list.add(elem);
      }
      return list;
    }
  }
}
