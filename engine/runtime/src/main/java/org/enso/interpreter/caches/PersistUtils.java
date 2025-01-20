package org.enso.interpreter.caches;

import java.io.IOException;
import java.util.ArrayList;
import org.enso.persist.Persistance;
import org.openide.util.lookup.ServiceProvider;

public class PersistUtils {
  @ServiceProvider(service = Persistance.class)
  public static final class PersistArrayList extends Persistance<ArrayList> {
    public PersistArrayList() {
      super(ArrayList.class, true, 30360);
    }

    @Override
    protected void writeObject(ArrayList obj, Output out) throws IOException {
      out.writeInt(obj.size());
      for (Object o : obj) {
        out.writeObject(o);
      }
    }

    @Override
    protected ArrayList readObject(Input in) throws IOException, ClassNotFoundException {
      int size = in.readInt();
      var lst = new ArrayList<>(size);
      for (int i = 0; i < size; i++) {
        var obj = in.readObject();
        lst.add(obj);
      }
      return lst;
    }
  }
}
