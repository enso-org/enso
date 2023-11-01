package org.enso.compiler.core.ir;

import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;

import org.enso.compiler.core.Persistance;
import org.openide.util.lookup.ServiceProvider;

public record Location(int start, int end) {
  public int length() {
    return end - start;
  }

  @ServiceProvider(service = Persistance.class)
  public static final class Persist extends Persistance<Location> {
    public Persist() {
      super(Location.class, 1);
    }

    @Override
    protected void writeObject(Location obj, ObjectOutput out) throws IOException {
      out.writeInt(obj.start());
      out.writeInt(obj.end());
    }

    @Override
    protected Location readObject(ObjectInput in) throws IOException, ClassNotFoundException {
      return new Location(in.readInt(), in.readInt());
    }
  }
}
