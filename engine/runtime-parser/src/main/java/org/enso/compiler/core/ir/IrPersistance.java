package org.enso.compiler.core.ir;

import java.io.IOException;
import org.enso.compiler.core.Persistance;
import org.openide.util.lookup.ServiceProvider;
import scala.Option;

public final class IrPersistance {
  private IrPersistance() {}

  @ServiceProvider(service = Persistance.class)
  public static final class PersistIdentifiedLocation extends Persistance<IdentifiedLocation> {
    public PersistIdentifiedLocation() {
      super(IdentifiedLocation.class, 2);
    }

    @Override
    protected void writeObject(IdentifiedLocation obj, Output out) throws IOException {
      out.writeObject(obj.location());
    }

    @Override
    protected IdentifiedLocation readObject(Input in) throws IOException, ClassNotFoundException {
      var obj = in.readObject();
      return new IdentifiedLocation((Location) obj, Option.empty());
    }
  }
}
