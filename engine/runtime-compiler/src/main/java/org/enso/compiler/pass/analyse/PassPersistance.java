package org.enso.compiler.pass.analyse;

import java.io.IOException;
import org.enso.compiler.core.Persistance;
import org.enso.compiler.pass.resolve.IgnoredBindings;
import org.enso.interpreter.dsl.Persistable;
import org.openide.util.lookup.ServiceProvider;

@Persistable(clazz = CachePreferenceAnalysis.WeightInfo.class, id = 1111)
@Persistable(clazz = DataflowAnalysis.DependencyInfo.class, id = 1112)
@Persistable(clazz = DataflowAnalysis.DependencyMapping.class, id = 1113)
public final class PassPersistance {
  private PassPersistance() {}

  @ServiceProvider(service = Persistance.class)
  public static final class PersistState extends Persistance<IgnoredBindings.State> {
    public PersistState() {
      super(IgnoredBindings.State.class, true, 1101);
    }

    @Override
    protected void writeObject(IgnoredBindings.State obj, Output out) throws IOException {
      out.writeBoolean(obj.isIgnored());
    }

    @Override
    protected IgnoredBindings.State readObject(Input in)
        throws IOException, ClassNotFoundException {
      var b = in.readBoolean();
      return b
          ? org.enso.compiler.pass.resolve.IgnoredBindings$State$Ignored$.MODULE$
          : org.enso.compiler.pass.resolve.IgnoredBindings$State$NotIgnored$.MODULE$;
    }
  }
}
