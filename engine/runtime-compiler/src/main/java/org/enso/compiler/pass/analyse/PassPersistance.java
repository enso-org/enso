package org.enso.compiler.pass.analyse;

import java.io.IOException;
import org.enso.compiler.core.Persistance;
import org.enso.compiler.pass.resolve.DocumentationComments;
import org.enso.compiler.pass.resolve.IgnoredBindings;
import org.enso.compiler.pass.resolve.ModuleAnnotations;
import org.enso.compiler.pass.resolve.TypeSignatures;
import org.enso.interpreter.dsl.Persistable;
import org.openide.util.lookup.ServiceProvider;

@Persistable(clazz = CachePreferenceAnalysis.WeightInfo.class, id = 1111)
@Persistable(clazz = DataflowAnalysis.DependencyInfo.class, id = 1112)
@Persistable(clazz = DataflowAnalysis.DependencyMapping.class, id = 1113)
@Persistable(clazz = GatherDiagnostics.DiagnosticsMeta.class, id = 1114)
@Persistable(clazz = DocumentationComments.Doc.class, id = 1115)
@Persistable(clazz = AliasAnalysis$Info$Occurrence.class, id = 1116)
@Persistable(clazz = TypeSignatures.Signature.class, id = 1117)
@Persistable(clazz = ModuleAnnotations.Annotations.class, id = 1118)
@Persistable(clazz = AliasAnalysis.Graph.class, id = 1119)
@Persistable(clazz = AliasAnalysis$Info$Scope$Root.class, id = 1120)
@Persistable(clazz = DataflowAnalysis$DependencyInfo$Type$Static.class, id = 1121)
@Persistable(clazz = DataflowAnalysis$DependencyInfo$Type$Dynamic.class, id = 1122)
@Persistable(clazz = AliasAnalysis$Info$Scope$Child.class, id = 1123)
@Persistable(clazz = AliasAnalysis$Graph$Scope.class, id = 1124)
@Persistable(clazz = AliasAnalysis$Graph$Occurrence$Use.class, id = 1125)
@Persistable(clazz = AliasAnalysis$Graph$Occurrence$Def.class, id = 1126)
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

  @ServiceProvider(service = Persistance.class)
  public static final class PersistTail extends Persistance<TailCall.TailPosition> {
    public PersistTail() {
      super(TailCall.TailPosition.class, true, 1102);
    }

    @Override
    protected void writeObject(TailCall.TailPosition obj, Output out) throws IOException {
      out.writeBoolean(obj.isTail());
    }

    @Override
    protected TailCall.TailPosition readObject(Input in)
        throws IOException, ClassNotFoundException {
      var b = in.readBoolean();
      return b
          ? org.enso.compiler.pass.analyse.TailCall$TailPosition$Tail$.MODULE$
          : org.enso.compiler.pass.analyse.TailCall$TailPosition$NotTail$.MODULE$;
    }
  }
}
