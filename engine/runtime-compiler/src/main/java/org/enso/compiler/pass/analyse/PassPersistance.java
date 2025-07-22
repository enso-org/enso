package org.enso.compiler.pass.analyse;

import java.io.IOException;
import java.util.HashMap;
import java.util.UUID;
import org.enso.common.CachePreferences;
import org.enso.compiler.pass.analyse.alias.AliasMetadata;
import org.enso.compiler.pass.analyse.alias.graph.Graph;
import org.enso.compiler.pass.analyse.types.TypeInferencePropagation;
import org.enso.compiler.pass.analyse.types.TypeInferenceSignatures;
import org.enso.compiler.pass.analyse.types.scope.StaticModuleScopeAnalysis;
import org.enso.compiler.pass.resolve.DocumentationComments;
import org.enso.compiler.pass.resolve.DocumentationComments$;
import org.enso.compiler.pass.resolve.ExpressionAnnotations$;
import org.enso.compiler.pass.resolve.FullyQualifiedNames;
import org.enso.compiler.pass.resolve.FullyQualifiedNames$;
import org.enso.compiler.pass.resolve.GenericAnnotations$;
import org.enso.compiler.pass.resolve.GlobalNames$;
import org.enso.compiler.pass.resolve.IgnoredBindings;
import org.enso.compiler.pass.resolve.IgnoredBindings$;
import org.enso.compiler.pass.resolve.MethodCalls$;
import org.enso.compiler.pass.resolve.MethodDefinitions;
import org.enso.compiler.pass.resolve.ModuleAnnotations;
import org.enso.compiler.pass.resolve.ModuleAnnotations$;
import org.enso.compiler.pass.resolve.Patterns$;
import org.enso.compiler.pass.resolve.TypeNames$;
import org.enso.compiler.pass.resolve.TypeSignatures;
import org.enso.compiler.pass.resolve.TypeSignatures$;
import org.enso.persist.Persistable;
import org.enso.persist.Persistance;

@Persistable(clazz = CachePreferenceAnalysis.WeightInfo.class, id = 1111)
@Persistable(clazz = DataflowAnalysis.DependencyInfo.class, id = 1112)
@Persistable(clazz = DataflowAnalysis.DependencyMapping.class, id = 1113)
@Persistable(clazz = GatherDiagnostics.DiagnosticsMeta.class, id = 1114)
@Persistable(clazz = DocumentationComments.Doc.class, id = 1115)
@Persistable(clazz = TypeSignatures.Signature.class, id = 2117)
@Persistable(clazz = ModuleAnnotations.Annotations.class, id = 1118)
@Persistable(clazz = DataflowAnalysis$DependencyInfo$Type$Static.class, id = 1121)
@Persistable(clazz = DataflowAnalysis$DependencyInfo$Type$Dynamic.class, id = 1122)
@Persistable(clazz = FullyQualifiedNames.FQNResolution.class, id = 1128)
@Persistable(clazz = FullyQualifiedNames.ResolvedLibrary.class, id = 1129)
@Persistable(clazz = FullyQualifiedNames.ResolvedModule.class, id = 1130)
@Persistable(clazz = AliasAnalysis$.class, id = 1201)
@Persistable(clazz = BindingAnalysis$.class, id = 1202)
@Persistable(clazz = CachePreferenceAnalysis$.class, id = 1203)
@Persistable(clazz = DataflowAnalysis$.class, id = 1204)
@Persistable(clazz = GlobalNames$.class, id = 1205)
@Persistable(clazz = IgnoredBindings$.class, id = 1206)
@Persistable(clazz = Patterns$.class, id = 1207)
@Persistable(clazz = TailCall.class, id = 1208)
@Persistable(clazz = TypeNames$.class, id = 1209)
@Persistable(clazz = TypeSignatures$.class, id = 1210)
@Persistable(clazz = DocumentationComments$.class, id = 1211)
@Persistable(clazz = ModuleAnnotations$.class, id = 1212)
@Persistable(clazz = GatherDiagnostics$.class, id = 1213)
@Persistable(clazz = MethodCalls$.class, id = 1214)
@Persistable(clazz = MethodDefinitions.class, id = 1215)
@Persistable(clazz = GenericAnnotations$.class, id = 1216)
@Persistable(clazz = ExpressionAnnotations$.class, id = 1217)
@Persistable(clazz = FullyQualifiedNames$.class, id = 1218)
@Persistable(clazz = AliasMetadata.Occurrence.class, id = 1261, allowInlining = false)
@Persistable(clazz = AliasMetadata.RootScope.class, id = 1262, allowInlining = false)
@Persistable(clazz = AliasMetadata.ChildScope.class, id = 1263, allowInlining = false)
@Persistable(clazz = Graph.Link.class, id = 1266, allowInlining = false)
@Persistable(clazz = TypeInferencePropagation.class, id = 1280)
@Persistable(clazz = TypeInferenceSignatures.class, id = 1281)
@Persistable(clazz = FramePointerAnalysis$.class, id = 1282)
@Persistable(clazz = TailCall.TailPosition.class, id = 1284)
@Persistable(clazz = StaticModuleScopeAnalysis.class, id = 1287)
public final class PassPersistance {
  private PassPersistance() {}

  @Persistable(id = 1101)
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

  @Persistable(id = 1289)
  public static final class PersistCachePreferences
      extends Persistance<org.enso.common.CachePreferences> {
    public PersistCachePreferences() {
      super(org.enso.common.CachePreferences.class, false, 1289);
    }

    @SuppressWarnings("unchecked")
    @Override
    protected CachePreferences readObject(Input in) throws IOException {
      var map = new HashMap<UUID, org.enso.common.CachePreferences.Kind>();
      var cnt = in.readInt();
      while (cnt-- > 0) {
        var id = in.readInline(UUID.class);
        var kind =
            switch (in.readByte()) {
              case 1 -> CachePreferences.Kind.SELF_ARGUMENT;
              case 2 -> CachePreferences.Kind.BINDING_EXPRESSION;
              default -> throw new IOException();
            };
        map.put(id, kind);
      }
      return new CachePreferences(map);
    }

    @SuppressWarnings("unchecked")
    @Override
    protected void writeObject(org.enso.common.CachePreferences obj, Output out)
        throws IOException {
      out.writeInt(obj.preferences().size());
      for (var entry : obj.preferences().entrySet()) {
        out.writeInline(UUID.class, entry.getKey());
        out.writeByte(
            switch (entry.getValue()) {
              case SELF_ARGUMENT -> 1;
              case BINDING_EXPRESSION -> 2;
              default -> throw new IOException();
            });
      }
    }
  }
}
