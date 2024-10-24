package org.enso.compiler.pass.analyse;

import java.io.IOException;
import org.enso.common.CachePreferences;
import org.enso.compiler.pass.analyse.alias.AliasMetadata;
import org.enso.compiler.pass.analyse.alias.graph.Graph;
import org.enso.compiler.pass.analyse.alias.graph.GraphOccurrence;
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
import org.enso.compiler.pass.resolve.MethodDefinitions$;
import org.enso.compiler.pass.resolve.ModuleAnnotations;
import org.enso.compiler.pass.resolve.ModuleAnnotations$;
import org.enso.compiler.pass.resolve.Patterns$;
import org.enso.compiler.pass.resolve.TypeNames$;
import org.enso.compiler.pass.resolve.TypeSignatures;
import org.enso.compiler.pass.resolve.TypeSignatures$;
import org.enso.persist.Persistable;
import org.enso.persist.Persistance;
import org.openide.util.lookup.ServiceProvider;
import scala.Option;
import scala.Tuple2$;

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
@Persistable(clazz = MethodDefinitions$.class, id = 1215)
@Persistable(clazz = GenericAnnotations$.class, id = 1216)
@Persistable(clazz = ExpressionAnnotations$.class, id = 1217)
@Persistable(clazz = FullyQualifiedNames$.class, id = 1218)
@Persistable(clazz = AliasMetadata.Occurrence.class, id = 1261, allowInlining = false)
@Persistable(clazz = AliasMetadata.RootScope.class, id = 1262, allowInlining = false)
@Persistable(clazz = AliasMetadata.ChildScope.class, id = 1263, allowInlining = false)
@Persistable(clazz = GraphOccurrence.Use.class, id = 1264, allowInlining = false)
@Persistable(clazz = GraphOccurrence.Def.class, id = 1265, allowInlining = false)
@Persistable(clazz = Graph.Link.class, id = 1266, allowInlining = false)
@Persistable(clazz = TypeInferencePropagation.class, id = 1280)
@Persistable(clazz = TypeInferenceSignatures.class, id = 1281)
@Persistable(clazz = FramePointerAnalysis$.class, id = 1282)
@Persistable(clazz = TailCall.TailPosition.class, id = 1284)
@Persistable(clazz = CachePreferences.class, id = 1285)
@Persistable(clazz = StaticModuleScopeAnalysis.class, id = 1287)
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

  @org.openide.util.lookup.ServiceProvider(service = Persistance.class)
  public static final class PersistAliasAnalysisGraphScope extends Persistance<Graph.Scope> {
    public PersistAliasAnalysisGraphScope() {
      super(Graph.Scope.class, false, 1267);
    }

    @Override
    @SuppressWarnings("unchecked")
    protected Graph.Scope readObject(Input in) throws IOException {
      var childScopes = in.readInline(scala.collection.immutable.List.class);
      var occurrencesValues = (scala.collection.immutable.Set<GraphOccurrence>) in.readObject();
      var occurrences = occurrencesValues.map(v -> Tuple2$.MODULE$.apply(v.id(), v)).toMap(null);
      var allDefinitions = in.readInline(scala.collection.immutable.List.class);
      var parent = new Graph.Scope(childScopes, occurrences, allDefinitions);
      var optionParent = Option.apply(parent);
      childScopes.forall(
          (object) -> {
            var ch = (Graph.Scope) object;
            ch.parent_$eq(optionParent);
            return null;
          });
      return parent;
    }

    @Override
    @SuppressWarnings("unchecked")
    protected void writeObject(Graph.Scope obj, Output out) throws IOException {
      out.writeInline(scala.collection.immutable.List.class, obj.childScopes());
      out.writeObject(obj.occurrences().values().toSet());
      out.writeInline(scala.collection.immutable.List.class, obj.allDefinitions());
    }
  }

  @org.openide.util.lookup.ServiceProvider(service = Persistance.class)
  public static final class PersistAliasAnalysisGraph extends Persistance<Graph> {
    public PersistAliasAnalysisGraph() {
      super(Graph.class, false, 1268);
    }

    @SuppressWarnings("unchecked")
    protected Graph readObject(Input in) throws IOException {

      var rootScope = (Graph.Scope) in.readObject();
      assignParents(rootScope);

      var links =
          (scala.collection.immutable.Set) in.readInline(scala.collection.immutable.Set.class);

      var nextIdCounter = in.readInt();
      var g = new Graph(rootScope, nextIdCounter, links);
      g.freeze();
      return g;
    }

    @SuppressWarnings("unchecked")
    @Override
    protected void writeObject(Graph obj, Output out) throws IOException {
      out.writeObject(obj.rootScope());
      out.writeInline(scala.collection.immutable.Set.class, obj.getLinks());
      out.writeInt(obj.nextIdCounter());
    }

    private static void assignParents(Graph.Scope scope) {
      var option = Option.apply(scope);
      scope
          .childScopes()
          .foreach(
              (ch) -> {
                assignParents(ch);
                ch.parent_$eq(option);
                return null;
              });
    }
  }
}
