package org.enso.interpreter.caches;

import buildinfo.Info;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLogger;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.List;
import java.util.Optional;
import java.util.logging.Level;
import org.apache.commons.lang3.StringUtils;
import org.enso.compiler.data.BindingsMap;
import org.enso.compiler.data.BindingsMap.DefinedEntity;
import org.enso.compiler.data.BindingsMap.ModuleReference;
import org.enso.editions.LibraryName;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.persist.Persistable;
import org.enso.persist.Persistance;
import org.enso.pkg.QualifiedName;
import org.enso.pkg.SourceFile;
import org.openide.util.lookup.ServiceProvider;
import scala.Option;
import scala.Tuple2;
import scala.collection.immutable.Map;

@Persistable(clazz = QualifiedName.class, id = 30300)
public final class ImportExportCache
    extends Cache<ImportExportCache.CachedBindings, ImportExportCache.Metadata> {

  private final LibraryName libraryName;

  public ImportExportCache(LibraryName libraryName) {
    super(Level.FINEST, libraryName.toString(), true, false);
    this.libraryName = libraryName;
    this.entryName = libraryName.name();
    this.dataSuffix = bindingsCacheDataExtension;
    this.metadataSuffix = bindingsCacheMetadataExtension;
  }

  @Override
  protected byte[] metadata(String sourceDigest, String blobDigest, CachedBindings entry)
      throws IOException {
    return new Metadata(sourceDigest, blobDigest).toBytes();
  }

  @Override
  protected CachedBindings deserialize(
      EnsoContext context, byte[] data, Metadata meta, TruffleLogger logger)
      throws ClassNotFoundException, IOException, ClassNotFoundException {
    var ref = Persistance.read(data, null);
    var bindings = ref.get(MapToBindings.class);
    return new CachedBindings(libraryName, bindings, Optional.empty());
  }

  @Override
  protected Optional<Metadata> metadataFromBytes(byte[] bytes, TruffleLogger logger)
      throws IOException {
    return Optional.of(Metadata.read(bytes));
  }

  @Override
  protected Optional<String> computeDigest(CachedBindings entry, TruffleLogger logger) {
    return entry.sources().map(sources -> computeDigestOfLibrarySources(sources, logger));
  }

  @Override
  @SuppressWarnings("unchecked")
  protected Optional<String> computeDigestFromSource(EnsoContext context, TruffleLogger logger) {
    return context
        .getPackageRepository()
        .getPackageForLibraryJava(libraryName)
        .map(pkg -> computeDigestOfLibrarySources(pkg.listSourcesJava(), logger));
  }

  @Override
  @SuppressWarnings("unchecked")
  protected Optional<Cache.Roots> getCacheRoots(EnsoContext context) {
    return context
        .getPackageRepository()
        .getPackageForLibraryJava(libraryName)
        .map(
            pkg -> {
              var bindingsCacheRoot = pkg.getBindingsCacheRootForPackage(Info.ensoVersion());
              var localCacheRoot = bindingsCacheRoot.resolve(libraryName.namespace());
              var distribution = context.getDistributionManager();
              var pathSegments =
                  new String[] {
                    pkg.namespace(),
                    pkg.normalizedName(),
                    pkg.getConfig().version(),
                    Info.ensoVersion(),
                    libraryName.namespace()
                  };
              var path =
                  distribution.LocallyInstalledDirectories()
                      .irCacheDirectory()
                      .resolve(StringUtils.join(pathSegments, "/"));
              var globalCacheRoot = context.getTruffleFile(path.toFile());
              return new Cache.Roots(localCacheRoot, globalCacheRoot);
            });
  }

  @Override
  protected byte[] serialize(EnsoContext context, CachedBindings entry) throws IOException {
    var arr = Persistance.write(entry.bindings(), null);
    return arr;
  }

  public static final class MapToBindings {
    private final Map<QualifiedName, Persistance.Reference<BindingsMap>> entries;

    public MapToBindings(Map<QualifiedName, Persistance.Reference<BindingsMap>> entries) {
      this.entries = entries;
    }

    public Option<BindingsMap> findForModule(QualifiedName moduleName) {
      var ref = entries.get(moduleName);
      if (ref.isEmpty()) {
        return Option.empty();
      }
      return Option.apply(ref.get().get(BindingsMap.class));
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistMapToBindings extends Persistance<MapToBindings> {
    public PersistMapToBindings() {
      super(MapToBindings.class, false, 364);
    }

    @Override
    protected void writeObject(MapToBindings obj, Output out) throws IOException {
      out.writeInt(obj.entries.size());
      var it = obj.entries.iterator();
      while (it.hasNext()) {
        var e = it.next();
        out.writeInline(QualifiedName.class, e._1());
        out.writeObject(e._2().get(BindingsMap.class));
      }
    }

    @Override
    @SuppressWarnings("unchecked")
    protected MapToBindings readObject(Input in) throws IOException, ClassNotFoundException {
      var size = in.readInt();
      var b = Map.newBuilder();
      b.sizeHint(size);
      while (size-- > 0) {
        var name = in.readInline(QualifiedName.class);
        var value = in.readReference(BindingsMap.class);
        b.addOne(Tuple2.apply(name, value));
      }
      return new MapToBindings((Map) b.result());
    }
  }

  public static record CachedBindings(
      LibraryName libraryName,
      MapToBindings bindings,
      Optional<List<SourceFile<TruffleFile>>> sources) {}

  public record Metadata(String sourceHash, String blobHash) implements Cache.Metadata {
    byte[] toBytes() throws IOException {
      try (var os = new ByteArrayOutputStream();
          var dos = new DataOutputStream(os)) {
        dos.writeUTF(sourceHash());
        dos.writeUTF(blobHash());
        return os.toByteArray();
      }
    }

    static Metadata read(byte[] arr) throws IOException {
      try (var is = new ByteArrayInputStream(arr);
          var dis = new DataInputStream(is)) {
        return new Metadata(dis.readUTF(), dis.readUTF());
      }
    }
  }

  private static final String bindingsCacheDataExtension = ".bindings";

  private static final String bindingsCacheMetadataExtension = ".bindings.meta";

  @Persistable(clazz = BindingsMap.PolyglotSymbol.class, id = 33006)
  @Persistable(
      clazz = org.enso.compiler.data.BindingsMap$ModuleReference$Abstract.class,
      id = 33007)
  @Persistable(clazz = BindingsMap.ModuleMethod.class, id = 33008)
  @Persistable(clazz = BindingsMap.Type.class, id = 33009)
  @Persistable(clazz = BindingsMap.ResolvedImport.class, id = 33010)
  @Persistable(clazz = BindingsMap.Cons.class, id = 33011)
  @Persistable(clazz = BindingsMap.ResolvedModule.class, id = 33012)
  @Persistable(clazz = BindingsMap.ResolvedType.class, id = 33013)
  @Persistable(clazz = BindingsMap.ResolvedMethod.class, id = 33014)
  @Persistable(clazz = BindingsMap.ExportedModule.class, id = 33015)
  @Persistable(clazz = org.enso.compiler.data.BindingsMap$SymbolRestriction$Only.class, id = 33016)
  @Persistable(clazz = org.enso.compiler.data.BindingsMap$SymbolRestriction$Union.class, id = 33017)
  @Persistable(
      clazz = org.enso.compiler.data.BindingsMap$SymbolRestriction$Intersect.class,
      id = 33018)
  @Persistable(
      clazz = org.enso.compiler.data.BindingsMap$SymbolRestriction$AllowedResolution.class,
      id = 33019)
  @Persistable(clazz = org.enso.compiler.data.BindingsMap$SymbolRestriction$All$.class, id = 33020)
  @Persistable(
      clazz = org.enso.compiler.data.BindingsMap$SymbolRestriction$Hiding.class,
      id = 33021)
  @Persistable(clazz = BindingsMap.Resolution.class, id = 33029)
  @Persistable(clazz = BindingsMap.ResolvedConstructor.class, id = 33030)
  @Persistable(clazz = BindingsMap.ResolvedPolyglotSymbol.class, id = 33031)
  @Persistable(clazz = BindingsMap.ResolvedPolyglotField.class, id = 33032)
  @ServiceProvider(service = Persistance.class)
  public static final class PersistBindingsMap extends Persistance<BindingsMap> {
    public PersistBindingsMap() {
      super(BindingsMap.class, false, 33005);
    }

    @Override
    protected void writeObject(BindingsMap obj, Output out) throws IOException {
      out.writeObject(obj.definedEntities());
      out.writeObject(obj.currentModule());
      out.writeInline(scala.collection.immutable.List.class, obj.resolvedImports());
      out.writeInline(scala.collection.immutable.List.class, obj.resolvedExports());
      out.writeInline(scala.collection.immutable.Map.class, obj.exportedSymbols());
    }

    @Override
    @SuppressWarnings("unchecked")
    protected BindingsMap readObject(Input in) throws IOException, ClassNotFoundException {
      var de = (scala.collection.immutable.List<DefinedEntity>) in.readObject();
      var cm = (ModuleReference) in.readObject();
      var imp = in.readInline(scala.collection.immutable.List.class);
      var exp = in.readInline(scala.collection.immutable.List.class);
      var sym = in.readInline(scala.collection.immutable.Map.class);
      var map = new BindingsMap(de, cm);
      map.resolvedImports_$eq(imp);
      map.resolvedExports_$eq(exp);
      map.exportedSymbols_$eq(sym);
      return map;
    }
  }
}
