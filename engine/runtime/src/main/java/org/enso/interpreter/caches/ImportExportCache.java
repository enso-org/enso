package org.enso.interpreter.caches;

import buildinfo.Info;
import com.oracle.truffle.api.TruffleFile;
import com.oracle.truffle.api.TruffleLogger;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
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

public final class ImportExportCache
    implements Cache.Spi<ImportExportCache.CachedBindings, ImportExportCache.Metadata> {

  private final LibraryName libraryName;

  private ImportExportCache(LibraryName libraryName) {
    this.libraryName = libraryName;
  }

  public static Cache<ImportExportCache.CachedBindings, ImportExportCache.Metadata> create(
      LibraryName libraryName) {
    var impl = new ImportExportCache(libraryName);
    return Cache.create(impl, Level.FINEST, libraryName.toString(), true, false);
  }

  @Override
  public String metadataSuffix() {
    return bindingsCacheMetadataExtension;
  }

  @Override
  public String dataSuffix() {
    return bindingsCacheDataExtension;
  }

  @Override
  public String entryName() {
    return libraryName.name();
  }

  @Override
  public byte[] metadata(String sourceDigest, String blobDigest, CachedBindings entry)
      throws IOException {
    return new Metadata(sourceDigest, blobDigest).toBytes();
  }

  @Override
  public byte[] serialize(EnsoContext context, CachedBindings entry) throws IOException {
    var arr =
        Persistance.write(
            entry.bindings(), CacheUtils.writeReplace(context.getCompiler().context(), false));
    return arr;
  }

  @Override
  public CachedBindings deserialize(
      EnsoContext context, ByteBuffer data, Metadata meta, TruffleLogger logger)
      throws IOException {
    var ref = Persistance.read(data, CacheUtils.readResolve(context.getCompiler().context()));
    var bindings = ref.get(MapToBindings.class);
    return new CachedBindings(libraryName, bindings, Optional.empty());
  }

  @Override
  public Optional<Metadata> metadataFromBytes(byte[] bytes, TruffleLogger logger)
      throws IOException {
    return Optional.of(Metadata.read(bytes));
  }

  @Override
  public Optional<String> computeDigest(CachedBindings entry, TruffleLogger logger) {
    return entry.sources().map(sources -> CacheUtils.computeDigestOfLibrarySources(sources));
  }

  @Override
  @SuppressWarnings("unchecked")
  public Optional<String> computeDigestFromSource(EnsoContext context, TruffleLogger logger) {
    return context
        .getPackageRepository()
        .getPackageForLibraryJava(libraryName)
        .map(pkg -> CacheUtils.computeDigestOfLibrarySources(pkg.listSourcesJava()));
  }

  @Override
  @SuppressWarnings("unchecked")
  public Optional<Cache.Roots> getCacheRoots(EnsoContext context) {
    return context
        .getPackageRepository()
        .getPackageForLibraryJava(libraryName)
        .map(
            pkg -> {
              TruffleFile bindingsCacheRoot =
                  pkg.getBindingsCacheRootForPackage(Info.ensoVersion());
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
  public String sourceHash(Metadata meta) {
    return meta.sourceHash();
  }

  @Override
  public String blobHash(Metadata meta) {
    return meta.blobHash();
  }

  public static final class MapToBindings {
    private final java.util.Map<QualifiedName, org.enso.compiler.core.ir.Module> entries;

    public MapToBindings(java.util.Map<QualifiedName, org.enso.compiler.core.ir.Module> entries) {
      this.entries = entries;
    }

    public org.enso.compiler.core.ir.Module findForModule(QualifiedName moduleName) {
      return entries.get(moduleName);
    }
  }

  @ServiceProvider(service = Persistance.class)
  public static final class PersistMapToBindings extends Persistance<MapToBindings> {
    public PersistMapToBindings() {
      super(MapToBindings.class, false, 3642);
    }

    @Override
    protected void writeObject(MapToBindings obj, Output out) throws IOException {
      out.writeInline(java.util.Map.class, obj.entries);
    }

    @Override
    @SuppressWarnings("unchecked")
    protected MapToBindings readObject(Input in) throws IOException, ClassNotFoundException {
      var map = in.readInline(java.util.Map.class);
      return new MapToBindings(map);
    }
  }

  public static record CachedBindings(
      LibraryName libraryName,
      MapToBindings bindings,
      Optional<List<SourceFile<TruffleFile>>> sources) {}

  public record Metadata(String sourceHash, String blobHash) {
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
  @Persistable(clazz = BindingsMap.Type.class, id = 33009)
  @Persistable(clazz = BindingsMap.ResolvedImport.class, id = 33010)
  @Persistable(clazz = BindingsMap.Cons.class, id = 33011)
  @Persistable(clazz = BindingsMap.ResolvedModule.class, id = 33012)
  @Persistable(clazz = BindingsMap.ResolvedType.class, id = 33013)
  @Persistable(clazz = BindingsMap.ResolvedModuleMethod.class, id = 33014)
  @Persistable(clazz = BindingsMap.ResolvedExtensionMethod.class, id = 33015)
  @Persistable(clazz = BindingsMap.ResolvedConversionMethod.class, id = 33016)
  @Persistable(clazz = BindingsMap.ExportedModule.class, id = 33017)
  @Persistable(clazz = BindingsMap.Resolution.class, id = 33018)
  @Persistable(clazz = BindingsMap.ResolvedConstructor.class, id = 33019)
  @Persistable(clazz = BindingsMap.ResolvedPolyglotSymbol.class, id = 33020)
  @Persistable(clazz = BindingsMap.ResolvedPolyglotField.class, id = 33021)
  @Persistable(clazz = BindingsMap.ModuleMethod.class, id = 33022)
  @Persistable(clazz = BindingsMap.ExtensionMethod.class, id = 33023)
  @Persistable(clazz = BindingsMap.ConversionMethod.class, id = 33024)
  @Persistable(clazz = BindingsMap.Argument.class, id = 33025)
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
      out.writeInline(scala.collection.immutable.Map.class, obj.exportedSymbols());
    }

    @Override
    @SuppressWarnings("unchecked")
    protected BindingsMap readObject(Input in) throws IOException, ClassNotFoundException {
      var de = (scala.collection.immutable.List<DefinedEntity>) in.readObject();
      var cm = (ModuleReference) in.readObject();
      var imp = in.readInline(scala.collection.immutable.List.class);
      var sym = in.readInline(scala.collection.immutable.Map.class);
      var map = new BindingsMap(de, cm);
      map.resolvedImports_(imp);
      map.exportedSymbols_(sym);
      return map;
    }
  }
}
