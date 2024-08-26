package org.enso.interpreter.caches;

import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.source.Source;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Optional;
import java.util.logging.Level;
import org.apache.commons.lang3.StringUtils;
import org.enso.common.CompilationStage;
import org.enso.compiler.core.ir.Module;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.persist.Persistance;
import org.enso.version.BuildVersion;

public final class ModuleCache
    implements Cache.Spi<ModuleCache.CachedModule, ModuleCache.Metadata> {
  private final org.enso.interpreter.runtime.Module module;

  private ModuleCache(org.enso.interpreter.runtime.Module module) {
    this.module = module;
  }

  public static Cache<ModuleCache.CachedModule, ModuleCache.Metadata> create(
      org.enso.interpreter.runtime.Module module) {
    var mc = new ModuleCache(module);
    return Cache.create(mc, Level.FINEST, module.getName().toString(), true, false);
  }

  @Override
  public String metadataSuffix() {
    return irCacheMetadataExtension;
  }

  @Override
  public String dataSuffix() {
    return irCacheDataExtension;
  }

  @Override
  public String entryName() {
    return module.getName().item();
  }

  @Override
  public byte[] metadata(String sourceDigest, String blobDigest, CachedModule entry)
      throws IOException {
    return new Metadata(sourceDigest, blobDigest, entry.compilationStage().toString()).toBytes();
  }

  @Override
  public byte[] serialize(EnsoContext context, CachedModule entry) throws IOException {
    var arr =
        Persistance.write(
            entry.moduleIR(), CacheUtils.writeReplace(context.getCompiler().context(), true));
    return arr;
  }

  @Override
  public CachedModule deserialize(
      EnsoContext context, ByteBuffer data, Metadata meta, TruffleLogger logger)
      throws IOException {
    var ref = Persistance.read(data, CacheUtils.readResolve(context.getCompiler().context()));
    var mod = ref.get(Module.class);
    return new CachedModule(
        mod, CompilationStage.valueOf(meta.compilationStage()), module.getSource());
  }

  @Override
  public Optional<Metadata> metadataFromBytes(byte[] bytes, TruffleLogger logger)
      throws IOException {
    return Optional.of(Metadata.read(bytes));
  }

  private Optional<String> computeDigestOfModuleSources(Source source) {
    if (source != null) {
      byte[] sourceBytes;
      if (source.hasBytes()) {
        sourceBytes = source.getBytes().toByteArray();
      } else {
        sourceBytes = source.getCharacters().toString().getBytes(StandardCharsets.UTF_8);
      }
      return Optional.of(CacheUtils.computeDigestFromBytes(ByteBuffer.wrap(sourceBytes)));
    } else {
      return Optional.empty();
    }
  }

  @Override
  public Optional<String> computeDigest(CachedModule entry, TruffleLogger logger) {
    return computeDigestOfModuleSources(entry.source());
  }

  @Override
  public Optional<String> computeDigestFromSource(EnsoContext context, TruffleLogger logger) {
    try {
      return computeDigestOfModuleSources(module.getSource());
    } catch (IOException e) {
      logger.log(Level.FINEST, "failed to retrieve the source of " + module.getName(), e);
      return Optional.empty();
    }
  }

  @Override
  public Optional<Cache.Roots> getCacheRoots(EnsoContext context) {
    if (module != context.getBuiltins().getModule()) {
      return context
          .getPackageOf(module.getSourceFile())
          .map(
              pkg -> {
                var irCacheRoot = pkg.getIrCacheRootForPackage(BuildVersion.ensoVersion());
                var qualName = module.getName();
                var localCacheRoot = irCacheRoot.resolve(qualName.path().mkString("/"));

                var distribution = context.getDistributionManager();
                var pathSegmentsJava = new ArrayList<String>();
                pathSegmentsJava.addAll(
                    Arrays.asList(
                        pkg.namespace(),
                        pkg.normalizedName(),
                        pkg.getConfig().version(),
                        BuildVersion.ensoVersion()));
                pathSegmentsJava.addAll(qualName.pathAsJava());
                var path =
                    distribution.LocallyInstalledDirectories()
                        .irCacheDirectory()
                        .resolve(StringUtils.join(pathSegmentsJava, "/"));
                var globalCacheRoot = context.getTruffleFile(path.toFile());

                return new Cache.Roots(localCacheRoot, globalCacheRoot);
              });
    } else {
      var distribution = context.getDistributionManager();
      var pathSegmentsJava = new ArrayList<String>();
      pathSegmentsJava.addAll(
          Arrays.asList(
              Builtins.NAMESPACE,
              Builtins.PACKAGE_NAME,
              BuildVersion.ensoVersion(),
              BuildVersion.ensoVersion()));
      pathSegmentsJava.addAll(module.getName().pathAsJava());
      var path =
          distribution.LocallyInstalledDirectories()
              .irCacheDirectory()
              .resolve(StringUtils.join(pathSegmentsJava, "/"));
      var globalCacheRoot = context.getTruffleFile(path.toFile());

      return Optional.of(new Cache.Roots(globalCacheRoot, globalCacheRoot));
    }
  }

  @Override
  public String sourceHash(Metadata meta) {
    return meta.sourceHash();
  }

  @Override
  public String blobHash(Metadata meta) {
    return meta.blobHash();
  }

  public record CachedModule(Module moduleIR, CompilationStage compilationStage, Source source) {}

  public record Metadata(String sourceHash, String blobHash, String compilationStage) {
    byte[] toBytes() throws IOException {
      try (var os = new ByteArrayOutputStream();
          var dos = new DataOutputStream(os)) {
        dos.writeUTF(sourceHash());
        dos.writeUTF(blobHash());
        dos.writeUTF(compilationStage());
        return os.toByteArray();
      }
    }

    static Metadata read(byte[] arr) throws IOException {
      try (var is = new ByteArrayInputStream(arr);
          var dis = new DataInputStream(is)) {
        return new Metadata(dis.readUTF(), dis.readUTF(), dis.readUTF());
      }
    }
  }

  private static final String irCacheDataExtension = ".ir";

  private static final String irCacheMetadataExtension = ".meta";
}
