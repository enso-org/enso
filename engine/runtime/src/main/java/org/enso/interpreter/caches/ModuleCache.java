package org.enso.interpreter.caches;

import buildinfo.Info;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.source.Source;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Optional;
import java.util.UUID;
import java.util.logging.Level;
import org.apache.commons.lang3.StringUtils;
import org.enso.compiler.core.ir.Module;
import org.enso.compiler.core.ir.ProcessingPass;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.persist.Persistance;
import org.enso.polyglot.CompilationStage;

public final class ModuleCache extends Cache<ModuleCache.CachedModule, ModuleCache.Metadata> {

  private final org.enso.interpreter.runtime.Module module;

  public ModuleCache(org.enso.interpreter.runtime.Module module) {
    super(Level.FINEST, module.getName().toString(), true, false);
    this.module = module;
    this.entryName = module.getName().item();
    this.dataSuffix = irCacheDataExtension;
    this.metadataSuffix = irCacheMetadataExtension;
  }

  @Override
  protected byte[] metadata(String sourceDigest, String blobDigest, CachedModule entry) {
    try {
      return objectMapper.writeValueAsBytes(
          new Metadata(sourceDigest, blobDigest, entry.compilationStage().toString()));
    } catch (JsonProcessingException e) {
      throw new RuntimeException(e);
    }
  }

  @Override
  protected CachedModule deserialize(
      EnsoContext context, byte[] data, Metadata meta, TruffleLogger logger)
      throws ClassNotFoundException, IOException, ClassNotFoundException {
    var ref =
        Persistance.read(
            data,
            (obj) ->
                switch (obj) {
                  case ProcessingPass.Metadata metadata -> {
                    var option = metadata.restoreFromSerialization(context.getCompiler().context());
                    if (option.nonEmpty()) {
                      yield option.get();
                    } else {
                      throw raise(
                          RuntimeException.class, new IOException("Cannot convert " + metadata));
                    }
                  }
                  case null -> null;
                  default -> obj;
                });
    var mod = ref.get(Module.class);
    return new CachedModule(
        mod, CompilationStage.valueOf(meta.compilationStage()), module.getSource());
  }

  @Override
  protected Optional<Metadata> metadataFromBytes(byte[] bytes, TruffleLogger logger) {
    var maybeJsonString = new String(bytes, Cache.metadataCharset);
    try {
      return Optional.of(objectMapper.readValue(maybeJsonString, Metadata.class));
    } catch (JsonProcessingException e) {
      logger.log(logLevel, "Failed to deserialize module's metadata.", e);
      return Optional.empty();
    }
  }

  private Optional<String> computeDigestOfModuleSources(Source source) {
    if (source != null) {
      byte[] sourceBytes;
      if (source.hasBytes()) {
        sourceBytes = source.getBytes().toByteArray();
      } else {
        sourceBytes = source.getCharacters().toString().getBytes(metadataCharset);
      }
      return Optional.of(computeDigestFromBytes(sourceBytes));
    } else {
      return Optional.empty();
    }
  }

  @Override
  protected Optional<String> computeDigest(CachedModule entry, TruffleLogger logger) {
    return computeDigestOfModuleSources(entry.source());
  }

  @Override
  protected Optional<String> computeDigestFromSource(EnsoContext context, TruffleLogger logger) {
    try {
      return computeDigestOfModuleSources(module.getSource());
    } catch (IOException e) {
      logger.log(logLevel, "failed to retrieve the source of " + module.getName(), e);
      return Optional.empty();
    }
  }

  @Override
  protected Optional<Cache.Roots> getCacheRoots(EnsoContext context) {
    if (module != context.getBuiltins().getModule()) {
      return context
          .getPackageOf(module.getSourceFile())
          .map(
              pkg -> {
                var irCacheRoot = pkg.getIrCacheRootForPackage(Info.ensoVersion());
                var qualName = module.getName();
                var localCacheRoot = irCacheRoot.resolve(qualName.path().mkString("/"));

                var distribution = context.getDistributionManager();
                var pathSegmentsJava = new ArrayList<String>();
                pathSegmentsJava.addAll(
                    Arrays.asList(
                        pkg.namespace(),
                        pkg.normalizedName(),
                        pkg.getConfig().version(),
                        Info.ensoVersion()));
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
              Builtins.NAMESPACE, Builtins.PACKAGE_NAME, Info.ensoVersion(), Info.ensoVersion()));
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
  protected byte[] serialize(EnsoContext context, CachedModule entry) throws IOException {
    var arr =
        Persistance.write(
            entry.moduleIR(),
            (obj) ->
                switch (obj) {
                  case ProcessingPass.Metadata metadata -> metadata.prepareForSerialization(
                      context.getCompiler().context());
                  case UUID uuid -> null;
                  case null -> null;
                  default -> obj;
                });
    return arr;
  }

  public record CachedModule(Module moduleIR, CompilationStage compilationStage, Source source) {}

  public record Metadata(
      @JsonProperty("source_hash") String sourceHash,
      @JsonProperty("blob_hash") String blobHash,
      @JsonProperty("compilation_stage") String compilationStage)
      implements Cache.Metadata {}

  private static final String irCacheDataExtension = ".ir";

  private static final String irCacheMetadataExtension = ".meta";

  private static final ObjectMapper objectMapper = new ObjectMapper();

  @SuppressWarnings("unchecked")
  private static <T extends Exception> T raise(Class<T> cls, Exception e) throws T {
    throw (T) e;
  }
}
