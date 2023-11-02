package org.enso.interpreter.caches;

import buildinfo.Info;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.oracle.truffle.api.TruffleLogger;
import com.oracle.truffle.api.source.Source;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

import org.apache.commons.lang3.StringUtils;
import org.enso.compiler.core.ir.Module;
import org.enso.interpreter.runtime.EnsoContext;
import org.enso.interpreter.runtime.builtin.Builtins;
import org.enso.polyglot.CompilationStage;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Optional;
import java.util.UUID;
import java.util.logging.Level;

import org.enso.compiler.core.Persistance;

import akka.http.impl.engine.http2.client.PersistentConnection;

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
            return objectMapper.writeValueAsBytes(new Metadata(sourceDigest, blobDigest, entry.compilationStage().toString()));
        } catch (JsonProcessingException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    protected CachedModule deserialize(EnsoContext context, byte[] data, Metadata meta, TruffleLogger logger) throws ClassNotFoundException, IOException, ClassNotFoundException {
      var buf = ByteBuffer.wrap(data);
      var at = buf.getInt(0);
      var ref = Persistance.Reference.from(buf, at);
      var mod = ref.get(Module.class);
      return new CachedModule(mod, CompilationStage.valueOf(meta.compilationStage()), module.getSource());
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
            return context.getPackageOf(module.getSourceFile()).map(pkg -> {
                var irCacheRoot    = pkg.getIrCacheRootForPackage(Info.ensoVersion());
                var qualName       = module.getName();
                var localCacheRoot = irCacheRoot.resolve(qualName.path().mkString("/"));

                var distribution = context.getDistributionManager();
                var pathSegmentsJava = new ArrayList<String>();
                pathSegmentsJava.addAll(Arrays.asList(
                        pkg.namespace(),
                        pkg.normalizedName(),
                        pkg.getConfig().version(),
                        Info.ensoVersion()
                ));
                pathSegmentsJava.addAll(qualName.pathAsJava());
                var path = distribution.LocallyInstalledDirectories().irCacheDirectory()
                        .resolve(StringUtils.join(pathSegmentsJava, "/"));
                var globalCacheRoot = context.getTruffleFile(path.toFile());

                return new Cache.Roots(localCacheRoot, globalCacheRoot);
            });
        } else {
            var distribution = context.getDistributionManager();
            var pathSegmentsJava = new ArrayList<String>();
            pathSegmentsJava.addAll(Arrays.asList(
                    Builtins.NAMESPACE,
                    Builtins.PACKAGE_NAME,
                    Info.ensoVersion(),
                    Info.ensoVersion()
            ));
            pathSegmentsJava.addAll(module.getName().pathAsJava());
            var path = distribution.LocallyInstalledDirectories().irCacheDirectory()
                    .resolve(StringUtils.join(pathSegmentsJava, "/"));
            var globalCacheRoot = context.getTruffleFile(path.toFile());

            return Optional.of(new Cache.Roots(globalCacheRoot, globalCacheRoot));
        }
    }

    @Override
    protected byte[] serialize(EnsoContext context, CachedModule entry) throws IOException {
      var byteStream = new ByteArrayOutputStream();
      boolean noUUIDs = false;
      for (var p : context.getPackageRepository().getLoadedPackagesJava()) {
        if ("Standard".equals(p.namespace())) {
          for (var s : p.listSourcesJava()) {
            if (s.file().getPath().equals(entry.source().getPath())) {
              noUUIDs = true;
              break;
            }
          }
        }
      }
      var prelude = new byte[4];
      byteStream.write(prelude);
      var gen = Persistance.newGenerator(byteStream);
      var at = gen.writeObject(entry.moduleIR()) + prelude.length;
      var arr = byteStream.toByteArray();
      arr[0] = (byte) ((at >> 24) & 0xff);
      arr[1] = (byte) ((at >> 16) & 0xff);
      arr[2] = (byte) ((at >> 8) & 0xff);
      arr[3] = (byte) (at & 0xff);
      return arr;
    }

    public record CachedModule(Module moduleIR, CompilationStage compilationStage, Source source) {
    }

    public record Metadata(
            @JsonProperty("source_hash") String sourceHash,
            @JsonProperty("blob_hash") String blobHash,
            @JsonProperty("compilation_stage") String compilationStage) implements Cache.Metadata {}

    private final static String irCacheDataExtension = ".ir";

    private final static String irCacheMetadataExtension = ".meta";

    private final static ObjectMapper objectMapper = new ObjectMapper();

}
