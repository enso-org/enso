package org.enso.interpreter.caches;

import buildinfo.Info;
import com.esotericsoftware.kryo.Kryo;
import com.esotericsoftware.kryo.io.Output;
import com.esotericsoftware.kryo.io.Input;
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
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Optional;
import java.util.UUID;
import java.util.logging.Level;

public final class ModuleCache extends Cache<ModuleCache.CachedModule, ModuleCache.Metadata> {

    private final org.enso.interpreter.runtime.Module module;

    public ModuleCache(org.enso.interpreter.runtime.Module module) {
      super(Level.FINEST, module.getName().toString(), true, false);
      this.module = module;
      this.entryName = module.getName().item();
      this.dataSuffix = irCacheDataExtension;
      this.metadataSuffix = irCacheMetadataExtension;
      kryo.setRegistrationRequired(false);
    }

    @Override
    protected byte[] metadata(String sourceDigest, String blobDigest, CachedModule entry) {
            try (var output = new Output()) {
                var object = new Metadata(sourceDigest, blobDigest, entry.compilationStage().toString());
                kryo.writeObject(output, object);
                return output.toBytes();
            }
    }

    @Override
    protected CachedModule deserialize(EnsoContext context, byte[] data, Metadata meta, TruffleLogger logger) throws ClassNotFoundException, IOException, ClassNotFoundException {
        try (var stream = new Input(data)) {
          Module ir = kryo.readObject(stream, Module.class);
              try {
                  return new CachedModule(ir,CompilationStage.valueOf(meta.compilationStage()), module.getSource());
              } catch (IOException ioe) {
                  throw new ClassNotFoundException(ioe.getMessage());
              }
        }
    }

    @Override
    protected Optional<Metadata> metadataFromBytes(byte[] bytes, TruffleLogger logger) {
        try (var stream = new Input(bytes)) {
            Metadata metadata = kryo.readObject(stream, Metadata.class);
            return Optional.of(metadata);
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
      try (var stream = new ObjectOutputStream(byteStream) {
        void filterUUIDs() {
          enableReplaceObject(true);
        }

        @Override
        protected Object replaceObject(Object obj) throws IOException {
          if (obj instanceof UUID) {
            return null;
          }
          return obj;
        }
      }) {
        if (noUUIDs) {
          stream.filterUUIDs();
        }
        Output output = null;
        try {
            output = new Output(stream);
            kryo.writeObject(output, entry.moduleIR());
        } finally {
            output.close();
        }
      }
      return byteStream.toByteArray();
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
    private final static Kryo kryo = new Kryo();

}
