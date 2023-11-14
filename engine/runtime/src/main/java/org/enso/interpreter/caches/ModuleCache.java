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
import com.esotericsoftware.kryo.util.MapReferenceResolver;
import io.altoo.serialization.kryo.scala.serializer.ScalaImmutableAbstractMapSerializer;
import io.altoo.serialization.kryo.scala.serializer.ScalaImmutableAbstractSetSerializer;
import io.altoo.serialization.kryo.scala.serializer.ScalaMutableMapSerializer;
import io.altoo.serialization.kryo.scala.serializer.SubclassResolver;
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
      if (kryo.getClassResolver() instanceof SubclassResolver resolver) {
          resolver.enable();
      }
      kryo.setWarnUnregisteredClasses(true);
      kryo.addDefaultSerializer(java.util.UUID.class, UUIDSerializer.class);
      kryo.addDefaultSerializer(scala.collection.Set.class, ScalaImmutableAbstractSetSerializer.class);
      kryo.addDefaultSerializer(scala.collection.Map.class, ScalaImmutableAbstractMapSerializer.class);
      kryo.addDefaultSerializer(scala.collection.mutable.Map.class, ScalaMutableMapSerializer.class);
      kryo.register(java.util.UUID.class);
      kryo.register(scala.collection.immutable.Set.class);
      kryo.register(scala.collection.immutable.Map.class);
      kryo.register(scala.collection.immutable.List.class);
        kryo.register(scala.collection.immutable.$colon$colon.class);
      kryo.register(scala.collection.mutable.HashMap.class);
      kryo.register(org.enso.compiler.core.ir.Module.class);
      kryo.register(org.enso.compiler.core.ir.module.scope.definition.Method.Explicit.class);
      kryo.register(org.enso.compiler.core.ir.IdentifiedLocation.class);
        kryo.register(org.enso.compiler.core.ir.DefinitionArgument.Specified.class);
        kryo.register(org.enso.compiler.core.ir.expression.Application.Prefix.class);
        kryo.register(org.enso.compiler.core.ir.CallArgument.Specified.class);
        kryo.register(org.enso.compiler.core.ir.DiagnosticStorage.class);
        kryo.register(org.enso.compiler.core.ir.Function.Lambda.class);
        kryo.register(org.enso.compiler.core.ir.Literal.Number.class);
        kryo.register(org.enso.compiler.core.ir.Name.Self.class);
        kryo.register(org.enso.compiler.core.ir.Name.Literal.class);
        kryo.register(org.enso.compiler.core.ir.Name.MethodReference.class);
        kryo.register(org.enso.compiler.core.ir.expression.Application.Prefix.class);
        kryo.register(org.enso.compiler.core.ir.MetadataStorage.class);

        kryo.register(org.enso.compiler.pass.analyse.TailCall$.class);
        kryo.register(org.enso.compiler.pass.analyse.AliasAnalysisDefs.Scope.class);
        kryo.register(org.enso.compiler.pass.analyse.DataflowAnalysis.DependencyMapping.class);
        kryo.register(org.enso.compiler.pass.analyse.DataflowAnalysis.DependencyInfo.class);
        kryo.register(org.enso.compiler.pass.resolve.IgnoredBindings$.class);

        kryo.register(org.enso.compiler.pass.analyse.AliasAnalysis$.class);
        kryo.register(org.enso.compiler.pass.analyse.AliasAnalysisDefs.Occurrence.class);
        kryo.register(org.enso.compiler.pass.analyse.AliasAnalysis.Graph.class);
        kryo.register(org.enso.syntax.text.Location.class);

        kryo.register(org.enso.compiler.pass.analyse.CachePreferenceAnalysis$.class);
        kryo.register(org.enso.compiler.pass.analyse.CachePreferenceAnalysis.WeightInfo.class);
        kryo.register(scala.collection.immutable.HashMap.class);
        kryo.register(org.enso.compiler.pass.resolve.ModuleAnnotations$.class);
        kryo.register(org.enso.compiler.pass.resolve.ModuleAnnotations.Annotations.class);
        kryo.register(org.enso.compiler.pass.resolve.GenericAnnotations$.class);
        kryo.register(org.enso.compiler.pass.resolve.GlobalNames$.class);
        kryo.register(org.enso.compiler.pass.analyse.BindingAnalysis$.class);
        kryo.register(org.enso.compiler.data.BindingsMap.class);
        kryo.register(org.enso.pkg.QualifiedName.class);
        kryo.register(org.enso.compiler.data.BindingsMap.ModuleMethod.class);
        kryo.register(org.enso.compiler.data.BindingsMap.ResolvedMethod.class);
        kryo.register(org.enso.compiler.data.BindingsMap.Resolution.class);
        kryo.register(org.enso.interpreter.caches.ModuleCache.Metadata.class);


        kryo.register(scala.collection.immutable.Nil$.class);

        kryo.register(scala.None$.class);
        kryo.register(scala.Some.class);
    }

    @Override
    protected byte[] metadata(String sourceDigest, String blobDigest, CachedModule entry) {
        var stream = new ByteArrayOutputStream(4095);
        try (var output = new Output(stream)) {
            var object = new Metadata(sourceDigest, blobDigest, entry.compilationStage().toString());
            kryo.writeClassAndObject(output, object);
        }
        return stream.toByteArray();
    }

    @Override
    protected CachedModule deserialize(EnsoContext context, byte[] data, Metadata meta, TruffleLogger logger) throws ClassNotFoundException, IOException, ClassNotFoundException {
        try (var stream = new Input(data)) {
          Object ir = kryo.readClassAndObject(stream);
              try {
                  return new CachedModule((Module)ir,CompilationStage.valueOf(meta.compilationStage()), module.getSource());
              } catch (IOException ioe) {
                  throw new ClassNotFoundException(ioe.getMessage());
              }
        }
    }

    @Override
    protected Optional<Metadata> metadataFromBytes(byte[] bytes, TruffleLogger logger) {
        try (var stream = new Input(bytes)) {
            Object metadata = kryo.readClassAndObject(stream);
            return Optional.of((Metadata)metadata);
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
      var byteStream = new ByteArrayOutputStream(4096);
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
            output = new Output(stream, 4096);
            kryo.writeClassAndObject(output, entry.moduleIR());
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
    private final static Kryo kryo = new Kryo(new SubclassResolver(), new MapReferenceResolver());

}
