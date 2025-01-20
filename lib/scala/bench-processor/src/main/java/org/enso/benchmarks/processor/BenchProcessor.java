package org.enso.benchmarks.processor;

import java.io.File;
import java.io.IOException;
import java.lang.module.FindException;
import java.lang.module.ModuleFinder;
import java.lang.module.ResolutionException;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.BiConsumer;
import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.ProcessingEnvironment;
import javax.annotation.processing.Processor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic.Kind;
import org.enso.benchmarks.Utils;
import org.openide.util.lookup.ServiceProvider;

@SupportedAnnotationTypes("org.enso.benchmarks.processor.GenerateBenchSources")
@ServiceProvider(service = Processor.class)
public class BenchProcessor extends AbstractProcessor {

  private static final String MODULE_PATH_PROP_NAME =
      BenchProcessor.class.getName() + ".modulePath";

  @Override
  public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latest();
  }

  private static boolean shouldCreateModuleLayer() {
    return System.getProperty(MODULE_PATH_PROP_NAME) != null;
  }

  /**
   * This annotation processor is invoked from frgaal compiler. Which means that all the
   * truffle-related modules are not on boot module layer. To avoid problems with double class
   * loading with different class loaders, we explicitly create our own module layer for truffle and
   * execute all the polyglot code in this layer, with the associated class loader. Note that
   * usually, this is done by Truffle itself during initialization. We must ensure that the {@code
   * SpecCollector} is loaded via the class loader used for loading the {@code org.graalvm.truffle}
   * (or {@code org.enso.runtime}) module.
   */
  private static ModuleLayer createTruffleModuleLayer() {
    // Note that we cannot use java.class.path here as for frgaal compiler, there is just
    // one jar on the classpath, and it is the frgaal compiler jar.
    var mp = System.getProperty(MODULE_PATH_PROP_NAME);
    if (mp == null) {
      throw new IllegalStateException(
          MODULE_PATH_PROP_NAME
              + " system property must be set so that the annotation processor "
              + "is able to create the truffle module layer");
    }
    System.out.println(
        "BenchProcessor: "
            + MODULE_PATH_PROP_NAME
            + " system property set. "
            + "Creating truffle module layer for benchmark collection.");
    var paths = Arrays.stream(mp.split(File.pathSeparator)).map(Path::of).toArray(Path[]::new);
    var bootLayer = ModuleLayer.boot();
    var bootConf = bootLayer.configuration();
    var modFinder = ModuleFinder.of(paths);
    if (modFinder.find("org.graalvm.truffle").isEmpty()) {
      throw new IllegalStateException("org.graalvm.truffle module not found in the classpath");
    }
    // We are creating new module layer, with the boot module layer as the parent. In the boot
    // module layer, there are base JDK modules that are required, but there are also some modules
    // that
    // may be duplicated on the system property. For example, it is likely that there is
    // `org.graalvm.word` module which is also in the boot module layer (as it is part of the JDK).
    // We have to filter these modules out of the new configuration.
    Set<String> duplicatedModPaths = new HashSet<>();
    for (var bootMod : bootConf.modules()) {
      var modRefOpt = modFinder.find(bootMod.name());
      modRefOpt.ifPresent(
          modRef -> {
            var uri =
                modRef
                    .location()
                    .orElseThrow(
                        () ->
                            new AssertionError(
                                "Module " + modRef.descriptor().name() + " has no location"));
            duplicatedModPaths.add(new File(uri).getPath());
          });
    }
    // Path to modules without duplicated ones.
    List<Path> pathsToUse = new ArrayList<>();
    for (var path : paths) {
      var pathStr = path.toAbsolutePath().toString();
      if (!duplicatedModPaths.contains(pathStr)) {
        pathsToUse.add(path);
      }
    }
    var rootModules =
        Set.of(
            "org.graalvm.truffle",
            "com.oracle.truffle.regex",
            "org.enso.runtime",
            "org.enso.runtime.language.epb",
            "org.enso.interpreter.arrow",
            "org.enso.bench.processor");
    try {
      var truffleLayer = createLayerFromPaths(pathsToUse, rootModules, bootLayer);
      return truffleLayer;
    } catch (FindException | ResolutionException e) {
      System.out.println("Error creating truffle module layer: " + e.getMessage() + " :");
      System.out.println("  Paths used for the module finder: " + pathsToUse);
      System.out.println("  Out of all paths: " + Arrays.toString(paths));
      System.out.println("  Root modules for resolution: " + rootModules);
      System.out.println("  Boot layer: " + bootLayer);
      throw new AssertionError(e);
    }
  }

  /**
   * A callback called from {@code SpecCollector} so that we don't have to pass the {@link
   * ProcessingEnvironment} directly to it.
   *
   * @param className Name of the class to create.
   * @param content Source code for the class.
   */
  void createSourceFile(String className, String content) {
    try (var os = processingEnv.getFiler().createSourceFile(className).openOutputStream()) {
      os.write(content.getBytes(StandardCharsets.UTF_8));
    } catch (IOException e) {
      throw new IllegalStateException(
          "Unexpected excpetion when creating source file for class " + className, e);
    }
  }

  /**
   * Creates a new module layer from paths with the given root modules with boot module layer as
   * parent.
   *
   * @param paths Paths to modules.
   * @param rootModules Set of root modules to resolve.
   * @return New module layer.
   */
  private static ModuleLayer createLayerFromPaths(
      List<Path> paths, Set<String> rootModules, ModuleLayer parentLayer) {
    var modFinder = ModuleFinder.of(paths.toArray(Path[]::new));
    var modConf = parentLayer.configuration().resolve(modFinder, ModuleFinder.of(), rootModules);
    return parentLayer.defineModulesWithOneLoader(modConf, ClassLoader.getSystemClassLoader());
  }

  /**
   * @param runtimeLoader Class loader for the {@code org.enso.runtime} module.
   */
  private void generateBenchSpecs(
      ClassLoader runtimeLoader,
      String moduleName,
      String varName,
      File projectRootDir,
      File ensoHomeOverride) {
    try {
      var specCollectorClazz =
          runtimeLoader.loadClass("org.enso.benchmarks.processor.SpecCollector");
      var createMethod =
          specCollectorClazz.getDeclaredMethod(
              "create", String.class, String.class, File.class, File.class, BiConsumer.class);
      BiConsumer<String, String> createSrcFileFn = this::createSourceFile;
      var specCollector =
          createMethod.invoke(
              null, moduleName, varName, projectRootDir, ensoHomeOverride, createSrcFileFn);
      var genSpecsMethod = specCollectorClazz.getMethod("generateBenchSpecs");
      genSpecsMethod.invoke(specCollector);
    } catch (ClassNotFoundException
        | NoSuchMethodException
        | InvocationTargetException
        | IllegalAccessException e) {
      throw new IllegalStateException(e);
    } catch (Exception ex) {
      processingEnv.getMessager().printMessage(Kind.ERROR, ex.getMessage());
      // Better have duplicated error message than none at all
      System.out.println(
          "[org.enso.benchmarks.processor.BenchProcessor]: ERROR: " + ex.getMessage());
      ex.printStackTrace(System.out);
    }
  }

  @Override
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    if (annotations.isEmpty()) {
      return true;
    }
    var elements = roundEnv.getElementsAnnotatedWith(GenerateBenchSources.class);
    if (!elements.isEmpty()) {
      File ensoDir = Utils.findRepoRootDir();
      // Note that ensoHomeOverride does not have to exist, only its parent directory
      var ensoHomeOverride = ensoDir.toPath().resolve("distribution").resolve("component").toFile();
      ClassLoader runtimeLoader;
      if (shouldCreateModuleLayer()) {
        var truffleModLayer = createTruffleModuleLayer();
        runtimeLoader = truffleModLayer.findLoader("org.enso.runtime");
      } else {
        runtimeLoader = BenchProcessor.class.getClassLoader();
      }
      for (var element : elements) {
        GenerateBenchSources annotation = element.getAnnotation(GenerateBenchSources.class);
        var projectRootDir = new File(annotation.projectRootPath());
        var moduleName = annotation.moduleName();
        var varName = annotation.variableName();
        generateBenchSpecs(runtimeLoader, moduleName, varName, projectRootDir, ensoHomeOverride);
      }
    }
    return true;
  }
}
