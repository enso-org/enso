package org.enso.microsoft.nativeimage;

import java.io.IOException;
import java.lang.reflect.Modifier;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.jar.JarFile;
import org.graalvm.nativeimage.hosted.Feature;
import org.graalvm.nativeimage.hosted.RuntimeProxyCreation;
import org.graalvm.nativeimage.hosted.RuntimeReflection;

/**
 * Explicitly registers all the classes that implement {@link com.azure.xml.XmlSerializable}
 * interface for reflection. This is needed for {@code azure-core} that (de)serializes objects into
 * XML via reflection.
 *
 * <p>Traverses all the {@code azure-*.jar} files on classpath.
 *
 * <p>Note that there are various {@code reflection-config.json} files in azure modules, but they do
 * not list any classes that implement {@code XmlSerializable} interface.
 */
public class AzureNativeImageFeature implements Feature {

  private static final String XML_SERIALIZABLE_CLASS_NAME = "com.azure.xml.XmlSerializable";

  @Override
  public void beforeAnalysis(BeforeAnalysisAccess access) {
    System.out.println("[AzureNativeImageFeature] Registering classes for reflection...");
    var xmlSerializableClass = access.findClassByName(XML_SERIALIZABLE_CLASS_NAME);
    if (xmlSerializableClass == null) {
      throw new IllegalStateException(
          "XmlSerializable class not found: " + XML_SERIALIZABLE_CLASS_NAME);
    }
    var httpExceptionClass =
        access.findClassByName("com.azure.core.exception.HttpResponseException");
    if (httpExceptionClass == null) {
      throw new IllegalStateException(
          "HttpResponseException class not found: com.azure.core.exception.HttpResponseException");
    }

    var classesForReflection = new ArrayList<Class<?>>();
    for (var path : access.getApplicationClassPath()) {
      var fileName = path.getFileName().toString();
      if (fileName.startsWith("azure") && fileName.endsWith(".jar")) {
        var xmlClasses = findImplementationClasses(access, path, xmlSerializableClass);
        System.out.println(
            "[AzureNativeImageFeature] Found "
                + xmlClasses.size()
                + " classes implementing XmlSerializable in "
                + fileName);
        var respExClasses = findImplementationClasses(access, path, httpExceptionClass);
        System.out.println(
            "[AzureNativeImageFeature] Found "
                + respExClasses.size()
                + " classes implementing HttpResponseException in "
                + fileName);
        classesForReflection.addAll(xmlClasses);
        classesForReflection.addAll(respExClasses);
      }
    }
    System.out.println(
        "Registering " + classesForReflection.size() + " classes for runtime reflection.");
    registerForReflection(xmlSerializableClass);
    registerForReflection(httpExceptionClass);
    for (var klazz : classesForReflection) {
      // TODO: Register only `toXml` and `fromXml` methods.
      registerForReflection(klazz);
    }
  }

  private static void registerForReflection(Class<?> clazz) {
    RuntimeReflection.register(clazz);
    RuntimeReflection.register(clazz.getConstructors());
    RuntimeReflection.register(clazz.getMethods());
    RuntimeReflection.register(clazz.getFields());
    RuntimeReflection.registerAllConstructors(clazz);
    RuntimeReflection.registerAllMethods(clazz);
    RuntimeReflection.registerAllFields(clazz);
    if (clazz.isInterface()) {
      RuntimeProxyCreation.register(clazz);
    }
  }

  private static List<Class<?>> findImplementationClasses(
      BeforeAnalysisAccess access, Path jarPath, Class<?> baseClass) {
    List<Class<?>> xmlSerializableClasses = new ArrayList<>();
    try (var jarFile = new JarFile(jarPath.toFile())) {
      var entries = jarFile.entries();
      while (entries.hasMoreElements()) {
        var entry = entries.nextElement();
        var entryName = entry.getName();
        if (entryName.endsWith(".class")) {
          var className = entryName.replace('/', '.').replace(".class", "");
          var klazz = access.findClassByName(className);
          if (klazz != null && implementsBaseClass(baseClass, klazz)) {
            xmlSerializableClasses.add(klazz);
          }
        }
      }
    } catch (IOException e) {
      throw new IllegalStateException("Failed to read classes from azure-core jar: " + jarPath, e);
    }
    return xmlSerializableClasses;
  }

  private static boolean implementsBaseClass(Class<?> baseClass, Class<?> clazz) {
    return baseClass.isAssignableFrom(clazz)
        && !clazz.isInterface()
        && !Modifier.isAbstract(clazz.getModifiers());
  }
}
