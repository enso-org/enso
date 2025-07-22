package org.enso.microsoft.nativeimage;

import com.azure.core.implementation.ReflectionSerializable;
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
 * Manually registers some classes and methods for reflection from {@code azure-*.jar} files on
 * classpath that are not registered by default in {@code reflection-config.json} files.
 *
 * <p>Note that there are various {@code reflection-config.json} files in azure modules, but they do
 * not list any classes that implement {@code XmlSerializable} interface.
 */
public class AzureNativeImageFeature implements Feature {

  private static final String XML_SERIALIZABLE_CLASS_NAME = "com.azure.xml.XmlSerializable";

  private int registeredClasses;
  private int registeredMethods;
  private int registeredFields;
  private int registeredConstructors;

  @Override
  public void beforeAnalysis(BeforeAnalysisAccess access) {
    registeredClasses = 0;
    registeredMethods = 0;
    registeredFields = 0;
    registeredConstructors = 0;
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

    for (var path : access.getApplicationClassPath()) {
      var fileName = path.getFileName().toString();
      if (fileName.startsWith("azure") && fileName.endsWith(".jar")) {
        var xmlClasses = findImplementationClasses(access, path, xmlSerializableClass);
        if (!xmlClasses.isEmpty()) {
          System.out.println(
              "[AzureNativeImageFeature] Found "
                  + xmlClasses.size()
                  + " classes implementing "
                  + xmlSerializableClass.getName()
                  + " in "
                  + fileName);
          for (var xmlClass : xmlClasses) {
            registerXmlClassForReflection(xmlClass);
          }
        }

        var respExClasses = findImplementationClasses(access, path, httpExceptionClass);
        if (!respExClasses.isEmpty()) {
          System.out.println(
              "[AzureNativeImageFeature] Found "
                  + respExClasses.size()
                  + " classes implementing "
                  + httpExceptionClass.getName()
                  + " in "
                  + fileName);
          for (var respExClass : respExClasses) {
            registerForReflection(respExClass);
          }
        }
      }
    }
    registerForReflection(xmlSerializableClass);
    registerForReflection(httpExceptionClass);
    System.out.printf(
        "[AzureNativeImageFeature] Registered %d classes, %d methods, %d fields, and %d"
            + " constructors for reflection.%n",
        registeredClasses, registeredMethods, registeredFields, registeredConstructors);
  }

  private void registerXmlClassForReflection(Class<?> clazz) {
    RuntimeReflection.register(clazz);
    registeredClasses++;
    registerXmlMethodsForReflection(clazz);
  }

  /**
   * Checks for the {@code toXml} and {@code fromXml} methods are inspired by {@link
   * ReflectionSerializable#supportsXmlSerializable(Class)}.
   */
  private void registerXmlMethodsForReflection(Class<?> clazz) {
    for (var method : clazz.getDeclaredMethods()) {
      if (method.getName().equals("fromXml")) {
        RuntimeReflection.register(method);
        registeredMethods++;
      } else if (method.getName().equals("toXml")) {
        RuntimeReflection.register(method);
        registeredMethods++;
      }
    }
  }

  private void registerForReflection(Class<?> clazz) {
    RuntimeReflection.register(clazz);
    registeredClasses++;
    var ctors = clazz.getConstructors();
    RuntimeReflection.register(ctors);
    registeredConstructors += ctors.length;

    var methods = clazz.getMethods();
    RuntimeReflection.register(methods);
    registeredMethods += methods.length;

    var fields = clazz.getFields();
    RuntimeReflection.register(fields);
    registeredFields += fields.length;

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
