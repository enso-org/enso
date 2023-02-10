package org.enso.interpreter.dsl;

import javax.annotation.processing.*;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.*;

import org.apache.commons.lang3.StringUtils;
import org.openide.util.lookup.ServiceProvider;

@SupportedAnnotationTypes("org.enso.interpreter.dsl.BuiltinType")
@ServiceProvider(service = Processor.class)
public class TypeProcessor extends BuiltinsMetadataProcessor<TypeProcessor.TypeMetadataEntry> {

  private final Map<Filer, Map<String, BuiltinTypeConstr>> builtinTypes = new HashMap<>();
  private JavaFileObject jfo = null;

  private class BuiltinTypeConstr {
    private String tpeName;
    private String fullName;

    BuiltinTypeConstr(String tpeName, String fullName) {
      this.tpeName = tpeName;
      this.fullName = fullName;
    }

    public String getFullName() {
      return fullName;
    }

    public String getTpeName() {
      return tpeName;
    }
  }

  public static final String NODE_PKG = "org.enso.interpreter.node.expression.builtin";
  public static final String META_PATH =
      "META-INF" + "/" + NODE_PKG.replace('.', '/') + "/BuiltinTypes.metadata";

  @Override
  protected boolean handleProcess(
      Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    if (jfo == null) {
      // Create generator for Java source file once(!) so that it can be used at the last
      // round of processing. Otherwise, javac complains that the generated file won't be
      // used in further processing. That's fine, we know it won't.
      try {
        jfo = processingEnv.getFiler().createSourceFile(ConstantsGenFullClassname);
      } catch (IOException e) {
        processingEnv.getMessager().printMessage(Diagnostic.Kind.ERROR, e.getMessage());
        return false;
      }
    }
    for (TypeElement annotation : annotations) {
      Set<? extends Element> annotatedElements = roundEnv.getElementsAnnotatedWith(annotation);
      for (Element elt : annotatedElements) {
        TypeElement element = (TypeElement) elt;
        BuiltinType builtinTypeAnnotation = element.getAnnotation(BuiltinType.class);
        String pkgName =
            processingEnv.getElementUtils().getPackageOf(element).getQualifiedName().toString();
        String clazzName = element.getSimpleName().toString();
        // Replace CamelCase class name to Snake_Case used in Enso
        String ensoTypeName = clazzName.replaceAll("([^_A-Z])([A-Z])", "$1_$2");
        registerBuiltinType(
            processingEnv.getFiler(),
            ensoTypeName,
            pkgName + "." + clazzName,
            builtinTypeAnnotation.name());
      }
    }
    return true;
  }

  /**
   * Dumps the information about the collected builtin types to {@link
   * MethodProcessor#metadataPath()} resource file.
   *
   * <p>The format of a single row in the metadata file: <Enso name of the builtin type>:<class
   * representing the builtin type>:[<builtin type's comma separated fields>]
   *
   * @param writer a writer to the metadata resource
   * @param pastEntries entries from the previously created metadata file, if any. Entries that
   *     should not be appended to {@code writer} should be removed
   * @throws IOException
   */
  @Override
  protected void storeMetadata(Writer writer, Map<String, TypeMetadataEntry> pastEntries) throws IOException {
    for (Filer f : builtinTypes.keySet()) {
      for (Map.Entry<String, BuiltinTypeConstr> entry : builtinTypes.get(f).entrySet()) {
        BuiltinTypeConstr constr = entry.getValue();
        writer.append(
            entry.getKey()
                + ":"
                + constr.getTpeName()
                + ":"
                + constr.getFullName()
                + "\n");
        if (pastEntries.containsKey(entry.getKey())) {
          pastEntries.remove(entry.getKey());
        }
      }
    }
    try (PrintWriter out = new PrintWriter(jfo.openWriter())) {
      out.println("package " + ConstantsGenPkg + ";");
      out.println();
      out.println("public class " + ConstantsGenClass + " {");
      out.println();
      for (Filer f : builtinTypes.keySet()) {
        for (Map.Entry<String, BuiltinTypeConstr> entry : builtinTypes.get(f).entrySet()) {
          BuiltinTypeConstr constr = entry.getValue();
          if (!constr.getFullName().isEmpty()) {
            generateEntry(entry.getKey().toUpperCase(), constr.getFullName(), out);
          }
        }
      }

      pastEntries
          .values()
          .forEach(
              entry ->
                entry.stdlibName().ifPresent(n -> generateEntry(entry.ensoName().toUpperCase(), n, out))
          );

      out.println();
      out.println("}");
    }
  }

  public void generateEntry(String name, String value, PrintWriter out) {
    out.println(
            "  public static final String "
                    + name
                    + " = \""
                    + value
                    + "\";");
    out.println(
            "  public static final String "
                    + name + "_BUILTIN"
                    + " = "
                    + toBuiltinName(value)
                    + ";");
  }

  private String toBuiltinName(String name) {
    return "Constants.BUILTIN_NAMESPACE + \"." + name.substring(name.lastIndexOf('.') + 1) + "\"";
  }

  protected void registerBuiltinType(
      Filer f, String name, String clazzName, String fullName) {
    Map<String, BuiltinTypeConstr> classes = builtinTypes.get(f);
    if (classes == null) {
      classes = new HashMap<>();
      builtinTypes.put(f, classes);
    }
    classes.put(name, new BuiltinTypeConstr(clazzName, fullName));
  }

  @Override
  protected String metadataPath() {
    return META_PATH;
  }

  @Override
  protected void cleanup() {
    builtinTypes.clear();
  }

  private static final String ConstantsGenPkg = "org.enso.interpreter.runtime.type";
  private static final String ConstantsGenClass = "ConstantsGen";
  private static final String ConstantsGenFullClassname = ConstantsGenPkg + "." + ConstantsGenClass;

  @Override
  public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latest();
  }

  public record TypeMetadataEntry(String ensoName, String clazzName, Optional<String> stdlibName) implements MetadataEntry {

    @Override
    public String toString() {
      return ensoName + ":" + clazzName + ":" + stdlibName.orElse("");
    }

    @Override
    public String key() {
      return ensoName;
    }
  }

  @Override
  protected TypeMetadataEntry toMetadataEntry(String line) {
    return fromStringToMetadataEntry(line);
  }

  public static TypeMetadataEntry fromStringToMetadataEntry(String line) {
    String[] elements = line.split(":");
    if (elements.length < 2) throw new RuntimeException("invalid builtin metadata entry: " + line);
    Optional<String> stdLibName = elements.length == 3 ? Optional.of(elements[2]) : Optional.empty();
    return new TypeMetadataEntry(elements[0], elements[1], stdLibName);
  }
}
