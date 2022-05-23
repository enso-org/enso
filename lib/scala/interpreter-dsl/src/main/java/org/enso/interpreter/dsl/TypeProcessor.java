package org.enso.interpreter.dsl;

import javax.annotation.processing.*;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
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

  private class BuiltinTypeConstr {
    private String tpeName;
    private String fullName;
    private String[] paramNames;

    BuiltinTypeConstr(String tpeName, String fullName, String[] params) {
      this.tpeName = tpeName;
      this.fullName = fullName;
      this.paramNames = params;
    }

    public String getFullName() {
      return fullName;
    }

    public String getTpeName() {
      return tpeName;
    }

    public String[] getParamNames() {
      return paramNames;
    }
  }

  public static final String NODE_PKG = "org.enso.interpreter.node.expression.builtin";
  public static final String META_PATH =
      "META-INF" + "/" + NODE_PKG.replace('.', '/') + "/BuiltinTypes.metadata";

  @Override
  protected boolean handleProcess(
      Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
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
            builtinTypeAnnotation.name(),
            builtinTypeAnnotation.params());
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
    JavaFileObject gen = processingEnv.getFiler().createSourceFile(ConstantsGenFullClassname);
    for (Filer f : builtinTypes.keySet()) {
      System.out.println("foo" + f.toString());
      for (Map.Entry<String, BuiltinTypeConstr> entry : builtinTypes.get(f).entrySet()) {
        BuiltinTypeConstr constr = entry.getValue();
        writer.append(
            entry.getKey()
                + ":"
                + constr.getTpeName()
                + ":"
                + StringUtils.join(Arrays.asList(constr.getParamNames()), ",")
                + ":"
                + constr.getFullName()
                + "\n");
        if (pastEntries.containsKey(entry.getKey())) {
          pastEntries.remove(entry.getKey());
        }
      }
    }
    try (PrintWriter out = new PrintWriter(gen.openWriter())) {
      out.println("package " + ConstantsGenPkg + ";");
      out.println();
      out.println("public class " + ConstantsGenClass + " {");
      out.println();
      for (Filer f : builtinTypes.keySet()) {
        for (Map.Entry<String, BuiltinTypeConstr> entry : builtinTypes.get(f).entrySet()) {
          BuiltinTypeConstr constr = entry.getValue();
          if (!constr.getFullName().isEmpty()) {
            out.println(
                "  public static final String "
                    + entry.getKey().toUpperCase()
                    + " = \""
                    + constr.getFullName()
                    + "\";");
          }
        }
      }

      pastEntries
          .values()
          .forEach(
              entry ->
                entry.stdlibName().ifPresent(n ->
                        out.println(
                        "    public static final String "
                                + entry.ensoName().toUpperCase()
                                + " = \""
                                + n
                                + "\";")
                )
          );

      out.println();
      out.println("}");
    }
  }

  protected void registerBuiltinType(
      Filer f, String name, String clazzName, String fullName, String[] params) {
    Map<String, BuiltinTypeConstr> classes = builtinTypes.get(f);
    if (classes == null) {
      classes = new HashMap<>();
      builtinTypes.put(f, classes);
    }
    classes.put(name, new BuiltinTypeConstr(clazzName, fullName, params));
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

  public record TypeMetadataEntry(String ensoName, String clazzName, String[] paramNames, Optional<String> stdlibName) implements MetadataEntry {

    @Override
    public String toString() {
      return ensoName + ":" + clazzName + ":" + StringUtils.join(paramNames, ",") + ":" + stdlibName.orElse("");
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
    String[] params = elements.length >= 3 ? elements[2].split(",") : new String[0];
    Optional<String> stdLibName = elements.length == 4 ? Optional.of(elements[3]) : Optional.empty();
    return new TypeMetadataEntry(elements[0], elements[1], params, stdLibName);
  }
}
