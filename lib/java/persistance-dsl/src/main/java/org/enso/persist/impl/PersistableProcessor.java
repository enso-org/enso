package org.enso.persist.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.stream.Collectors;
import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.Processor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.AnnotationMirror;
import javax.lang.model.element.AnnotationValue;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.TypeMirror;
import javax.lang.model.util.SimpleAnnotationValueVisitor9;
import javax.lang.model.util.Types;
import javax.tools.Diagnostic.Kind;
import javax.tools.StandardLocation;
import org.openide.util.lookup.ServiceProvider;

/**
 * Processes the {@code Persistable} annotation. See its javadoc for proper usage. A new subclass of
 * {@code Persistance} class is generated per each {@code Persistable} annotation.
 */
@SupportedAnnotationTypes({"org.enso.persist.Persistable", "org.enso.persist.Persistable.Group"})
@ServiceProvider(service = Processor.class)
public class PersistableProcessor extends AbstractProcessor {
  private final Map<String, Map<Integer, String>> registeredClasses = new TreeMap<>();

  @Override
  public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latest();
  }

  @Override
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    var ok = true;
    var eu = processingEnv.getElementUtils();
    var tu = processingEnv.getTypeUtils();
    var Persistable = eu.getTypeElement("org.enso.persist.Persistable");
    var PersistableGroup = eu.getTypeElement("org.enso.persist.Persistable.Group");
    try {
      for (var elem : roundEnv.getElementsAnnotatedWith(Persistable)) {
        var anno = getAnnotation(elem, Persistable);
        ok &= generatePersistance(elem, anno);
      }
      for (var elem : roundEnv.getElementsAnnotatedWith(PersistableGroup)) {
        var group = getAnnotation(elem, PersistableGroup);
        for (var anno : readAnnoArray(group, "value")) {
          ok &= generatePersistance(elem, anno);
        }
      }
    } catch (IOException e) {
      ok = false;
      processingEnv.getMessager().printMessage(Kind.ERROR, e.getMessage());
    }
    if (roundEnv.processingOver()) {
      for (var entry : registeredClasses.entrySet()) {
        try {
          var props = new Properties();
          var propsWhere = StandardLocation.SOURCE_OUTPUT;
          var propsPkg = entry.getKey();
          var propsName = "Persistables.properties";
          var cn = entry.getKey() + ".Persistables";
          try {
            var res = processingEnv.getFiler().getResource(propsWhere, propsPkg, propsName);
            if (res != null) {
              try (var is = res.openInputStream()) {
                props.load(is);
              }
            }
          } catch (IOException notReallyImportant) {
            // Not actionable
          }
          var src = processingEnv.getFiler().createSourceFile(cn);
          try (var w = src.openWriter()) {
            // values from processor take preceedence
            for (var idName : entry.getValue().entrySet()) {
              props.setProperty("" + idName.getKey(), idName.getValue());
            }

            w.append("package " + entry.getKey() + ";\n");
            w.append("import org.enso.persist.Persistance;\n");
            w.append("public final class Persistables extends Persistance.Pool {\n");
            w.append("  public static final Persistance.Pool POOL = new Persistables();\n");
            w.append("  private Persistables() {\n");
            w.append("    super(\"").append(entry.getKey()).append("\",");
            var lineEnding = "\n";
            for (var idName : props.entrySet()) {
              w.append(lineEnding);
              w.append("      new " + idName.getValue() + "()");
              lineEnding = ",\n";
            }
            w.append("\n    );\n");
            w.append("  }\n");
            w.append("}\n");
          }
          var out = processingEnv.getFiler().createResource(propsWhere, propsPkg, propsName);
          try (var os = out.openOutputStream()) {
            // store accumulated key/value pairs for subsequent (incremental) update
            props.store(os, "");
          }
        } catch (IOException ex) {
          processingEnv.getMessager().printMessage(Kind.ERROR, ex.getMessage());
          ok = false;
        }
      }
    }
    return ok;
  }

  private String findFqn(Element e) {
    Objects.requireNonNull(e);
    var inPackage = findNameInPackage(e);
    var pkg = processingEnv.getElementUtils().getPackageOf(e);
    return pkg.getQualifiedName() + "." + inPackage;
  }

  private static String findNameInPackage(Element e) {
    var sb = new StringBuilder();
    while (e != null && !(e instanceof PackageElement)) {
      if (sb.length() > 0) {
        sb.insert(0, ".");
      }
      sb.insert(0, e.getSimpleName());
      e = e.getEnclosingElement();
    }
    return sb.toString();
  }

  private boolean generatePersistance(Element orig, AnnotationMirror anno) throws IOException {
    var eu = processingEnv.getElementUtils();
    var tu = processingEnv.getTypeUtils();
    var Persistance = eu.getTypeElement("org.enso.persist.Persistance");
    var PersistanceRaw = tu.erasure(Persistance.asType());
    var typeElemName = readAnnoValue(anno, "clazz");
    if (typeElemName == null) {
      typeElemName = ((TypeElement) orig).getQualifiedName().toString();
    }
    var typeElem = eu.getTypeElement(typeElemName);
    if (typeElem == null) {
      processingEnv.getMessager().printMessage(Kind.ERROR, "Cannot find type for " + typeElemName);
      return false;
    }

    if (tu.isSubtype(typeElem.asType(), PersistanceRaw)) {
      registerPersistablesClass(typeElem, anno);
      return true;
    }
    var canInline = !"false".equals(readAnnoValue(anno, "allowInlining"));
    var shallInline = "true".equals(readAnnoValue(anno, "allowInlining"));
    var richerConstructor =
        new Comparator<Object>() {
          @Override
          public int compare(Object a, Object b) {
            var ea = (ExecutableElement) a;
            var eb = (ExecutableElement) b;

            var diff = eb.getParameters().size() - ea.getParameters().size();
            if (diff == 0) {
              diff = countInlineRef(eb.getParameters()) - countInlineRef(ea.getParameters());
            }
            return diff;
          }
        };
    var constructors =
        typeElem.getEnclosedElements().stream()
            .filter(e -> isVisibleFrom(e, orig) && e.getKind() == ElementKind.CONSTRUCTOR)
            .sorted(richerConstructor)
            .collect(Collectors.toList());

    ExecutableElement cons;
    List<Element> singletonFields;
    if (constructors.isEmpty()) {
      singletonFields =
          typeElem.getEnclosedElements().stream()
              .filter(
                  e ->
                      e.getKind().isField()
                          && e.getModifiers().contains(Modifier.STATIC)
                          && isVisibleFrom(e, orig))
              .filter(e -> tu.isSameType(e.asType(), typeElem.asType()))
              .collect(Collectors.toList());
      if (singletonFields.isEmpty()) {
        processingEnv
            .getMessager()
            .printMessage(
                Kind.ERROR, "There should be exactly one constructor in " + typeElem, orig);
        return false;
      }
      cons = null;
    } else {
      cons = (ExecutableElement) constructors.get(0);
      singletonFields = null;
      if (constructors.size() > 1) {
        var snd = (ExecutableElement) constructors.get(1);
        if (richerConstructor.compare(cons, snd) == 0) {
          var sb = new StringBuilder();
          sb.append("There should be exactly one 'richest' constructor in ")
              .append(typeElem)
              .append(". Found:");
          for (var c : constructors) {
            sb.append("\n  ").append(c);
          }
          processingEnv.getMessager().printMessage(Kind.ERROR, sb.toString(), orig);
          return false;
        }
      }
    }
    var pkgName = eu.getPackageOf(orig).getQualifiedName().toString();
    var className = "Persist" + findNameInPackage(typeElem).replace(".", "_");
    var fo = processingEnv.getFiler().createSourceFile(pkgName + "." + className, orig);
    var ok = true;
    try (var w = fo.openWriter()) {
      var id = readAnnoValue(anno, "id");
      registerPersistablesClass(pkgName, className, Integer.parseInt(id));

      w.append("package ").append(pkgName).append(";\n");
      w.append("import java.io.IOException;\n");
      w.append("import org.enso.persist.Persistance;\n");
      w.append("public final class ")
          .append(className)
          .append(" extends Persistance<")
          .append(typeElemName)
          .append("> {\n");
      w.append("  public ").append(className).append("() {\n");
      w.append("    super(")
          .append(typeElemName)
          .append(".class, false, ")
          .append(id)
          .append(");\n");
      w.append("  }\n");
      w.append("  @SuppressWarnings(\"unchecked\")\n");
      w.append("  @Override\n");
      w.append("  protected ")
          .append(typeElemName)
          .append(" readObject(Input in) throws IOException {\n");

      if (cons != null) {
        for (var v : cons.getParameters()) {
          if (tu.isSameType(eu.getTypeElement("java.lang.String").asType(), v.asType())) {
            w.append("    var ").append(v.getSimpleName()).append(" = in.readUTF();\n");
          } else if (!v.asType().getKind().isPrimitive()) {
            var elem = findTypeOrNull(tu, v, orig);
            if (elem == null) {
              ok = false;
              continue;
            }
            var name = findFqn(elem);
            if (canInline && shouldInline(elem, shallInline)) {
              w.append("    var ")
                  .append(v.getSimpleName())
                  .append(" = in.readInline(")
                  .append(name)
                  .append(".class);\n");
            } else {
              w.append("    var ")
                  .append(v.getSimpleName())
                  .append(" = (")
                  .append(name)
                  .append(") in.readObject();\n");
            }
          } else
            switch (v.asType().getKind()) {
              case BOOLEAN ->
                  w.append("    var ").append(v.getSimpleName()).append(" = in.readBoolean();\n");
              case BYTE ->
                  w.append("    var ").append(v.getSimpleName()).append(" = in.readByte();\n");
              case INT ->
                  w.append("    var ").append(v.getSimpleName()).append(" = in.readInt();\n");
              case LONG ->
                  w.append("    var ").append(v.getSimpleName()).append(" = in.readLong();\n");
              default ->
                  processingEnv
                      .getMessager()
                      .printMessage(
                          Kind.ERROR, "Unsupported primitive type: " + v.asType().getKind());
            }
        }
        w.append("    return new ").append(typeElemName).append("(\n");
        w.append("      ");
        {
          var sep = "";
          for (var v : cons.getParameters()) {
            w.append(sep);
            w.append(v.getSimpleName());
            sep = ", ";
          }
        }
        w.append("\n");
        w.append("    );\n");
      } else {
        if (singletonFields.size() == 1) {
          var singleton = singletonFields.get(0);
          w.append("    return ")
              .append(typeElemName)
              .append(".")
              .append(singleton.getSimpleName())
              .append(";\n");
        } else {
          w.append("    return switch (in.readByte()) {\n");
          for (var i = 0; i < singletonFields.size(); i++) {
            var singleton = singletonFields.get(i);
            w.append(
                "      case "
                    + i
                    + " -> "
                    + typeElemName
                    + "."
                    + singleton.getSimpleName()
                    + ";\n");
          }
          w.append("      default -> throw new IOException();\n");
          w.append("    };\n");
        }
      }
      w.append("  }\n");
      w.append("  @SuppressWarnings(\"unchecked\")\n");
      w.append("  @Override\n");
      w.append("  protected void writeObject(")
          .append(typeElemName)
          .append(" obj, Output out) throws IOException {\n");
      if (cons != null) {
        for (var v : cons.getParameters()) {
          if (tu.isSameType(eu.getTypeElement("java.lang.String").asType(), v.asType())) {
            w.append("    out.writeUTF(obj.").append(v.getSimpleName()).append("());\n");
          } else if (!v.asType().getKind().isPrimitive()) {
            var elem = findTypeOrNull(tu, v, orig);
            if (elem == null) {
              ok = false;
              continue;
            }
            var name = findFqn(elem);
            if (canInline && shouldInline(elem, shallInline)) {
              w.append("    out.writeInline(")
                  .append(name)
                  .append(".class, obj.")
                  .append(v.getSimpleName())
                  .append("());\n");
            } else {
              w.append("    out.writeObject(obj.").append(v.getSimpleName()).append("());\n");
            }
          } else
            switch (v.asType().getKind()) {
              case BOOLEAN ->
                  w.append("    out.writeBoolean(obj.").append(v.getSimpleName()).append("());\n");
              case BYTE ->
                  w.append("    out.writeByte(obj.").append(v.getSimpleName()).append("());\n");
              case INT ->
                  w.append("    out.writeInt(obj.").append(v.getSimpleName()).append("());\n");
              case LONG ->
                  w.append("    out.writeLong(obj.").append(v.getSimpleName()).append("());\n");
              default ->
                  processingEnv
                      .getMessager()
                      .printMessage(
                          Kind.ERROR, "Unsupported primitive type: " + v.asType().getKind());
            }
        }
      } else {
        if (singletonFields.size() > 1) {
          w.append("    var index = -1;\n");
          for (var i = 0; i < singletonFields.size(); i++) {
            var singleton = singletonFields.get(i);
            w.append(
                "    if (obj == "
                    + typeElemName
                    + "."
                    + singleton.getSimpleName()
                    + ") index = "
                    + i
                    + ";\n");
          }
          w.append("    out.write(index);\n");
        }
      }
      w.append("  }\n");
      w.append("}\n");
    }
    return ok;
  }

  private TypeElement findTypeOrNull(Types tu, VariableElement v, Element orig) {
    var type = tu.erasure(v.asType());
    var elem = (TypeElement) tu.asElement(type);
    if (elem == null) {
      processingEnv
          .getMessager()
          .printMessage(Kind.ERROR, "No persistable class for " + type, orig);
    }
    return elem;
  }

  private void registerPersistablesClass(Element elem, AnnotationMirror anno) {
    StringBuilder name = new StringBuilder();
    for (; ; ) {
      if (elem instanceof PackageElement pkg) {
        var id = readAnnoValue(anno, "id");
        registerPersistablesClass(pkg.toString(), name.toString(), Integer.parseInt(id));
        break;
      } else {
        var sn = elem.getSimpleName().toString();
        if (!name.isEmpty()) {
          name.insert(0, '.');
        }
        name.insert(0, sn);
        elem = elem.getEnclosingElement();
      }
    }
  }

  private void registerPersistablesClass(String pkgName, String className, int id) {
    var pkg = registeredClasses.get(pkgName);
    if (pkg == null) {
      pkg = new TreeMap<>();
      registeredClasses.put(pkgName, pkg);
    }
    var prev = pkg.put(id, className);
    if (prev != null) {
      processingEnv
          .getMessager()
          .printMessage(
              Kind.ERROR,
              "Duplicated registration with id=" + id + " by " + className + " and " + prev);
    }
  }

  private boolean isVisibleFrom(Element e, Element from) {
    if (e.getModifiers().contains(Modifier.PUBLIC)) {
      return true;
    }
    if (e.getModifiers().contains(Modifier.PRIVATE)) {
      return false;
    }
    var eu = processingEnv.getElementUtils();
    return eu.getPackageOf(e) == eu.getPackageOf(from);
  }

  private int countInlineRef(List<? extends VariableElement> parameters) {
    var tu = processingEnv.getTypeUtils();
    var cnt = 0;
    for (var p : parameters) {
      var type = tu.asElement(tu.erasure(p.asType()));
      if (type != null) {
        switch (type.getSimpleName().toString()) {
          case "Reference" -> cnt++;
          case "Option" -> cnt++;
          default -> {}
        }
      }
    }
    return cnt;
  }

  private boolean shouldInline(TypeElement elem, boolean shallInline) {
    var inline =
        switch (findFqn(elem)) {
              case "scala.collection.immutable.Seq" -> true;
              default -> shallInline;
            }
            || !elem.getKind().isInterface();
    return inline;
  }

  private AnnotationMirror getAnnotation(Element elem, TypeElement annoType) {
    var tu = processingEnv.getTypeUtils();
    for (var m : elem.getAnnotationMirrors()) {
      var realType = m.getAnnotationType();
      if (tu.isSameType(realType, annoType.asType())) {
        return m;
      }
    }
    return null;
  }

  private String readAnnoValue(AnnotationMirror mirror, String name) {
    for (var entry : mirror.getElementValues().entrySet()) {
      if (name.equals(entry.getKey().getSimpleName().toString())) {
        return entry
            .getValue()
            .accept(
                new SimpleAnnotationValueVisitor9<String, Object>() {
                  @Override
                  public String visitBoolean(boolean b, Object p) {
                    return Boolean.toString(b);
                  }

                  @Override
                  public String visitInt(int i, Object p) {
                    return Integer.toString(i);
                  }

                  @Override
                  public String visitType(TypeMirror t, Object p) {
                    var e = (TypeElement) processingEnv.getTypeUtils().asElement(t);
                    return e.getQualifiedName().toString();
                  }
                },
                null);
      }
    }
    return null;
  }

  private List<AnnotationMirror> readAnnoArray(AnnotationMirror mirror, String name) {
    for (var entry : mirror.getElementValues().entrySet()) {
      if (name.equals(entry.getKey().getSimpleName().toString())) {
        return entry
            .getValue()
            .accept(
                new SimpleAnnotationValueVisitor9<
                    List<AnnotationMirror>, List<AnnotationMirror>>() {
                  @Override
                  public List<AnnotationMirror> visitArray(
                      List<? extends AnnotationValue> vals, List<AnnotationMirror> p) {
                    for (var v : vals) {
                      v.accept(this, p);
                    }
                    return p;
                  }

                  @Override
                  public List<AnnotationMirror> visitAnnotation(
                      AnnotationMirror a, List<AnnotationMirror> p) {
                    p.add(a);
                    return p;
                  }
                },
                new ArrayList<>());
      }
    }
    throw new IllegalArgumentException();
  }
}
