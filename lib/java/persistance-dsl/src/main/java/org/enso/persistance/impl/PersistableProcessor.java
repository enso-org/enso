package org.enso.persistance.impl;

import java.io.IOException;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.Processor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.lang.model.element.ExecutableElement;
import javax.lang.model.element.Modifier;
import javax.lang.model.element.PackageElement;
import javax.lang.model.element.TypeElement;
import javax.lang.model.element.VariableElement;
import javax.lang.model.type.MirroredTypeException;
import javax.tools.Diagnostic.Kind;

import org.enso.persistance.Persistable;

import org.openide.util.lookup.ServiceProvider;

@SupportedAnnotationTypes({
  "org.enso.persistance.Persistable",
  "org.enso.persistance.Persistable.Group"
})
@ServiceProvider(service=Processor.class)
public class PersistableProcessor extends AbstractProcessor {
  @Override
   public SourceVersion getSupportedSourceVersion() {
    return SourceVersion.latest();
  }

  @Override
  public boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    var ok = true;
    try {
      for (var elem : roundEnv.getElementsAnnotatedWith(Persistable.class)) {
        var anno = elem.getAnnotation(Persistable.class);
        ok &= generatePersistance(elem, anno);
      }
      for (var elem : roundEnv.getElementsAnnotatedWith(Persistable.Group.class)) {
        for (var anno : elem.getAnnotation(Persistable.Group.class).value()) {
          ok &= generatePersistance(elem, anno);
        }
      }
    } catch (IOException e) {
      ok = false;
      e.printStackTrace();
      processingEnv.getMessager().printMessage(Kind.ERROR, e.getMessage());
    }
    return ok;
  }

  private String findFqn(Element e) {
    var inPackage = findNameInPackage(e);
    var pkg = processingEnv.getElementUtils().getPackageOf(e);
    return pkg.getQualifiedName() + "." + inPackage;
  }

  private static String findNameInPackage(Element e) {
    var sb = new StringBuilder();
    while (e != null && !(e instanceof PackageElement)) {
      if (!sb.isEmpty()) {
        sb.insert(0, ".");
      }
      sb.insert(0, e.getSimpleName());
      e = e.getEnclosingElement();
    }
    return sb.toString();
  }

  private boolean generatePersistance(Element orig, Persistable anno) throws IOException {
    var eu = processingEnv.getElementUtils();
    var tu = processingEnv.getTypeUtils();
    String typeElemName;
    try {
      typeElemName = anno.clazz().getName();
    } catch (MirroredTypeException e) {
      typeElemName = e.getTypeMirror().toString();
    }
    var typeElem = eu.getTypeElement(typeElemName);
    if (typeElem == null) {
      processingEnv.getMessager().printMessage(Kind.ERROR, "Cannot find type for " + typeElemName);
      return false;
    }
    var richerConstructor = new Comparator<Object>() {
      @Override
      public int compare(Object a, Object b) {
        var ea = (ExecutableElement)a;
        var eb = (ExecutableElement)b;

        var diff = eb.getParameters().size() - ea.getParameters().size();
        if (diff == 0) {
          diff = countSeq(eb.getParameters()) - countSeq(ea.getParameters());
        }
        return diff;
      }
    };
    var constructors = typeElem.getEnclosedElements().stream()
      .filter(
        e -> e.getModifiers().contains(Modifier.PUBLIC) && e.getKind() == ElementKind.CONSTRUCTOR
      )
      .sorted(richerConstructor)
      .toList();
    if (constructors.isEmpty()) {
      processingEnv.getMessager().printMessage(Kind.ERROR, "There should be exactly one constructor in " + typeElem, orig);
      return false;
    }
    var cons = (ExecutableElement) constructors.get(0);
    if (constructors.size() > 1) {
      var snd = (ExecutableElement) constructors.get(1);
      if (richerConstructor.compare(cons, snd) == 0) {
        processingEnv.getMessager().printMessage(Kind.ERROR, "There should be exactly one 'richest' constructor in " + typeElem, orig);
        return false;
      }
    }
    var pkgName = eu.getPackageOf(orig).getQualifiedName().toString();
    var className = "Persist" + findNameInPackage(typeElem).replace(".", "_");
    var fo = processingEnv.getFiler().createSourceFile(pkgName + "." + className, orig);
    try (var w = fo.openWriter()) {
      w.append("package ").append(pkgName).append(";\n");
      w.append("import java.io.IOException;\n");
      w.append("import org.enso.persistance.Persistance;\n");
      w.append("@org.openide.util.lookup.ServiceProvider(service=Persistance.class)\n");
      w.append("public final class ").append(className).append(" extends Persistance<").append(typeElemName).append("> {\n");
      w.append("  public ").append(className).append("() {\n");
      w.append("    super(").append(typeElemName).append(".class, false, ").append(Integer.toString(anno.id())).append(");\n");
      w.append("  }\n");
      w.append("  @SuppressWarnings(\"unchecked\")\n");
      w.append("  protected ").append(typeElemName).append(" readObject(Input in) throws IOException {\n");

      for (var v : cons.getParameters()) {
        if (tu.isSameType(eu.getTypeElement("java.lang.String").asType(), v.asType())) {
          w.append("    var ").append(v.getSimpleName()).append(" = in.readUTF();\n");
        } else if (!v.asType().getKind().isPrimitive()) {
          var type = tu.erasure(v.asType());
          var elem = (TypeElement) tu.asElement(type);
          var name = findFqn(elem);
          if (shouldInline(elem)) {
            w.append("    var ").append(v.getSimpleName()).append(" = in.readInline(").append(name).append(".class);\n");
          } else {
            w.append("    var ").append(v.getSimpleName()).append(" = (").append(name).append(") in.readObject();\n");
          }
        } else switch (v.asType().getKind()) {
          case BOOLEAN ->
            w.append("    var ").append(v.getSimpleName()).append(" = in.readBoolean();\n");
          case INT ->
            w.append("    var ").append(v.getSimpleName()).append(" = in.readInt();\n");
          default ->
            processingEnv.getMessager().printMessage(Kind.ERROR, "Unsupported primitive type: " + v.asType().getKind());
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
      w.append("  }\n");
      w.append("  @SuppressWarnings(\"unchecked\")\n");
      w.append("  protected void writeObject(").append(typeElemName).append(" obj, Output out) throws IOException {\n");

      for (var v : cons.getParameters()) {
        if (tu.isSameType(eu.getTypeElement("java.lang.String").asType(), v.asType())) {
          w.append("    out.writeUTF(obj.").append(v.getSimpleName()).append("());\n");
        } else if (!v.asType().getKind().isPrimitive()) {
          var type = tu.erasure(v.asType());
          var elem = (TypeElement) tu.asElement(type);
          var name = findFqn(elem);
          if (shouldInline(elem)) {
            w.append("    out.writeInline(").append(name).append(".class, obj.").append(v.getSimpleName()).append("());\n");
          } else {
            w.append("    out.writeObject(obj.").append(v.getSimpleName()).append("());\n");
          }
        } else switch (v.asType().getKind()) {
          case BOOLEAN ->
            w.append("    out.writeBoolean(obj.").append(v.getSimpleName()).append("());\n");
          case INT ->
            w.append("    out.writeInt(obj.").append(v.getSimpleName()).append("());\n");
          default ->
            processingEnv.getMessager().printMessage(Kind.ERROR, "Unsupported primitive type: " + v.asType().getKind());
        }
      }
      w.append("  }\n");
      w.append("}\n");
    }
    return true;
  }

  private int countSeq(List<? extends VariableElement> parameters) {
    var tu = processingEnv.getTypeUtils();
    var cnt = 0;
    for (var p : parameters) {
      var type = tu.asElement(tu.erasure(p.asType()));
      if (type != null && type.getSimpleName().toString().equals("Seq")) {
        cnt++;
      }
    }
    return cnt;
  }

  private  boolean shouldInline(TypeElement elem) {
    var inline = switch (findFqn(elem)) {
      case "scala.collection.immutable.Seq" -> true;
      default -> false;
    } || !elem.getKind().isInterface();
    return inline;
  }
}
