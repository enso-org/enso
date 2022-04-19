package org.enso.interpreter.dsl;

import com.google.auto.service.AutoService;

import javax.annotation.processing.*;
import javax.lang.model.SourceVersion;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import java.io.IOException;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

@SupportedAnnotationTypes("org.enso.interpreter.dsl.BuiltinType")
@SupportedSourceVersion(SourceVersion.RELEASE_11)
@AutoService(Processor.class)
public class TypeProcessor extends BuiltinsMetadataProcessor {
    private final Map<Filer, Map<String, BuiltinTypeConstr>> builtinTypes = new HashMap<>();

    private class BuiltinTypeConstr {
        private String tpeName;
        private String paramNames;

        BuiltinTypeConstr(String tpeName, String params) {
            this.tpeName = tpeName;
            this.paramNames = params;
        }

        public String getTpeName() {
            return tpeName;
        }

        public String getParamNames() {
            return paramNames;
        }

    }


    public static final String NODE_PKG = "org.enso.interpreter.node.expression.builtin";
    public static final String META_PATH =
            "META-INF" + "/" + NODE_PKG.replace('.', '/') + "/BuiltinTypes.metadata";
    @Override
    protected String metadataPath() {
        return META_PATH;
    }

    @Override
    protected void cleanup() {
        builtinTypes.clear();
    }

    @Override
    protected void storeMetadata(Writer writer, Map<String, String> pastEntries) throws IOException {
        for (Filer f: builtinTypes.keySet()) {
            for (Map.Entry<String, BuiltinTypeConstr> entry : builtinTypes.get(f).entrySet()) {
                BuiltinTypeConstr constr = entry.getValue();
                writer.append(entry.getKey() + ":" + constr.getTpeName() + ":" + constr.getParamNames() + "\n");
                if (pastEntries.containsKey(entry.getKey())) {
                    pastEntries.remove(entry.getKey());
                }
            }
        }
    }

    @Override
    protected boolean handleProcess(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
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
                registerBuiltinType(processingEnv.getFiler(), ensoTypeName, pkgName + "." + clazzName, builtinTypeAnnotation.params());
            }
        }
        return true;
    }

    protected void registerBuiltinType(Filer f, String name, String clazzName, String params) {
        Map<String, BuiltinTypeConstr> classes = builtinTypes.get(f);
        if (classes == null) {
            classes = new HashMap<>();
            builtinTypes.put(f, classes);
        }
        classes.put(name, new BuiltinTypeConstr(clazzName, params));
    }
}
