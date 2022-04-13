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

        BuiltinTypeConstr(String tpeName) {
            this.tpeName = tpeName;
            this.paramNames = "";
        }

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
    protected void storeMetadata(Writer writer) throws IOException {
        for (Filer f: builtinTypes.keySet()) {
            for (Map.Entry<String, BuiltinTypeConstr> entry : builtinTypes.get(f).entrySet()) {
                BuiltinTypeConstr constr = entry.getValue();
                writer.append(entry.getKey() + ":" + constr.getTpeName() + ":" + constr.getParamNames() + "\n");
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
                registerBuiltinType(processingEnv.getFiler(), clazzName, pkgName + "." + clazzName, builtinTypeAnnotation.params());
            }
        }
        return true;
    }

    protected void registerBuiltinType(Filer f, String name, String clazzName, String params) {
        Map<String, BuiltinTypeConstr> methods = builtinTypes.get(f);
        if (methods == null) {
            methods = new HashMap<>();
            builtinTypes.put(f, methods);
        }
        methods.put(name, new BuiltinTypeConstr(clazzName, params));
    }
}
