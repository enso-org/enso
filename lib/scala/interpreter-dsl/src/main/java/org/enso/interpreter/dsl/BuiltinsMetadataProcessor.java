package org.enso.interpreter.dsl;

import org.enso.interpreter.dsl.model.MethodDefinition;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.Filer;
import javax.annotation.processing.RoundEnvironment;
import javax.lang.model.element.TypeElement;
import javax.tools.FileObject;
import javax.tools.StandardLocation;
import java.io.IOException;
import java.io.Writer;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public abstract class BuiltinsMetadataProcessor extends AbstractProcessor {
    private final Map<Filer, Map<String, String>> builtinMethods = new HashMap<>();

    @Override
    public final boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        if (roundEnv.errorRaised()) {
            return false;
        }
        if (roundEnv.processingOver()) {
            try {
                storeBuiltinMetadata(MethodDefinition.META_PATH);
            } catch (IOException e) {
                e.printStackTrace();
            }
            builtinMethods.clear();
            return true;
        } else {
            return handleProcess(annotations, roundEnv);
        }
    }

    /**
     * The regular body of {@link #process}.
     * Called during regular rounds if there are no outstanding errors.
     * In the last round, one of the processors will dump all builtin metadata
     * @param annotations as in {@link #process}
     * @param roundEnv as in {@link #process}
     * @return as in {@link #process}
     */
    protected abstract boolean handleProcess(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv);

    private void storeBuiltinMetadata(String path) throws IOException {
        FileObject res = processingEnv.getFiler().createResource(
                StandardLocation.CLASS_OUTPUT, "",
                path
        );
        Writer writer = res.openWriter();
        try {
            for (Filer f: builtinMethods.keySet()) {
                for (Map.Entry<String, String> entry : builtinMethods.get(f).entrySet()) {
                    writer.append(entry.getKey() + ":" + entry.getValue() + "\n");
                }
            }
        } finally {
            writer.close();
        }
    }
    protected void registerBuiltinMethod(Filer f, String name, String clazzName) {
        Map<String, String> methods = builtinMethods.get(f);
        if (methods == null) {
            methods = new HashMap<>();
            builtinMethods.put(f, methods);
        }
        methods.put(name, clazzName);
    }
}
