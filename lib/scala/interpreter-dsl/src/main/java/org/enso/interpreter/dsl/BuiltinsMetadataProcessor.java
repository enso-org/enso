package org.enso.interpreter.dsl;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.lang.model.element.TypeElement;
import javax.tools.FileObject;
import javax.tools.StandardLocation;
import java.io.IOException;
import java.io.Writer;
import java.util.Set;

public abstract class BuiltinsMetadataProcessor extends AbstractProcessor {


    protected abstract String metadataPath();

    protected abstract void cleanup();

    protected abstract void storeMetadata(Writer writer) throws IOException;

    @Override
    public final boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
        if (roundEnv.errorRaised()) {
            return false;
        }
        if (roundEnv.processingOver()) {
            try {
                FileObject res = processingEnv.getFiler().createResource(
                        StandardLocation.CLASS_OUTPUT, "",
                        metadataPath()
                );
                Writer writer = res.openWriter();
                try {
                    storeMetadata(writer);
                } finally {
                    writer.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
            cleanup();
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

}
