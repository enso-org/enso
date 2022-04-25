package org.enso.interpreter.dsl;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;
import javax.tools.FileObject;
import javax.tools.StandardLocation;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.NoSuchFileException;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

public abstract class BuiltinsMetadataProcessor extends AbstractProcessor {

  protected abstract String metadataPath();

  protected abstract void cleanup();

  protected abstract void storeMetadata(Writer writer, Map<String, String> pastEntries)
      throws IOException;

  @Override
  public final boolean process(Set<? extends TypeElement> annotations, RoundEnvironment roundEnv) {
    if (roundEnv.errorRaised()) {
      return false;
    }
    if (roundEnv.processingOver()) {
      // A hack to improve support for separate compilation.
      // Since one cannot open the existing resource file in Append mode,
      // we read the whole file and provide to the function that does the update.
      // Deletes/renaming is still not gonna work but that would be the same case
      // if we were writing metadata information per source file anyway.
      Map<String, String> pastEntries;
      try {
        FileObject existingFile =
            processingEnv.getFiler().getResource(StandardLocation.CLASS_OUTPUT, "", metadataPath());
        try (InputStream resource = existingFile.openInputStream()) {
          pastEntries =
              new BufferedReader(new InputStreamReader(resource, StandardCharsets.UTF_8))
                  .lines()
                  .collect(Collectors.toMap(l -> l.split(":")[0], Function.identity()));
        }
      } catch (NoSuchFileException notFoundException) {
        pastEntries = new HashMap<>();
      } catch (Exception e) {
        e.printStackTrace();
        pastEntries = new HashMap<>();
      }
      try {
        FileObject res =
            processingEnv
                .getFiler()
                .createResource(StandardLocation.CLASS_OUTPUT, "", metadataPath());
        Writer writer = res.openWriter();
        try {
          storeMetadata(writer, pastEntries);
          // Separate compilation hack
          for (String value : pastEntries.values()) {
            writer.append(value + "\n");
          }
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
   * The regular body of {@link #process}. Called during regular rounds if there are no outstanding
   * errors. In the last round, one of the processors will dump all builtin metadata
   *
   * @param annotations as in {@link #process}
   * @param roundEnv as in {@link #process}
   * @return as in {@link #process}
   */
  protected abstract boolean handleProcess(
      Set<? extends TypeElement> annotations, RoundEnvironment roundEnv);
}
