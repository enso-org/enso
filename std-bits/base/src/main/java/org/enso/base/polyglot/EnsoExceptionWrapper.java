package org.enso.base.polyglot;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.AccessDeniedException;
import java.nio.file.DirectoryNotEmptyException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.FileSystemException;
import java.nio.file.NoSuchFileException;
import java.nio.file.NotDirectoryException;
import java.util.Optional;
import org.graalvm.polyglot.Value;

/** A utility class to convert Java exceptions to Enso errors. */
public class EnsoExceptionWrapper {
  private EnsoExceptionWrapper() {}

  /**
   * Wraps common exceptions into their Enso counterparts.
   *
   * @param e the exception to wrap.
   * @return the wrapped exception or null if not supported by this method.
   */
  public static Optional<Value> wrapCommonExceptions(Exception e) {
    var result =
        switch (e) {
          case IllegalArgumentException argException ->
              EnsoMeta.makeInstance(
                  "Standard.Base.Errors.Illegal_Argument",
                  "Illegal_Argument",
                  "Error",
                  argException.getMessage(),
                  argException);
          case IllegalStateException stateException ->
              EnsoMeta.makeInstance(
                  "Standard.Base.Errors.Illegal_State",
                  "Illegal_State",
                  "Error",
                  stateException.getMessage(),
                  stateException);
          default -> null;
        };
    return Optional.ofNullable(result);
  }

  private static Value makeEnsoFile(String path) {
    var fileType = EnsoMeta.getType("Standard.Base.System.File", "File");
    return fileType.invokeMember("new", path);
  }

  /**
   * Wraps file-related exceptions into their Enso counterparts.
   *
   * @param path the file path related to the exception.
   * @param e the exception to wrap.
   * @return the wrapped exception.
   */
  public static Optional<Value> wrapFileExceptions(String path, Exception e) {
    var associatedPath = e instanceof FileSystemException fsEx ? fsEx.getFile() : path;
    if (associatedPath == null) {
      // Convert to an IO error if no path is available.
      if (e instanceof IOException || e instanceof UncheckedIOException) {
        return Optional.of(
            EnsoMeta.makeInstance(
                "Standard.Base.Errors.File_Error",
                "File_Error",
                "IO_Error",
                null,
                "An IO error has occurred: " + e));
      }

      return Optional.empty();
    }

    var result =
        switch (e) {
          case NoSuchFileException _, FileNotFoundException _ ->
              EnsoMeta.makeInstance(
                  "Standard.Base.Errors.File_Error",
                  "File_Error",
                  "Not_Found",
                  makeEnsoFile(associatedPath));
          case FileAlreadyExistsException _ ->
              EnsoMeta.makeInstance(
                  "Standard.Base.Errors.File_Error",
                  "File_Error",
                  "Already_Exists",
                  makeEnsoFile(associatedPath));
          case AccessDeniedException _ ->
              EnsoMeta.makeInstance(
                  "Standard.Base.Errors.File_Error",
                  "File_Error",
                  "Access_Denied",
                  makeEnsoFile(associatedPath));
          case DirectoryNotEmptyException _ ->
              EnsoMeta.makeInstance(
                  "Standard.Base.Errors.File_Error",
                  "File_Error",
                  "Directory_Not_Empty",
                  makeEnsoFile(associatedPath));
          case NotDirectoryException _ ->
              EnsoMeta.makeInstance(
                  "Standard.Base.Errors.File_Error",
                  "File_Error",
                  "Not_A_Directory",
                  makeEnsoFile(associatedPath));
          case IOException _, UncheckedIOException _ ->
              EnsoMeta.makeInstance(
                  "Standard.Base.Errors.File_Error",
                  "File_Error",
                  "IO_Error",
                  makeEnsoFile(associatedPath),
                  e.getMessage());
          default -> null;
        };
    return Optional.ofNullable(result);
  }
}
