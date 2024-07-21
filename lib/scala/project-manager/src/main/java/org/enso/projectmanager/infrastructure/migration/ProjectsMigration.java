package org.enso.projectmanager.infrastructure.migration;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.StandardCopyOption;
import java.nio.file.attribute.PosixFilePermission;
import java.util.HashSet;
import java.util.Set;
import org.apache.commons.io.FileUtils;
import org.enso.desktopenvironment.Platform;
import org.enso.projectmanager.boot.configuration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class ProjectsMigration {

  private static final Logger logger = LoggerFactory.getLogger(ProjectsMigration.class);

  private ProjectsMigration() {}

  public static void migrate(configuration.StorageConfig storageConfig) {
    var oldProjectsPath =
        Platform.getOperatingSystem()
            .getDirectories()
            .getUserHome()
            .resolve("enso")
            .resolve("projects")
            .toFile();
    if (oldProjectsPath.isDirectory()) {
      try {
        File newProjectsPath = storageConfig.userProjectsPath();
        migrateProjectsDirectory(oldProjectsPath, newProjectsPath);
      } catch (IOException e) {
        logger.error("Migration aborted. Failed to get user documents directory.", e);
      }
    }
  }

  static void migrateProjectsDirectory(File oldProjectsPath, File newProjectsPath) {
    if (oldProjectsPath.isDirectory()) {
      logger.info(
          "Running projects migration from '{}' to '{}'.", oldProjectsPath, newProjectsPath);
      if (newProjectsPath.isDirectory()) {
        try {
          logger.info(
              "Both '{}' and '{}' project directories exist. Cleaning up.",
              oldProjectsPath,
              newProjectsPath);
          FileUtils.deleteDirectory(oldProjectsPath);
          return;
        } catch (IOException e) {
          logger.error(
              "Both '{}' and '{}' project directories exist. Failed to clean up.",
              oldProjectsPath,
              newProjectsPath,
              e);
          return;
        }
      }

      try {
        logger.info(
            "Moving projects directory from '{}' to '{}'.", oldProjectsPath, newProjectsPath);
        moveDirectory(oldProjectsPath, newProjectsPath);
      } catch (IOException ex) {
        logger.error("Migration aborted. Failed to copy user projects directory.", ex);
        return;
      }

      if (!Platform.getOperatingSystem().isWindows()) {
        try {
          logger.info("Setting projects directory permissions '{}'.", newProjectsPath);
          setProjectsDirectoryPermissions(newProjectsPath);
        } catch (IOException ex) {
          logger.error(
              "Failed to set permissions on projects directory '{}'.", newProjectsPath, ex);
          return;
        }
      }

      logger.info("Projects migration successful.");
    }
  }

  /**
   * Moves the source directory to the destination directory by renaming the source directory. If
   * renaming is not supported by the file system or the destination directory is located on a
   * different file system, tries to copy the source directory and then delete the source.
   *
   * @param source the source path
   * @param destination the destination path
   * @throws IOException if the moving was unsuccessful
   */
  static void moveDirectory(File source, File destination) throws IOException {
    try {
      Files.move(source.toPath(), destination.toPath(), StandardCopyOption.ATOMIC_MOVE);
    } catch (IOException e) {
      try {
        FileUtils.copyDirectory(source, destination);
      } catch (IOException ex) {
        FileUtils.deleteQuietly(destination);
        throw ex;
      }
      FileUtils.deleteDirectory(source);
    }
  }

  /**
   * Sets the projects directory permissions to {@code rwx------}.
   *
   * @param path the directory path.
   * @throws IOException if the action is unsuccessful.
   */
  static void setProjectsDirectoryPermissions(File path) throws IOException {
    Set<PosixFilePermission> permissions = new HashSet<>();
    permissions.add(PosixFilePermission.OWNER_READ);
    permissions.add(PosixFilePermission.OWNER_WRITE);
    permissions.add(PosixFilePermission.OWNER_EXECUTE);

    Files.setPosixFilePermissions(path.toPath(), permissions);
  }
}
