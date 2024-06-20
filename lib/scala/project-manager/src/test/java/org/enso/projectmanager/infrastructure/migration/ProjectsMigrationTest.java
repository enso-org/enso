package org.enso.projectmanager.infrastructure.migration;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.attribute.PosixFilePermissions;
import org.apache.commons.io.FileUtils;
import org.enso.desktopenvironment.Platform;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class ProjectsMigrationTest {

  @Rule public TemporaryFolder tmp = new TemporaryFolder();

  @Test
  public void moveDirectory() throws IOException {
    File oldProjectsDir = tmp.newFolder("old-projects");
    File newProjectsDir = new File(tmp.getRoot(), "new-projects");

    File project1 = createProjectStructure(oldProjectsDir, "Project1");
    File project2 = createProjectStructure(oldProjectsDir, "Project2");

    Assert.assertTrue(oldProjectsDir.isDirectory());
    Assert.assertTrue(project1.isDirectory());
    Assert.assertTrue(project2.isDirectory());
    Assert.assertFalse(newProjectsDir.isDirectory());

    ProjectsMigration.moveDirectory(oldProjectsDir, newProjectsDir);

    Assert.assertFalse(oldProjectsDir.isDirectory());
    Assert.assertTrue(newProjectsDir.isDirectory());
    Assert.assertTrue(new File(newProjectsDir, "Project1").isDirectory());
    Assert.assertTrue(new File(newProjectsDir, "Project2").isDirectory());
  }

  @Test
  public void setProjectDirectoryPermissions() throws IOException {
    if (!Platform.isWindows()) {
      File projectsDir = tmp.newFolder("projects");
      createProjectStructure(projectsDir, "Project1");

      Assert.assertTrue(projectsDir.isDirectory());

      ProjectsMigration.setProjectsDirectoryPermissions(projectsDir);

      var permissions = Files.getPosixFilePermissions(projectsDir.toPath());
      Assert.assertEquals("rwx------", PosixFilePermissions.toString(permissions));
    }
  }

  @Test
  public void migrateProjectsDirectoryIdempotent() throws IOException {
    File oldProjectsDir = tmp.newFolder("old-projects");
    File newProjectsDir = new File(tmp.getRoot(), "new-projects");

    File project1 = createProjectStructure(oldProjectsDir, "Project1");
    File project2 = createProjectStructure(oldProjectsDir, "Project2");

    Assert.assertTrue(oldProjectsDir.isDirectory());
    Assert.assertTrue(project1.isDirectory());
    Assert.assertTrue(project2.isDirectory());
    Assert.assertFalse(newProjectsDir.isDirectory());

    ProjectsMigration.migrateProjectsDirectory(oldProjectsDir, newProjectsDir);

    File newProject1 = new File(newProjectsDir, "Project1");
    File newProject2 = new File(newProjectsDir, "Project2");
    Assert.assertFalse(oldProjectsDir.isDirectory());
    Assert.assertTrue(newProjectsDir.isDirectory());
    Assert.assertTrue(newProject1.isDirectory());
    Assert.assertTrue(newProject2.isDirectory());

    ProjectsMigration.migrateProjectsDirectory(oldProjectsDir, newProjectsDir);

    Assert.assertFalse(oldProjectsDir.isDirectory());
    Assert.assertTrue(newProjectsDir.isDirectory());
    Assert.assertTrue(newProject1.isDirectory());
    Assert.assertTrue(newProject2.isDirectory());
  }

  @Test
  public void migrateProjectsDirectoryCleanupWhenBothExist() throws IOException {
    File oldProjectsDir = tmp.newFolder("old-projects");
    File newProjectsDir = new File(tmp.getRoot(), "new-projects");

    File project1 = createProjectStructure(newProjectsDir, "Project1");
    File project2 = createProjectStructure(newProjectsDir, "Project2");

    Assert.assertTrue(oldProjectsDir.isDirectory());
    Assert.assertTrue(newProjectsDir.isDirectory());
    Assert.assertTrue(project1.isDirectory());
    Assert.assertTrue(project2.isDirectory());

    ProjectsMigration.moveDirectory(oldProjectsDir, newProjectsDir);

    Assert.assertFalse(oldProjectsDir.isDirectory());
    Assert.assertTrue(newProjectsDir.isDirectory());
    Assert.assertTrue(new File(newProjectsDir, "Project1").isDirectory());
    Assert.assertTrue(new File(newProjectsDir, "Project2").isDirectory());
  }

  private static File createProjectStructure(File tmp, String name) throws IOException {
    var projectDir = new File(tmp, name);
    var srcDir = new File(projectDir, "src");
    var ensoDir = new File(projectDir, ".enso");

    FileUtils.forceMkdir(srcDir);
    FileUtils.forceMkdir(ensoDir);

    createNewFile(new File(projectDir, "package.yaml"));
    createNewFile(new File(srcDir, "Main.enso"));

    return projectDir;
  }

  private static void createNewFile(File file) throws IOException {
    if (!file.createNewFile()) {
      throw new IOException("File '" + file + "' already exists.");
    }
  }
}
