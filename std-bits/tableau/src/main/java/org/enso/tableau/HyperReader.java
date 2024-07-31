package org.enso.tableau;

import com.tableau.hyperapi.*;

import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.channels.Channels;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

public class HyperReader {
  public static final Path HYPER_PATH = Path.of(getHyperPath());
  private static HyperProcess process;

  private static final Logger LOGGER = Logger.getLogger("enso-hyper-reader");

  private static String getHyperPath() {
    if (System.getenv("HYPER_PATH") != null) {
      return System.getenv("HYPER_PATH");
    }
    if (System.getenv("ENSO_DATA_DIRECTORY") != null) {
      return System.getenv("ENSO_DATA_DIRECTORY") + "/hyper";
    } else {
      return switch (OSPlatform.CurrentPlatform) {
        case WINDOWS -> System.getenv("LocalAppData") + "/enso/hyper";
        case MAC_ARM64, MAX_X64 -> System.getProperty("user.home")
            + "/Library/Application Support/org.enso/hyper";
        case LINUX, OTHER -> System.getProperty("user.home") + "/.local/share/enso/hyper";
      };
    }
  }

  private static HyperProcess getProcess() throws IOException {
    // Check if the hyper directory exists, if not create it.
    if (!Files.exists(HYPER_PATH)) {
      try {
        Files.createDirectories(HYPER_PATH);
      } catch (Exception e) {
        throw new IOException("Failed to create Hyper directory: " + HYPER_PATH, e);
      }
    }

    // Check if any files in the hyper directory, otherwise download them.
    try (var files = Files.list(HYPER_PATH)) {
      if (files.findAny().isEmpty()) {
        switch (OSPlatform.CurrentPlatform) {
          case WINDOWS -> downloadHyper(
              "https://enso-data-samples.s3.us-west-1.amazonaws.com/tableau/hyperd.exe",
              "hyperd.exe",
              false);
          case MAC_ARM64 -> downloadHyper(
              "https://enso-data-samples.s3.us-west-1.amazonaws.com/tableau/macos-arm64/hyperd",
              "hyperd",
              true);
          case MAX_X64 -> throw new IOException(
              "Unsupported platform: Only ARM64 Mac is supported.");
          case LINUX -> downloadHyper(
              "https://enso-data-samples.s3.us-west-1.amazonaws.com/tableau/linux/hyperd",
              "hyperd",
              true);
          case OTHER -> throw new IOException(
              "Unsupported platform: " + OSPlatform.CurrentPlatform);
        }
        ;
      }
    } catch (Exception e) {
      throw new IOException("Failed to download hyperd.", e);
    }

    // Start hyper process.
    if (process == null || !process.isOpen()) {
      var contextClassLoader = Thread.currentThread().getContextClassLoader();
      try {
        Thread.currentThread().setContextClassLoader(HyperReader.class.getClassLoader());
        LOGGER.log(Level.INFO, "Starting Hyper process: " + HYPER_PATH + ".");
        try {
          process = new HyperProcess(HYPER_PATH, Telemetry.DO_NOT_SEND_USAGE_DATA_TO_TABLEAU);
        } catch (Throwable ioe) {
          ioe.printStackTrace();
          throw ioe;
        }
      } finally {
        Thread.currentThread().setContextClassLoader(contextClassLoader);
      }
    }

    return process;
  }

  private static void downloadHyper(String uri, String fileName, boolean setExecutable)
      throws IOException, URISyntaxException {
    LOGGER.log(Level.INFO, "Downloading Hyper from: " + uri);
    var hyperdFile = HYPER_PATH.resolve(fileName).toFile();
    var url = new URI(uri);
    var readChannel = Channels.newChannel(url.toURL().openStream());
    try (var fos = new FileOutputStream(hyperdFile)) {
      var writeChannel = fos.getChannel();
      writeChannel.transferFrom(readChannel, 0, Long.MAX_VALUE);
    }
    if (setExecutable) {
      hyperdFile.setExecutable(true);
    }
  }

  public static String[] readSchemas(String path) throws IOException {
    var process = getProcess();
    try (var connection = new Connection(process.getEndpoint(), path)) {
      var catalog = connection.getCatalog();
      return catalog.getSchemaNames().stream()
          .map(s -> s.getName().getUnescaped())
          .toArray(String[]::new);
    }
  }

  public static String addSchema(String path, String schema) throws IOException {
    var schemaName = new SchemaName(schema);
    var process = getProcess();

    try (var connection = new Connection(process.getEndpoint(), path)) {
      var catalog = connection.getCatalog();
      catalog.createSchema(schemaName);
      return schemaName.getName().getUnescaped();
    }
  }

  public static String[] listTablesAllSchemas(String path) throws IOException {
    var process = getProcess();
    try (var connection = new Connection(process.getEndpoint(), path)) {
      var catalog = connection.getCatalog();
      return listTablesImpl(catalog, catalog.getSchemaNames());
    }
  }

  public static String[] listTables(String path, String schemaName) throws IOException{
    var schemaNames = List.of(new SchemaName(schemaName));
    var process = getProcess();
    try (var connection = new Connection(process.getEndpoint(), path)) {
      var catalog = connection.getCatalog();
      return listTablesImpl(catalog, schemaNames);
    }
  }

  private static String[] listTablesImpl(Catalog catalog, List<SchemaName> schemaNames) {
    var output = new ArrayList<String>();
    for (var schemaName : schemaNames) {
      var tables = catalog.getTableNames(schemaName);
      for (var table : tables) {
        output.add(table.getName().getUnescaped());
      }
    }
    return output.toArray(String[]::new);
  }
}
