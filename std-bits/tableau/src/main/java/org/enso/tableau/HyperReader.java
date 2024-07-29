package org.enso.tableau;

import com.tableau.hyperapi.Connection;
import com.tableau.hyperapi.HyperProcess;
import com.tableau.hyperapi.SchemaName;
import com.tableau.hyperapi.Telemetry;

import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.channels.Channels;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.logging.Level;
import java.util.logging.Logger;

public class HyperReader {
  public static final Path HYPER_PATH = Path.of(getHyperPath());
  private static HyperProcess process;

  private static final Logger LOGGER = Logger.getLogger("enso-hyper-reader");

  private static String getHyperPath() {
    if (System.getenv("HYPER_PATH") != null) {
      return System.getenv("HYPER_PATH");
    } if (System.getenv("ENSO_DATA_DIRECTORY") != null) {
      return System.getenv("ENSO_DATA_DIRECTORY") + "/hyper";
    } else {
      return switch (OSPlatform.CurrentPlatform) {
        case WINDOWS -> System.getenv("LocalAppData") + "/enso/hyper";
        case MAC_ARM64, MAX_X64 -> System.getProperty("user.home") + "/Library/Application Support/org.enso/hyper";
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
          case WINDOWS ->
            downloadHyper("https://enso-data-samples.s3.us-west-1.amazonaws.com/tableau/hyperd.exe", "hyperd.exe");
          case MAC_ARM64 ->
            downloadHyper("https://enso-data-samples.s3.us-west-1.amazonaws.com/tableau/macos-arm64/hyperd", "hyperd");
          case MAX_X64 ->
            throw new IOException("Unsupported platform: Only ARM64 Mac is supported.");
          case LINUX ->
            downloadHyper("https://enso-data-samples.s3.us-west-1.amazonaws.com/tableau/linux/hyperd", "hyperd");
          case OTHER ->
            throw new IOException("Unsupported platform: " + OSPlatform.CurrentPlatform);
        };
      }
    } catch (Exception e) {
      throw new IOException("Failed to download hyperd.", e);
    }

    // Start hyper process.
    if (process == null) {
      LOGGER.log(Level.INFO, "Starting Hyper process: " + HYPER_PATH.toString() + ".");
      process = new HyperProcess(HYPER_PATH, Telemetry.DO_NOT_SEND_USAGE_DATA_TO_TABLEAU);
    }
    return process;
  }

  private static void downloadHyper(String uri, String fileName) throws IOException, URISyntaxException {
    LOGGER.log(Level.INFO, "Downloading Hyper from: " + uri);
    var url = new URI(uri);
    var readChannel = Channels.newChannel(url.toURL().openStream());
    try (var fos = new FileOutputStream(HYPER_PATH.resolve(fileName).toString())) {
      var writeChannel = fos.getChannel();
      writeChannel.transferFrom(readChannel, 0, Long.MAX_VALUE);
    }
  }

  public static String[] readSchemas(String path) throws IOException {
    try (var process = getProcess()) {
      try (var connection = new Connection(process.getEndpoint(), path)) {
        var catalog = connection.getCatalog();
        return catalog.getSchemaNames().stream().map(s -> s.getName().getUnescaped()).toArray(String[]::new);
      }
    }
  }

  public static String addSchema(String path, String schema) throws IOException {
    var schemaName = new SchemaName(schema);
    try (var process = getProcess()) {
      try (var connection = new Connection(process.getEndpoint(), path)) {
        var catalog = connection.getCatalog();
        catalog.createSchema(schemaName);
        return schemaName.getName().getUnescaped();
      }
    }
  }
}
