package org.enso.tableau;

import com.tableau.hyperapi.*;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.channels.Channels;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.IntStream;
import org.enso.table.data.table.Column;
import org.enso.table.problems.ProblemAggregator;

/** Class responsible for reading from Tableau Hyper files. */
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
      } catch (IOException | UnsupportedOperationException | SecurityException e) {
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
          case MAX_X64 -> downloadHyper(
              "https://enso-data-samples.s3.us-west-1.amazonaws.com/tableau/macos-x64/hyperd",
              "hyperd",
              true);
          case LINUX -> downloadHyper(
              "https://enso-data-samples.s3.us-west-1.amazonaws.com/tableau/linux/hyperd",
              "hyperd",
              true);
          case OTHER -> throw new IOException(
              "Unsupported platform: " + OSPlatform.CurrentPlatform);
        }
      }
    } catch (IOException
        | URISyntaxException
        | InvalidPathException
        | UnsupportedOperationException
        | SecurityException e) {
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
          LOGGER.log(Level.SEVERE, "Failed to start Hyper process.", ioe);
          throw new IOException("Failed to start Hyper process.", ioe);
        }
      } finally {
        Thread.currentThread().setContextClassLoader(contextClassLoader);
      }
    }

    return process;
  }

  private static void downloadHyper(String uri, String fileName, boolean setExecutable)
      throws IOException,
          URISyntaxException,
          InvalidPathException,
          UnsupportedOperationException,
          SecurityException {
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

  private static Connection getConnection(String path) throws IOException {
    var process = getProcess();
    try {
      return new Connection(process.getEndpoint(), path, CreateMode.NONE);
    } catch (HyperException e) {
      if (e.getMessage().contains("The database does not exist")) {
        throw new FileNotFoundException("Database not found: " + path);
      } else {
        throw new IOException("Failed to open database: " + path, e);
      }
    }
  }

  public static String[] readSchemas(String path) throws IOException {
    try (var connection = getConnection(path)) {
      var catalog = connection.getCatalog();
      return catalog.getSchemaNames().stream()
          .map(s -> s.getName().getUnescaped())
          .toArray(String[]::new);
    }
  }

  public static HyperTable[] listTablesAllSchemas(String path) throws IOException {
    try (var connection = getConnection(path)) {
      var catalog = connection.getCatalog();
      return listTablesImpl(catalog, catalog.getSchemaNames());
    }
  }

  public static HyperTable[] listTables(String path, String schemaName) throws IOException {
    var schemaNames = List.of(new SchemaName(schemaName));
    try (var connection = getConnection(path)) {
      var catalog = connection.getCatalog();
      return listTablesImpl(catalog, schemaNames);
    }
  }

  private static HyperTable[] listTablesImpl(Catalog catalog, List<SchemaName> schemaNames) {
    var output = new ArrayList<HyperTable>();
    for (var schemaName : schemaNames) {
      var tables = catalog.getTableNames(schemaName);
      for (var table : tables) {
        output.add(
            new HyperTable(schemaName.getName().getUnescaped(), table.getName().getUnescaped()));
      }
    }
    return output.toArray(HyperTable[]::new);
  }

  public static HyperTableColumn[] readStructure(String path, String schemaName, String tableName)
      throws IOException {
    var tableNameObject = new TableName(new SchemaName(schemaName), tableName);
    try (var connection = getConnection(path)) {
      return readStructureInternal(connection, tableNameObject);
    }
  }

  private static HyperTableColumn[] readStructureInternal(
      Connection connection, TableName tableNameObject) {
    try {
      var catalog = connection.getCatalog();
      var definition = catalog.getTableDefinition(tableNameObject);
      var columns = definition.getColumns();
      return IntStream.range(0, columns.size())
          .mapToObj(i -> HyperTableColumn.fromHyperColumn(i, columns.get(i)))
          .toArray(HyperTableColumn[]::new);
    } catch (HyperException e) {
      if (e.getMessage().contains(" does not exist: ")) {
        var schemaObject = tableNameObject.getSchemaName();
        var schemaName =
            schemaObject.isPresent() ? schemaObject.get().getName().getUnescaped() : "";
        throw new HyperTableNotFound(schemaName, tableNameObject.getName().getUnescaped(), e);
      } else {
        throw new HyperQueryError(e.getMessage(), "TABLE_INFO " + tableNameObject, e);
      }
    }
  }

  public static Column[] readTable(
      String path,
      String schemaName,
      String tableName,
      Integer rowLimit,
      ProblemAggregator problemAggregator)
      throws IOException {
    var tableNameObject = new TableName(new SchemaName(schemaName), tableName);
    var query = "SELECT * FROM " + tableNameObject + (rowLimit == null ? "" : " LIMIT " + rowLimit);
    try (var connection = getConnection(path)) {
      var columns = readStructureInternal(connection, tableNameObject);

      var builders =
          Arrays.stream(columns)
              .map(
                  c ->
                      TableColumnBuilder.create(
                          c, rowLimit == null ? 1000 : rowLimit, problemAggregator))
              .toList();

      var result = connection.executeQuery(query);
      while (result.nextRow()) {
        builders.forEach(b -> b.append(result));
      }

      var storages = builders.stream().map(TableColumnBuilder::seal).toList();
      return IntStream.range(0, columns.length)
          .mapToObj(i -> new Column(columns[i].name(), storages.get(i)))
          .toArray(Column[]::new);
    } catch (HyperException e) {
      if (e.getMessage().contains(" does not exist: ")) {
        throw new HyperTableNotFound(schemaName, tableName, e);
      } else {
        throw new HyperQueryError(e.getMessage(), query, e);
      }
    }
  }
}
