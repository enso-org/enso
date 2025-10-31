package org.enso.tableau;

import com.tableau.hyperapi.Catalog;
import com.tableau.hyperapi.Connection;
import com.tableau.hyperapi.CreateMode;
import com.tableau.hyperapi.HyperException;
import com.tableau.hyperapi.HyperProcess;
import com.tableau.hyperapi.Inserter;
import com.tableau.hyperapi.SchemaName;
import com.tableau.hyperapi.SqlType;
import com.tableau.hyperapi.TableDefinition;
import com.tableau.hyperapi.TableName;
import com.tableau.hyperapi.Telemetry;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.channels.Channels;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.time.LocalDate;
import java.time.LocalTime;
import java.time.ZonedDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.ColumnBooleanStorage;
import org.enso.table.data.column.storage.ColumnDoubleStorage;
import org.enso.table.data.column.storage.ColumnLongStorage;
import org.enso.table.data.column.storage.ColumnStorage;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.NullType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Context;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Class responsible for reading/writing Tableau Hyper files. */
public class HyperFormat {
  public static final Path HYPER_PATH = Path.of(getHyperPath());
  private static HyperProcess process;

  private static final Logger LOGGER = LoggerFactory.getLogger(HyperFormat.class);

  private static String getHyperPath() {
    if (System.getenv("HYPER_PATH") != null) {
      return System.getenv("HYPER_PATH");
    }
    if (System.getenv("ENSO_DATA_DIRECTORY") != null) {
      return System.getenv("ENSO_DATA_DIRECTORY") + "/hyper";
    } else {
      return switch (OSPlatform.CurrentPlatform) {
        case WINDOWS -> System.getenv("LocalAppData") + "/enso/hyper";
        case MAC_ARM64, MAX_X64 ->
            System.getProperty("user.home") + "/Library/Application Support/org.enso/hyper";
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
          case WINDOWS ->
              downloadHyper(
                  "https://enso-data-samples.s3.us-west-1.amazonaws.com/tableau/hyperd.exe",
                  "hyperd.exe",
                  false);
          case MAC_ARM64 ->
              downloadHyper(
                  "https://enso-data-samples.s3.us-west-1.amazonaws.com/tableau/macos-arm64/hyperd",
                  "hyperd",
                  true);
          case MAX_X64 ->
              downloadHyper(
                  "https://enso-data-samples.s3.us-west-1.amazonaws.com/tableau/macos-x64/hyperd",
                  "hyperd",
                  true);
          case LINUX ->
              downloadHyper(
                  "https://enso-data-samples.s3.us-west-1.amazonaws.com/tableau/linux/hyperd",
                  "hyperd",
                  true);
          case OTHER ->
              throw new IOException("Unsupported platform: " + OSPlatform.CurrentPlatform);
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
        var classLoader = new TableauClassLoader();
        var jnaPath = classLoader.getResource("jnidispatch");
        Thread.currentThread().setContextClassLoader(classLoader);
        LOGGER.info("Starting Hyper process: {}.", HYPER_PATH);
        try {
          if (jnaPath != null) {
            // Use URI to correctly handle spaces and other encoded characters.
            System.setProperty(
                "jna.boot.library.path", Path.of(jnaPath.toURI()).getParent().toString());
          }
          process = new HyperProcess(HYPER_PATH, Telemetry.DO_NOT_SEND_USAGE_DATA_TO_TABLEAU);
        } catch (Throwable ioe) {
          LOGGER.error("Failed to start Hyper process.", ioe);
          throw new IOException("Failed to start Hyper process.", ioe);
        }
      } finally {
        Thread.currentThread().setContextClassLoader(contextClassLoader);
      }
    }

    return process;
  }

  private static final class TableauClassLoader extends ClassLoader {
    private TableauClassLoader() {
      super(HyperFormat.class.getClassLoader());
    }

    @Override
    public URL getResource(String name) {

      if (name.contains("jnidispatch")) {
        var libIdx = name.lastIndexOf("/");
        var dotIdx = name.indexOf(".");
        var osLibName =
            dotIdx == -1 ? name.substring(libIdx + 1) : name.substring(libIdx + 1, dotIdx);
        // Windows libs don't have `lib` prefix.
        var libName = osLibName.startsWith("lib") ? osLibName.substring(3) : osLibName;
        var bindings = Context.getCurrent().getBindings("enso");
        var found = bindings.invokeMember("find_native_library", libName);
        try {
          if (found == null || found.asString() == null) {
            LOGGER.warn("Failed to find library `{}`. Retrying with a fallback", libName);
            return super.getResource(name);
          } else {
            return new File(found.asString()).toURI().toURL();
          }
        } catch (MalformedURLException e) {
          return null;
        }
      } else {
        return super.getResource(name);
      }
    }

    @Override
    public InputStream getResourceAsStream(String name) {

      if (name.endsWith(".dylib") || name.endsWith(".so") || name.endsWith(".dll")) {
        var libIdx = name.lastIndexOf("/");
        var dotIdx = name.indexOf(".");
        var osLibName =
            dotIdx == -1 ? name.substring(libIdx + 1) : name.substring(libIdx + 1, dotIdx);
        // Windows libs don't have `lib` prefix.
        var libName = osLibName.startsWith("lib") ? osLibName.substring(3) : osLibName;
        var bindings = Context.getCurrent().getBindings("enso");
        var found = bindings.invokeMember("find_native_library", libName);
        try {
          if (found == null || found.asString() == null) {
            LOGGER.warn("Failed to find library `{}`. Retrying with a fallback", libName);
            return super.getResourceAsStream(name);
          } else {
            return new FileInputStream(found.asString());
          }
        } catch (FileNotFoundException e) {
          return null;
        }
      } else {
        return super.getResourceAsStream(name);
      }
    }
  }

  private static void downloadHyper(String uri, String fileName, boolean setExecutable)
      throws IOException,
          URISyntaxException,
          InvalidPathException,
          UnsupportedOperationException,
          SecurityException {
    LOGGER.info("Downloading Hyper from: {}", uri);
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
    getProcess();
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

  public static String[] writeTable(
      String path,
      String schemaName,
      String tableName,
      Table table,
      boolean append,
      boolean matchColumnsByName,
      boolean throwDontWarn)
      throws IOException {
    List<String> warningUnmatchedColumns = new ArrayList<>();
    getProcess();
    try (var connection =
        new Connection(process.getEndpoint(), path, CreateMode.CREATE_IF_NOT_EXISTS)) {
      TableDefinition tableDef;
      if (append && tableExists(schemaName, tableName, connection)) {
        tableDef = connection.getCatalog().getTableDefinition(new TableName(schemaName, tableName));
      } else {
        tableDef = createTable(schemaName, tableName, table.getColumns(), connection);
      }
      insertData(
          table, tableDef, connection, matchColumnsByName, warningUnmatchedColumns, throwDontWarn);
      connection.close();
    }
    return warningUnmatchedColumns.toArray(String[]::new);
  }

  private static boolean tableExists(String schemaName, String tableName, Connection connection) {
    final var sn = new SchemaName(schemaName);
    final var tn = new TableName(schemaName, tableName);
    return connection.getCatalog().getTableNames(sn).contains(tn);
  }

  private static TableDefinition createTable(
      String schemaName, String tableName, Column[] columns, Connection connection) {
    final var sn = new SchemaName(schemaName);
    if (!connection.getCatalog().getSchemaNames().contains(sn)) {
      connection.getCatalog().createSchema(sn);
    }

    var tableDef = new TableDefinition(new TableName(schemaName, tableName));
    for (var col : columns) {
      String columnName = col.getName();
      var sqlType = mapEnsoTypeToSqlType(col.getStorage().getType());
      tableDef.addColumn(columnName, sqlType);
    }

    connection.executeCommand("DROP TABLE IF EXISTS \"" + schemaName + "\".\"" + tableName + "\"");
    connection.getCatalog().createTable(tableDef);
    return tableDef;
  }

  private static SqlType mapEnsoTypeToSqlType(StorageType<?> type) {
    return switch (type) {
      case TextType t -> SqlType.text();
      case IntegerType t -> SqlType.bigInt();
      case FloatType t -> SqlType.doublePrecision();
      case BooleanType t -> SqlType.bool();
      case DateType t -> SqlType.date();
      case TimeOfDayType t -> SqlType.time();
      case DateTimeType t -> SqlType.timestampTz();
      // https://tableau.github.io/hyper-db/docs/sql/datatype/numeric
      // Precisions over 18 require 128-bit for internal storage. Processing 128-bit numeric
      // values is often slower than processing 64-bit values, so it is advisable to use
      // a sensible precision for the use case at hand instead of always using the maximum
      // precision by default.
      // TODO fix this after https://github.com/enso-org/enso/issues/13022
      case BigDecimalType t -> SqlType.numeric(18, 9);
      default -> throw new HyperUnsupportedTypeError(type.toString());
    };
  }

  private static void insertData(
      Table table,
      TableDefinition tableDef,
      Connection connection,
      boolean matchColumnsByName,
      List<String> warningUnmatchedColumns,
      boolean throwDontWarn) {
    var columnStorages =
        getOrderedStorages(
            table, tableDef, matchColumnsByName, warningUnmatchedColumns, throwDontWarn);

    validateTypesMatch(columnStorages, tableDef);

    try (Inserter inserter = new Inserter(connection, tableDef)) {
      for (int row = 0; row < table.rowCount(); ++row) {
        for (ColumnStorage<?> storage : columnStorages) {
          addValueToInserter(inserter, storage, row);
        }
        inserter.endRow();
      }
      inserter.execute();
    }
  }

  private static ColumnStorage<?>[] getOrderedStorages(
      Table table,
      TableDefinition tableDef,
      boolean matchColumnsByName,
      List<String> warningUnmatchedColumns,
      boolean throwDontWarn) {
    int numberOfRows = table.rowCount();
    if (matchColumnsByName) {
      var tableDefColumns = tableDef.getColumns();
      var existingColumnNames = new String[tableDefColumns.size()];
      for (int i = 0; i < tableDefColumns.size(); ++i) {
        existingColumnNames[i] =
            tableDefColumns.get(i).getName().toString().replaceAll("^\"|\"$", "");
      }

      validateNoExtraColumnsByName(
          table, existingColumnNames, warningUnmatchedColumns, throwDontWarn);

      var result = new ColumnStorage[existingColumnNames.length];
      for (int i = 0; i < existingColumnNames.length; ++i) {
        String name = existingColumnNames[i];
        var tableColumn = table.getColumnByName(name);
        result[i] =
            tableColumn == null
                ? Builder.fromRepeatedItem(null, numberOfRows)
                : tableColumn.getStorage();
        ;
      }
      return result;
    } else { // match by position
      validateNoExtraColumnsByPosition(table, tableDef, warningUnmatchedColumns, throwDontWarn);
      Column[] sourceColumns = table.getColumns();
      int defColumnCount = tableDef.getColumns().size();

      var result = new ColumnStorage[defColumnCount];
      for (int i = 0; i < result.length; i++) {
        result[i] =
            i < sourceColumns.length
                ? sourceColumns[i].getStorage()
                : Builder.fromRepeatedItem(null, numberOfRows);
      }
      return result;
    }
  }

  private static void addValueToInserter(Inserter inserter, ColumnStorage<?> storage, int row) {
    if (storage.isNothing(row)) {
      inserter.addNull();
    } else {
      switch (storage) {
        case ColumnDoubleStorage doubleStorage -> inserter.add(doubleStorage.getItemAsDouble(row));
        case ColumnLongStorage longStorage -> inserter.add(longStorage.getItemAsLong(row));
        case ColumnBooleanStorage boolStorage -> inserter.add(boolStorage.getItemAsBoolean(row));
        default -> {
          Object value = storage.getItemBoxed(row);
          switch (value) {
            case String s -> inserter.add(s);
            case LocalDate ld -> inserter.add(ld);
            case LocalTime lt -> inserter.add(lt);
            case ZonedDateTime zdt -> inserter.add(zdt);
            case BigDecimal bd -> inserter.add(bd);
            default -> throw new HyperUnsupportedTypeError(value.toString());
          }
        }
      }
    }
  }

  private static void validateNoExtraColumnsByName(
      Table table,
      String[] allowedColumnNames,
      List<String> warningUnmatchedColumns,
      boolean throwDontWarn) {
    Set<String> allowed = Set.of(allowedColumnNames);
    Set<String> tableColumnNames =
        Arrays.stream(table.getColumns()).map(Column::getName).collect(Collectors.toSet());

    String[] extraColumns =
        tableColumnNames.stream().filter(name -> !allowed.contains(name)).toArray(String[]::new);

    if (extraColumns.length > 0) {
      throw new HyperUnmatchedColumns(extraColumns);
    }

    List<String> missingColumns =
        Arrays.stream(allowedColumnNames).filter(name -> !tableColumnNames.contains(name)).toList();

    if (!missingColumns.isEmpty()) {
      if (throwDontWarn) {
        throw new HyperUnmatchedColumns(missingColumns.toArray(String[]::new));
      } else {
        warningUnmatchedColumns.addAll(missingColumns);
      }
    }
  }

  private static void validateNoExtraColumnsByPosition(
      Table table,
      TableDefinition tableDef,
      List<String> warningUnmatchedColumns,
      boolean throwDontWarn) {
    int tableColumnCount = table.getColumns().length;
    int defColumnCount = tableDef.getColumns().size();

    if (tableColumnCount > defColumnCount) {
      String[] extraColumnNames =
          IntStream.range(defColumnCount, tableColumnCount)
              .mapToObj(i -> table.getColumns()[i].getName())
              .toArray(String[]::new);

      throw new HyperUnmatchedColumns(extraColumnNames);
    }

    if (tableColumnCount < defColumnCount) {
      String[] missingColumnNames =
          IntStream.range(tableColumnCount, defColumnCount)
              .mapToObj(
                  i -> tableDef.getColumns().get(i).getName().toString().replaceAll("^\"|\"$", ""))
              .toArray(String[]::new);

      if (throwDontWarn) {
        throw new HyperUnmatchedColumns(missingColumnNames);
      } else {
        warningUnmatchedColumns.addAll(List.of(missingColumnNames));
      }
    }
  }

  private static void validateTypesMatch(ColumnStorage<?>[] storages, TableDefinition tableDef) {
    for (int i = 0; i < storages.length; i++) {
      var storage = storages[i];
      if (storage.getType() instanceof NullType) {
        continue; // Allow NULLs to append to anything
      }

      SqlType expectedSqlType = tableDef.getColumns().get(i).getType();
      SqlType actualSqlType = mapEnsoTypeToSqlType(storage.getType());

      if (!expectedSqlType.equals(actualSqlType)) {
        String columnName = tableDef.getColumns().get(i).getName().toString();
        throw new HyperTypeMismatch(
            columnName, expectedSqlType.toString(), actualSqlType.toString());
      }
    }
  }
}
