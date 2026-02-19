package org.enso.table.read;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.format.DateTimeParseException;
import java.util.Arrays;
import java.util.stream.IntStream;
import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.type.AnyObjectType;
import org.enso.table.data.column.storage.type.BigDecimalType;
import org.enso.table.data.column.storage.type.BooleanType;
import org.enso.table.data.column.storage.type.DateTimeType;
import org.enso.table.data.column.storage.type.DateType;
import org.enso.table.data.column.storage.type.FloatType;
import org.enso.table.data.column.storage.type.IntegerType;
import org.enso.table.data.column.storage.type.StorageType;
import org.enso.table.data.column.storage.type.TextType;
import org.enso.table.data.column.storage.type.TimeOfDayType;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.problems.ProblemAggregator;
import uk.co.jdunkerley.yxdb.Spatial;
import uk.co.jdunkerley.yxdb.YxdbField;
import uk.co.jdunkerley.yxdb.YxdbReader;
import uk.co.jdunkerley.yxdb.YxdbType;

public final class YXDBReader {
  /**
   * Reads an YXDB file and returns its contents as a Table.
   *
   * @param path the path to the YXDB file.
   * @return a Table containing the data from the YXDB file.
   */
  public static Table read(String path, ProblemAggregator problemAggregator)
      throws FileNotFoundException, IllegalArgumentException, IllegalStateException {
    // Test that the path exists
    if (!Files.exists(Path.of(path))) {
      throw new FileNotFoundException(path);
    }

    try (var yxdbReader = new YxdbReader(path)) {
      var recordCount = yxdbReader.numRecords();
      var fields = yxdbReader.fields();
      var storageTypes =
          Arrays.stream(fields).map(YXDBReader::mapYXDBField).toArray(StorageType[]::new);
      var storages =
          Arrays.stream(storageTypes)
              .map(st -> st.makeBuilder(recordCount, problemAggregator))
              .toArray(Builder[]::new);

      while (yxdbReader.next()) {
        try {
          for (int i = 0; i < storages.length; i++) {
            var yxdbValue =
                storageTypes[i] instanceof AnyObjectType
                    ? yxdbReader.readBlob(i)
                    : yxdbReader.read(i);
            storages[i].append(yxdbValue);
          }
        } catch (IndexOutOfBoundsException _) {
          throw new IllegalArgumentException(
              "The YXDB file appears to be corrupted on row " + storages[0].getCurrentSize());
        } catch (DateTimeParseException _) {
          throw new IllegalArgumentException(
              "The YXDB file contains invalid date/time data on row "
                  + storages[0].getCurrentSize());
        }
      }

      var columns =
          IntStream.range(0, storages.length)
              .mapToObj(i -> new Column(fields[i].name(), storages[i].seal()))
              .toArray(Column[]::new);
      return new Table(columns);
    } catch (IllegalArgumentException exc) {
      throw exc;
    } catch (IOException exc) {
      var message = exc.getMessage();
      throw new IllegalArgumentException(exc.getMessage(), exc);
    } catch (Exception exc) {
      throw new IllegalStateException("An unexpected error occurred: " + exc.getMessage(), exc);
    }
  }

  /**
   * Converts a spatial object in byte array format to its GeoJSON representation.
   *
   * @param spatialObj the spatial object as a byte array.
   * @return the GeoJSON representation of the spatial object.
   */
  public static String spatialObjectToGeoJSON(byte[] spatialObj) {
    return spatialObj == null ? null : Spatial.toGeoJson(spatialObj);
  }

  private static StorageType<?> mapYXDBField(YxdbField field) {
    return switch (field.yxdbType()) {
      case YxdbType.BOOLEAN -> BooleanType.INSTANCE;
      case YxdbType.BYTE -> IntegerType.INT_8;
      case YxdbType.INT16 -> IntegerType.INT_16;
      case YxdbType.INT32 -> IntegerType.INT_32;
      case YxdbType.INT64 -> IntegerType.INT_64;
      case YxdbType.FLOAT, YxdbType.DOUBLE -> FloatType.FLOAT_64;
      case YxdbType.DECIMAL -> BigDecimalType.INSTANCE;
      case YxdbType.STRING, YxdbType.WSTRING -> TextType.variableLengthWithLimit(field.size());
      case YxdbType.V_STRING, YxdbType.V_WSTRING -> TextType.VARIABLE_LENGTH;
      case YxdbType.DATE -> DateType.INSTANCE;
      case YxdbType.TIME -> TimeOfDayType.INSTANCE;
      case YxdbType.DATETIME -> DateTimeType.INSTANCE;
      case YxdbType.BLOB, YxdbType.SPATIAL_OBJ -> AnyObjectType.INSTANCE;
      default ->
          throw new IllegalStateException("Unsupported YXDB field type: " + field.yxdbType());
    };
  }
}
