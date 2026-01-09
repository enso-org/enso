package org.enso.table.read;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.type.*;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.problems.ProblemAggregator;
import uk.co.jdunkerley.yxdb.YxdbField;
import uk.co.jdunkerley.yxdb.YxdbReader;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.format.DateTimeParseException;
import java.util.Arrays;
import java.util.stream.IntStream;

public final class AlteryxYXDBReader {
    /**
     * Reads an Alteryx YXDB file and returns its contents as a Table.
     *
     * @param path     the path to the YXDB file.
     * @return a Table containing the data from the YXDB file.
     */
    public static Table read(String path, ProblemAggregator problemAggregator) throws FileNotFoundException, IllegalArgumentException, IllegalStateException {
        // Test that the path exists
        if (!Files.exists(Path.of(path))) {
            throw new FileNotFoundException(path);
        }

        try (var yxdbReader = new YxdbReader(path)) {
            var recordCount = yxdbReader.numRecords();
            var fields = yxdbReader.fields();
            var storageTypes = Arrays.stream(fields).map(AlteryxYXDBReader::mapYXDBField).toArray(StorageType[]::new);
            var storages = Arrays.stream(storageTypes).map(st -> st.makeBuilder(recordCount, problemAggregator)).toArray(Builder[]::new);

            while (yxdbReader.next()) {
                try {
                    for (int i = 0; i < storages.length; i++) {
                        storages[i].append(yxdbReader.read(i));
                    }
                } catch (IndexOutOfBoundsException _) {
                    throw new IllegalArgumentException("The YXDB file appears to be corrupted on row " + storages[0].getCurrentSize());
                } catch (DateTimeParseException _) {
                    throw new IllegalArgumentException("The YXDB file contains invalid date/time data on row " + storages[0].getCurrentSize());
                }
            }

            var columns = IntStream.range(0, storages.length)
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

    private static StorageType<?> mapYXDBField(YxdbField field) {
        return switch (field.yxdbType()) {
            case "Bool" -> BooleanType.INSTANCE;
            case "Byte" -> IntegerType.INT_8;
            case "Int16" -> IntegerType.INT_16;
            case "Int32" -> IntegerType.INT_32;
            case "Int64" -> IntegerType.INT_64;
            case "Float", "Double" -> FloatType.FLOAT_64;
            case "FixedDecimal" -> BigDecimalType.INSTANCE;
            case "String", "WString" -> TextType.variableLengthWithLimit(field.size());
            case "V_String", "V_WString", "SpatialObj" -> TextType.VARIABLE_LENGTH;
            case "Date" -> DateType.INSTANCE;
            case "Time" -> TimeOfDayType.INSTANCE;
            case "DateTime" -> DateTimeType.INSTANCE;
            case "Blob" -> AnyObjectType.INSTANCE;
            default -> throw new IllegalStateException("Unsupported YXDB field type: " + field.yxdbType());
        };
    }
}
