package org.enso.table.read;

import org.enso.base.polyglot.EnsoMeta;

import org.enso.table.data.column.builder.Builder;
import org.enso.table.data.column.storage.type.*;
import org.enso.table.data.table.Column;
import org.enso.table.data.table.Table;
import org.enso.table.problems.ProblemAggregator;
import org.graalvm.polyglot.Value;
import uk.co.jdunkerley.yxdb.YxdbField;
import uk.co.jdunkerley.yxdb.YxdbReader;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.format.DateTimeParseException;
import java.util.Arrays;
import java.util.stream.IntStream;

public final class AlteryxYXDBReader {
    private static Value makeIllegalState(String message) {
        return makeEnsoError("Standard.Base.Errors.Illegal_State", "Illegal_State", "Error", message, null);
    }

    private static Value makeEnsoError(String module, String name, String constructor, Object... args) {
        var ensoType = EnsoMeta.getType(module, name);
        if (!ensoType.isMetaObject() || !ensoType.getMetaQualifiedName().equals(module + "." + name)) {
            return makeIllegalState("Unable to create error of type " + module + "." + name);
        }

        var ensoConstructor = ensoType.getMember(constructor);
        if (ensoConstructor == null || (!ensoConstructor.canInstantiate() && !ensoConstructor.canExecute())) {
            return makeIllegalState("Unable to create error of type " + module + "." + name + " because constructor " + constructor + " is not found or not executable.");
        }

        var errorInstance = ensoConstructor.canInstantiate() ? ensoConstructor.newInstance(args) : ensoConstructor.execute(args);
        if (errorInstance == null || errorInstance.canExecute()) {
            return makeIllegalState("Unable to create error of type " + module + "." + name + " because constructor " + constructor + " did not return a valid instance.");
        }

        var errorType = EnsoMeta.getType("Standard.Base.Error", "Error");
        assert errorType.isMetaObject();
        assert errorType.getMetaQualifiedName().equals("Standard.Base.Error.Error");

        var ensoError = errorType.invokeMember("throw", errorInstance);
        assert ensoError.isException();
        return ensoError;
    }

    /**
     * Reads an Alteryx YXDB file and returns its contents as a Table.
     *
     * @param path     the path to the YXDB file.
     * @return a Table containing the data from the YXDB file.
     */
    public static Table read(String path, ProblemAggregator problemAggregator) {
        // Test that the path exists
        if (!Files.exists(Path.of(path))) {
            throw makeEnsoError("Standard.Base.Errors.File_Error", "File_Error", "Not_Found", path + "_govnr").throwException();
        }

        var state = 0;
        try (var yxdbReader = new YxdbReader(path)) {
            state = 1;
            var recordCount = yxdbReader.numRecords();
            var fields = yxdbReader.fields();
            var storageTypes = Arrays.stream(fields).map(AlteryxYXDBReader::mapYXDBField).toArray(StorageType[]::new);
            var storages = Arrays.stream(storageTypes).map(st-> st.makeBuilder(recordCount, problemAggregator)).toArray(Builder[]::new);

            state = 2;
            while (yxdbReader.next()) {
                try {
                    for (int i = 0; i < storages.length; i++) {
                        storages[i].append(yxdbReader.read(i));
                    }
                } catch (IndexOutOfBoundsException _) {
                    throw new IllegalArgumentException("The YXDB file appears to be corrupted on row " +  storages[0].getCurrentSize());
                } catch (DateTimeParseException _) {
                    throw new IllegalArgumentException("The YXDB file contains invalid date/time data on row " +  storages[0].getCurrentSize());
                }
            }

            state = 3;
            var columns = IntStream.range(0, storages.length)
                .mapToObj(i -> new Column(fields[i].name(), storages[i].seal()))
                .toArray(Column[]::new);
            return new Table(columns);
        } catch (IllegalArgumentException | IOException exc) {
            var message = exc.getMessage();
            var ensoError = switch (state) {
                case 0, 1, 2 ->
                    makeEnsoError("Standard.Base.Errors.Illegal_Argument", "Illegal_Argument", "Error", message);
                default -> makeIllegalState("An unexpected error occurred while reading the YXDB file: " + message);
            };
            throw ensoError.throwException();
        } catch (Exception exc) {
            throw  makeIllegalState("An unexpected error occurred: " + exc.getMessage()).throwException();
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
