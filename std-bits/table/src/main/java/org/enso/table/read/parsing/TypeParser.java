package org.enso.table.read.parsing;

import java.util.ArrayList;
import java.util.List;
import org.enso.table.data.column.builder.object.Builder;
import org.enso.table.data.column.storage.Storage;
import org.enso.table.data.column.storage.StringStorage;
import org.enso.table.read.ParsingProblem;
import org.enso.table.read.WithWarnings;

public abstract class TypeParser {
  public static class InvalidFormatMarker {}

  public static InvalidFormatMarker INVALID_FORMAT = new InvalidFormatMarker();

  public abstract Object parseSingleValue(String text);

  public abstract Builder makeBuilderWithCapacity(long capacity);

  public WithWarnings<Storage> parseColumn(StringStorage sourceStorage) {
    List<String> invalidCells = new ArrayList<>();
    Builder builder = makeBuilderWithCapacity(sourceStorage.size());

    for (int i = 0; i < sourceStorage.size(); ++i) {
      String cell = sourceStorage.getItem(i);
      var parsed = parseSingleValue(cell);
      if (parsed == INVALID_FORMAT) {
        invalidCells.add(cell);
        parsed = null;
      }

      builder.appendNoGrow(parsed);
    }

    var storage = builder.seal();
    List<ParsingProblem> problems =
        invalidCells.isEmpty() ? List.of() : List.of(new InvalidFormat(invalidCells));
    return new WithWarnings<>(storage, problems);
  }
}
