package org.enso.tableau;

import com.tableau.hyperapi.Nullability;
import com.tableau.hyperapi.TableDefinition;
import com.tableau.hyperapi.TypeTag;
import java.sql.Types;
import java.util.OptionalInt;

public record HyperTableColumn(
    int index,
    String name,
    int typeID,
    boolean nullable,
    OptionalInt length,
    OptionalInt precision,
    OptionalInt scale) {
  /** Type ID for JSON data. */
  public static final int JSON = 10001;

  /** Type ID for INTERVAL data. */
  public static final int INTERVAL = 10002;

  static HyperTableColumn fromHyperColumn(int index, TableDefinition.Column hyperColumn) {
    return new HyperTableColumn(
        index,
        hyperColumn.getName().getUnescaped(),
        mapTypeTag(hyperColumn.getType().getTag()),
        hyperColumn.getNullability().equals(Nullability.NULLABLE),
        hyperColumn.getType().getMaxLength(),
        hyperColumn.getType().getPrecision(),
        hyperColumn.getType().getScale());
  }

  private static int mapTypeTag(TypeTag tag) {
    return switch (tag) {
      case BOOL -> Types.BOOLEAN;
      case BIG_INT -> Types.BIGINT;
      case SMALL_INT -> Types.SMALLINT;
      case INT -> Types.INTEGER;
      case NUMERIC -> Types.NUMERIC;
      case FLOAT -> Types.FLOAT;
      case DOUBLE -> Types.DOUBLE;
      case TEXT, VARCHAR -> Types.VARCHAR;
      case CHAR -> Types.CHAR;
      case DATE -> Types.DATE;
      case TIME -> Types.TIME;
      case TIMESTAMP -> Types.TIMESTAMP;
      case TIMESTAMP_TZ -> Types.TIMESTAMP_WITH_TIMEZONE;
      case JSON -> JSON;
      case INTERVAL -> INTERVAL;
      default -> Types.OTHER;
    };
  }
}
