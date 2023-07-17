package org.enso.table.data.column.operation.map.datetime;

import java.time.temporal.ChronoField;
import java.time.temporal.IsoFields;
import java.time.temporal.Temporal;
import java.time.temporal.TemporalField;
import org.enso.table.data.column.operation.map.numeric.GenericUnaryIntegerOp;
import org.enso.table.data.column.storage.Storage;

public class DatePartExtractors {
  public static <T extends Temporal, I extends Storage<T>>
      GenericUnaryIntegerOp<Temporal, T, I> make_op(String name, TemporalField field) {
    return new GenericUnaryIntegerOp<>(name) {
      @Override
      protected long doGenericOperation(Temporal value) {
        return value.getLong(field);
      }
    };
  }

  public static <T extends Temporal, I extends Storage<T>>
      GenericUnaryIntegerOp<Temporal, T, I> year() {
    return make_op("year", ChronoField.YEAR);
  }

  public static <T extends Temporal, I extends Storage<T>>
      GenericUnaryIntegerOp<Temporal, T, I> quarter() {
    return new GenericUnaryIntegerOp<>("quarter") {
      @Override
      protected long doGenericOperation(Temporal value) {
        long month = value.get(ChronoField.MONTH_OF_YEAR);
        return (month - 1) / 3 + 1;
      }
    };
  }

  public static <T extends Temporal, I extends Storage<T>>
      GenericUnaryIntegerOp<Temporal, T, I> month() {
    return make_op("month", ChronoField.MONTH_OF_YEAR);
  }

  public static <T extends Temporal, I extends Storage<T>>
      GenericUnaryIntegerOp<Temporal, T, I> week() {
    return make_op("week", IsoFields.WEEK_OF_WEEK_BASED_YEAR);
  }

  public static <T extends Temporal, I extends Storage<T>>
      GenericUnaryIntegerOp<Temporal, T, I> day() {
    return make_op("day", ChronoField.DAY_OF_MONTH);
  }

  public static <T extends Temporal, I extends Storage<T>>
      GenericUnaryIntegerOp<Temporal, T, I> hour() {
    return make_op("hour", ChronoField.HOUR_OF_DAY);
  }

  public static <T extends Temporal, I extends Storage<T>>
      GenericUnaryIntegerOp<Temporal, T, I> minute() {
    return make_op("minute", ChronoField.MINUTE_OF_HOUR);
  }

  public static <T extends Temporal, I extends Storage<T>>
      GenericUnaryIntegerOp<Temporal, T, I> second() {
    return make_op("second", ChronoField.SECOND_OF_MINUTE);
  }

  public static <T extends Temporal, I extends Storage<T>>
      GenericUnaryIntegerOp<Temporal, T, I> millisecond() {
    return make_op("millisecond", ChronoField.MILLI_OF_SECOND);
  }

  public static <T extends Temporal, I extends Storage<T>>
      GenericUnaryIntegerOp<Temporal, T, I> microsecond() {
    return new GenericUnaryIntegerOp<>("microsecond") {
      @Override
      protected long doGenericOperation(Temporal value) {
        long micros = value.get(ChronoField.MICRO_OF_SECOND);
        return micros % 1000;
      }
    };
  }

  public static <T extends Temporal, I extends Storage<T>>
      GenericUnaryIntegerOp<Temporal, T, I> nanosecond() {
    return new GenericUnaryIntegerOp<>("nanosecond") {
      @Override
      protected long doGenericOperation(Temporal value) {
        long micros = value.get(ChronoField.NANO_OF_SECOND);
        return micros % 1000;
      }
    };
  }
}
