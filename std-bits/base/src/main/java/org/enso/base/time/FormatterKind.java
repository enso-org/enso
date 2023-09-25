package org.enso.base.time;

/** Specifies how the formatter was constrcuted. */
public enum FormatterKind {
  /** Formatters constructed using `from_simple_pattern`. */
  SIMPLE,

  /** Formatters constructed using `from_iso_week_date_pattern`. */
  ISO_WEEK_DATE,

  /** Formatters constructed from a raw Java formatter or using DateTimeFormatter.ofPattern. */
  RAW_JAVA,

  /** Formatters based on a constant, like ISO_ZONED_DATE_TIME, or the default Enso formatter. */
  CONSTANT
}
