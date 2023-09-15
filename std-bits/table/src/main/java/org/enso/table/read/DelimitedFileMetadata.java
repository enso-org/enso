package org.enso.table.read;

/**
 * Metadata that can be detected by the DelimitedReader.
 *
 * @param columnCount The number of columns found in the first row.
 * @param definedColumnNames The column names as defined in the file. It will be {@code null} if
 *     {@code GENERATE_HEADERS} is used or if {@code INFER} is used and no headers were found inside the file.
 * @param hasAnyContent Flag indicating if the file contained any data.
 * @param effectiveLineSeparator If it was provided explicitly at construction, the selected separator is used. If
 *     the initial separator was set to {@code null}, the separator detected from file contents will be returned.
 */
public record DelimitedFileMetadata(long columnCount,
                                    String[] definedColumnNames,
                                    boolean hasAnyContent,
                                    String effectiveLineSeparator) {
}
