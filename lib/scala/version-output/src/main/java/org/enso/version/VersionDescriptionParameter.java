package org.enso.version;

/**
 * Defines an additional parameter for the version description.
 *
 * @param humanReadableName the human readable prefix added when printing this parameter in
 *     human-readable format
 * @param jsonName the key when outputting the parameter in JSON format
 * @param value the value to use for the parameter; depending on if the whole version description
 *     will be queried as a human-readable version or in JSON, this value should be in the right
 *     format
 */
public record VersionDescriptionParameter(
    String humanReadableName, String jsonName, String value) {}
