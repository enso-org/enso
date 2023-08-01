package org.enso.table.util;

/**
 * A helper class for managing limitations of entity names.
 *
 * <p>{@see Naming_Properties} in Enso
 */
public interface NamingProperties {
  /**
   * Returns the maximum length of a name.
   *
   * <p>Will return `null` if there is no limit.
   */
  Long size_limit();

  /**
   * Returns the size of the string in the target encoding.
   *
   * <p>If there is no size limit, this function may panic.
   */
  long encoded_size(String name);

  /**
   * Truncates the string to the given size, measured in the target encoding.
   *
   * <p>If there is no size limit, this function may panic.
   */
  String truncate(String name, long max_encoded_size);
}
