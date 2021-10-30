package org.enso.interpreter.util;

import java.util.List;
import java.util.Optional;
import scala.Option;
import scala.collection.Seq;
import scala.jdk.javaapi.CollectionConverters;
import scala.jdk.javaapi.OptionConverters;

/** Utility class for converting between Scala and Java basic classes. */
public class ScalaConversions {
  /**
   * Converts a Scala {@link Option} to a Java {@link Optional}.
   *
   * @param option the scala option to convert
   * @return the corresponding java optional
   */
  public static <T> Optional<T> asJava(Option<T> option) {
    return OptionConverters.toJava(option);
  }

  /**
   * Converts a Scala {@link Seq} to a Java {@link List}.
   *
   * @param list the scala list to convert
   * @return the corresponding java list
   */
  public static <T> List<T> asJava(Seq<T> list) {
    return CollectionConverters.asJava(list);
  }
}
