package org.enso.interpreter.util;

import scala.Option;
import scala.collection.JavaConverters;
import scala.collection.Seq;

import java.util.List;
import java.util.Optional;

/** Utility class for converting between Scala and Java basic classes. */
public class ScalaConversions {
  /**
   * Converts a Scala {@link Option} to a Java {@link Optional}.
   *
   * @param option the scala option to convert
   * @return the corresponding java optional
   */
  public static <T> Optional<T> asJava(Option<T> option) {
    return Optional.ofNullable(option.getOrElse(() -> null));
  }

  /**
   * Converts a Scala {@link Seq} to a Java {@link List}.
   *
   * @param list the scala list to convert
   * @return the corresponding java list
   */
  public static <T> List<T> asJava(Seq<T> list) {
    return JavaConverters.seqAsJavaList(list);
  }
}
