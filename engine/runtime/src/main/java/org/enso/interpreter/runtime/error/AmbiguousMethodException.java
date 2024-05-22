package org.enso.interpreter.runtime.error;

import com.oracle.truffle.api.exception.AbstractTruffleException;
import com.oracle.truffle.api.nodes.Node;
import java.util.List;
import java.util.Objects;
import org.enso.interpreter.runtime.callable.function.Function;
import org.enso.interpreter.runtime.data.Type;

public class AmbiguousMethodException extends AbstractTruffleException {
  public AmbiguousMethodException(
      Node location, Type type, String methodName, List<Function> candidates) {
    super(constructMessage(type, methodName, candidates), location);
  }

  private static String constructMessage(Type type, String methodName, List<Function> candidates) {
    Objects.requireNonNull(type);
    Objects.requireNonNull(methodName);
    Objects.requireNonNull(candidates);
    if (candidates.size() < 2) {
      throw new IllegalArgumentException("candidates list must contain at least two functions");
    }
    var candidateNames = candidates.stream().map(f -> f.toString(false)).toList();
    var sb = new StringBuilder();
    sb.append("Ambiguous method call '");
    sb.append(type.getName()).append(".").append(methodName);
    sb.append("': ");
    sb.append("definition candidates are: ");
    sb.append(candidateNames);
    return sb.toString();
  }
}
