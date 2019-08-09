package org.enso.interpreter.runtime.error;

import java.util.Arrays;
import java.util.stream.Collectors;
import org.enso.interpreter.runtime.callable.Callable;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;

/**
 * An error that is thrown when the interpreter is unable to match the arguments provided at the
 * call site to the arguments defined for the callable.
 */
public class ArgumentMappingException extends RuntimeException {

  /**
   * Creates a new error.
   *
   * @param callable the callable for which argument matching is taking place
   * @param argument information on the call-site argument that failed to match
   * @param callPosition the position at the call site of {@code argument}
   */
  public ArgumentMappingException(Callable callable, CallArgumentInfo argument, int callPosition) {
    super(
        String.format(
            "Cannot match argument %s to callable with arguments %s",
            argument.isPositional()
                ? "at position " + callPosition
                : "with name " + argument.getName(),
            Arrays.stream(callable.getArgs())
                .map(arg -> arg.getName() + ", ")
                .collect(Collectors.joining())));
  }
}
