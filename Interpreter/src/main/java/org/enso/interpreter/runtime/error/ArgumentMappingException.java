package org.enso.interpreter.runtime.error;

import org.enso.interpreter.runtime.callable.argument.ArgumentDefinition;
import org.enso.interpreter.runtime.callable.argument.CallArgumentInfo;

import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * An error that is thrown when the interpreter is unable to match the arguments provided at the
 * call site to the arguments defined for the callable.
 */
public class ArgumentMappingException extends RuntimeException {

  /**
   * Creates a new error.
   *
   * @param argumentDefinitions the definition site argument information
   * @param argument information on the call-site argument that failed to match
   * @param callPosition the position at the call site of {@code argument}
   */
  public ArgumentMappingException(
      ArgumentDefinition[] argumentDefinitions, CallArgumentInfo argument, int callPosition) {
    super(
        String.format(
            "Cannot match argument %s to callable with arguments %s",
            argument.isPositional()
                ? "at position " + callPosition
                : "with name " + argument.getName(),
            Arrays.stream(argumentDefinitions)
                .map(arg -> arg.getName() + ", ")
                .collect(Collectors.joining())));
  }
}
